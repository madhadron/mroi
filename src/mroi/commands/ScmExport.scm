;;(require <mroi.commands.CommonIO>)

;; I can never get to this class unless there is an open image with mroi running on it.  Therefore
;; WM:get-current-window is not null.

(require 'srfi-1)
(require 'srfi-95)

(define-namespace Map "class:java.util.HashMap")
(define-namespace List "class:java.util.List")
(define-namespace Set "class:java.util.Set")
(define-namespace Geom "class:com.vividsolutions.jts.geom.Geometry")
(define-namespace JFC "class:javax.swing.JFileChooser")
(define-namespace WM "class:ij.WindowManager")
(define-namespace RoiC "class:mroi.RoiContainer")

(define-simple-class <mroi.commands.ScmExport> (<mroi.commands.RoiContainerCommand>)
	((isInvoked lbl) (<String>:equals-ignore-case lbl "scmexport"))
	((operation mz) mz)
 	((exec z frame) (export z) z))

(define (export z)
  (let ((crois (<mroi.Zipper>:.current z))) ; crois has type Map<Integer,MZipper<RoiContainer>>
    (receive (go? file-name) (get-file-name 'save)
	     (if go?
		 (scheme-to-file file-name (merge-frames (java-mrois->scheme crois)))
		 z))))

(define (scheme-to-file file-name sch)
  (call-with-output-file file-name
    (lambda (p) (format p "~S" sch))))

;; 'save | 'load -> string()
;; Opens a Swing file save/load dialog (according to the argument to the function) and blocks until the user makes a selection, when it returns the resulting filename
(define (get-file-name type)
  (let* ((canvas (<ij.gui.ImageWindow>:get-canvas (WM:get-current-window)))
	 (fc (JFC))
	 (go? (eq? (JFC:.APPROVE_OPTION) (case type
					   ('save (JFC:show-save-dialog fc canvas))
					   ('load (JFC:show-open-dialog fc canvas))
					   (else  (JFC:.CANCEL_OPTION))))))
    (if go?
	(values #t (<java.io.File>:get-canonical-path (JFC:get-selected-file fc)))
	(values #f ""))))

;; Converting Java structure to Scheme:

(define (java-int->int x)
  (<gnu.math.IntNum> (<java.lang.Integer>:intValue x)))

(define (java-mrois->scheme rois)
  (bimap java-int->int (compose (curry-map roi-container->list) zipper->list) (map->alist rois)))

;; key -> x, value -> y, association-list -> association-list
;; Maps f over the keys and g over the values of the association list
(define (bimap f g al)
  (fold-right (lambda (entry ys)
		(let ((new-entry (cons (f (car entry)) (g (cdr entry)))))
		  (cons new-entry ys))) '() al))

;; Map<X,Y> -> association-list
(define (map->alist (m :: <java.util.Map>))
  (letrec ((map-over-iterator (lambda (iter)  
				(let ((k (<java.util.Iterator>:next iter)))
				  (cons
				   (cons k (Map:get m k))
				   (if (<java.util.Iterator>:has-next iter)
				       (map-over-iterator iter)
				       '())))))
	   (iter-on-m (Set:iterator (Map:key-set m))))
    (if (<java.util.Iterator>:has-next iter-on-m)
	(map-over-iterator iter-on-m)
	'())))

(define (roi-container->list r)
  (list (java-int->int (RoiC:.id r))
	(if (eq? #!null (RoiC:get-predecessor-id r)) 
	    'null
	    (java-int->int (RoiC:get-predecessor-id r)))
	(list (pair 'polygon (Geom:to-string (RoiC:get-geometry r))))))
	


(define (id x) x)

(define (compose f g) (lambda (x) (f (g x))))

(define (curry-map f)
  (lambda (x) (map f x)))

;; Actual merging code (pure Scheme from here)
(define-record-type cell-seq
  (cell e)
  cell?
  (e entries))

(define empty-cell (cell '()))

; term, cell -> cell
; Prepends term to the association list entries in cell
(define (cons-cell p c)
  (cell (cons p (entries c))))

(define-record-type poly
  (polygon t polyid polydata)
  polygon?
  (t polygon-time)
  (polyid polygon-id)
  (polydata polygon-data))

(define-record-type multi-tree
  (tree v ch)
  tree?
  (v tree-current)
  (ch tree-children))

(define (prepend-polygon-to-root poly tr)
  (tree (cons-cell poly (tree-current tr)) (tree-children tr)))

(define (leaf x) (tree x '()))




;; alist -> list
;; Returns a list of all the values, still in order, in an association list.
(define (alist-values al)
  (fold-right (lambda (next so-far) (cons (cdr next) so-far)) '() al))

(define (key-< x y)
  (< (car x) (car y)))

(define (sort-frames frames)
  (bimap id (lambda (x) (sort x key-<)) (sort frames key-<)))

;; Map<Integer,MZipper<RoiContainer>> -> [tree]
(define (merge-frames frames)
  (let* ((key-< (lambda (x y) (< (car x) (car y))))
	 (sorted-frames (bimap id (lambda (x) (sort x key-<)) (sort frames key-<))))
    (map cdr (fold-right merge-frames1 '() sorted-frames))))

; time, (id pred data) -> (pred . polygon)
(define (reshape tm rec)
  (pair (polygon tm (first rec) (third rec)) (second rec)))

(define (reshape-frame q)
  (map (lambda (x) (reshape (car q) x)) (cdr q)))

;; (frame . list of regions), ((pred . tree)) -> ((pred . tree))
(define (merge-frames1 roi-list trees)
  (let* ((time (car roi-list))
	 (entries (map (lambda (x) (reshape time x)) (cdr roi-list))))
         ; Each entry has the form '(time id pred data)
         ; Each tree has the form '(pred value . children)
    (bind (amerge entries trees symbolic-<))))

; al1 has the form '( (poly . pred) ... )
; al2 has the form '( (pred cellseq[poly] . children) )
; output has the form '( (polyid (poly . pred) . ( (cellseq[poly] . children) ... ) ) )
(define (amerge ual1 ual2 sort-rule) 
  (let ((al1 (sort ual1 (lambda (x y) (< (polygon-id (car x)) (polygon-id (car y))))))
	(al2 (sort ual2 key-<)))
    (cond ((eq? al1 '()) (map (lambda (q) (list (car q) '() (cdr q))) al2))
	  ((eq? al2 '()) (map (lambda (q) (list (polygon-id (car q)) q)) al1))
	  (#t (let* ((entry-id (polygon-id (caar al1)))
		     (tree-pred (caar al2))
		     (same-pred (lambda (x) (and (number? (car x)) (= (car x) tree-pred)))))
		(case (sort-rule entry-id tree-pred)
		  ((eq)
		   (let-values (((same-key-entries other-entries) (split-with same-pred al2)))
		     (cons (append (list entry-id (first al1)) (map cdr same-key-entries))
			   (amerge (cdr al1) other-entries sort-rule))))
		  ((lt)
		   (cons (list entry-id (first al1)) 
			 (amerge (cdr al1) al2 sort-rule)))
		  ((gt)
		   (cons (list tree-pred '() (cdar al2))
			 (amerge al1 (cdr al2) sort-rule)))))))))


(define (symbolic-< x y)
  (cond ((not (number? y)) 'lt)
        ((not (number? x)) 'gt)
        ((< x y) 'lt)
        ((> x y) 'gt)
        ((= x y) 'eq)))

(define (split-with pred l)
  (cond ((eq? l '()) (values '() '()))
        ((pred (first l)) (let-values (((a b) (split-with pred (cdr l))))
                            (values (cons (first l) a) b)))
        (#t (values '() l))))

(define (bind l) (map bind1 l))

;; Conditions: (key left-val=(id time polygon predecessor) right1 right2 ...).
;; If key is not a number, then left-val is '()
;; If left-val is '(), then there is only right1, nothing further
;; The rights are all trees
;; Input: entry ~ (id (poly . pred) . trees)
;; Output: ( (pred . tree) )
(define (bind1 entry)
  (let ((key (first entry))
        (left-value (second entry))
        (right-values (cddr entry)))
  (cond ((or (not (number? key)) (eq? '() left-value)) 
	 (pair key (car right-values)))
        ((equal? right-values '())
	 (pair (cdr left-value) (tree (cons-cell (car left-value) empty-cell) '())))
        ((= (length right-values) 1)
	 (pair (cdr left-value) (prepend-polygon-to-root (car left-value) (car right-values))))
        ((> (length right-values) 1)
	 (pair (cdr left-value) (tree (cons-cell (car left-value) empty-cell) right-values))))))




;; List<X> -> list
(define (jlist->list jl)
  (letrec ((map-over-iterator (lambda (iter) (cons (<java.util.ListIterator>:next iter) 
						   (if (<java.util.ListIterator>:has-next iter)
						       (map-over-iterator iter)
						       '()))))
	   (iter-on-m (List:iterator jl)))
    (if (<java.util.ListIterator>:has-next iter-on-m)
	(map-over-iterator iter-on-m)
	'())))

;; list -> List<X>
(define (list->jlist l)
  (letrec ((jl (<java.util.ArrayList>))
	   (g (lambda (as) (if (not (null? as))
			       (begin (List:add jl (car as))
				      (g (cdr as)))))))
    (g l)
    jl))

;; (define (wkt->geometry wkt) (let ((wkt-reader (<com.vividsolutions.jts.io.WKTReader> (<mroi.geometry.GeometryUtilities>:.gfact))))
;; 			      (<com.vividsolutions.jts.io.WKTReader>:read wkt-reader (as <String> wkt))))

;; (define (read-roi-container rs)
;;   (RoiC (as <integer> (car rs)) (wkt->geometry (cadr rs)) #!null))

;;(write (display-roi-container (read-roi-container '(12 "POLYGON((1 1, 2 2, 3 3, 1 1))" 25))))

(define (has-current? z)
  (eq? (<java.lang.Object>:.class z) <mroi.JZipper>))

(define (is-filled? l)
  (not (eq? (cadr l) 'null)))

;;(is-filled? '(() boris ()))
;;(is-filled? '(() null ()))

(define (zipper->triple z)
  (let ((lefts (jlist->list (*:.lefts z)))
	(current (if (has-current? z) (<mroi.JZipper>:.current z) '()))
	(rights (jlist->list (*:.rights z))))
    (list lefts current rights)))

(define (triple->zipper tr)
  (let ((lefts (list->jlist (car tr)))
	(rights (list->jlist (caddr tr))))
    (if (is-filled? tr)
	(<mroi.JZipper> lefts (second tr) rights)
	(<mroi.NZipper> lefts rights))))

;; MZipper<X> -> [X]
(define (zipper->list z)
  (let* ((triple (zipper->triple z))
	 (lefts (first triple))
	 (middles (if (eq? (second triple) '()) '() (list (second triple))))
	 (rights (third triple)))
    (append lefts middles rights)))



