;;(require <mroi.commands.CommonIO>)

;; I can never get to this class unless there is an open image with mroi running on it.  Therefore
;; WM:get-current-window is not null.

(define-simple-class <mroi.commands.ScmSave> (<mroi.commands.RoiContainerCommand>)
	((isInvoked lbl) (<String>:equals-ignore-case lbl "scmsave"))
	((operation mz) #!null)
 	((exec z frame) (save z) z))

(define (save z)
  (let ((crois (<mroi.Zipper>:.current z)))
    (receive (go? file-name) (get-file-name 'save)
	     (if go?
		 (write-to-file file-name crois)
		 z))))

(define-namespace Map "class:java.util.HashMap")
(define-namespace List "class:java.util.List")
(define-namespace Set "class:java.util.Set")
(define-namespace Geom "class:com.vividsolutions.jts.geom.Geometry")
(define-namespace JFC "class:javax.swing.JFileChooser")
(define-namespace WM "class:ij.WindowManager")
(define-namespace RoiC "class:mroi.RoiContainer")


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

(define (id x) x)

(define (map->alist (m :: <java.util.Map>) key-f val-f)
  (letrec ((map-over-iterator (lambda (iter)  
				(let ((k (<java.util.Iterator>:next iter)))
				  (cons
				   (pair (key-f k)
					 (list (val-f (Map:get m k))))
				   (if (<java.util.Iterator>:has-next iter)
				       (map-over-iterator iter)
				       '())))))
	   (iter-on-m (Set:iterator (Map:key-set m))))
    (if (<java.util.Iterator>:has-next iter-on-m)
	(map-over-iterator iter-on-m)
	'())))

(define (alist->map a key-f val-f)
  (letrec ((m (make <java.util.HashMap>))
	   (g (lambda (as) (if (not (null? as))
			       (begin (Map:put m (key-f (caar as)) (val-f (cdar as)))
				      (g (cdr as)))))))
    (g a)
    m))

;; (alist->map '((3 . 5) (2 . 6)) id id)

(define (jlist->list jl)
  (letrec ((map-over-iterator (lambda (iter) (cons (<java.util.ListIterator>:next iter) 
						   (if (<java.util.ListIterator>:has-next iter)
						       (map-over-iterator iter)
						       '()))))
	   (iter-on-m (List:iterator jl)))
    (if (<java.util.ListIterator>:has-next iter-on-m)
	(map-over-iterator iter-on-m)
	'())))

(define (list->jlist l)
  (letrec ((jl (<java.util.ArrayList>))
	   (g (lambda (as) (if (not (null? as))
			       (begin (List:add jl (car as))
				      (g (cdr as)))))))
    (g l)
    jl))

(define (display-roi-container r)
  (list (RoiC:.id r) (Geom:to-string (RoiC:get-geometry r))
	(if (eq? #!null (RoiC:get-predecessor-id r)) 'null (RoiC:get-predecessor-id r))))

(define (wkt->geometry wkt) (let ((wkt-reader (<com.vividsolutions.jts.io.WKTReader> (<mroi.geometry.GeometryUtilities>:.gfact))))
			      (<com.vividsolutions.jts.io.WKTReader>:read wkt-reader (as <String> wkt))))

(define (read-roi-container rs)
  (RoiC (as <integer> (car rs)) (wkt->geometry (cadr rs)) #!null))

;;(write (display-roi-container (read-roi-container '(12 "POLYGON((1 1, 2 2, 3 3, 1 1))" 25))))

(define (has-current? z)
  (eq? (<java.lang.Object>:.class z) <mroi.JZipper>))

(define (is-filled? l)
  (not (eq? (cadr l) 'null)))

;;(is-filled? '(() boris ()))
;;(is-filled? '(() null ()))

(define (zipper->triple z tail-f current-f null-current-value)
  (let ((lefts (tail-f (jlist->list (*:.lefts z))))
	(current (if (has-current? z) (current-f (<mroi.JZipper>:.current z)) null-current-value))
	(rights (tail-f (jlist->list (*:.rights z)))))
    (list lefts current rights)))

(define (triple->zipper tr tail-f current-f)
  (let ((lefts (list->jlist (tail-f (car tr))))
	(rights (list->jlist (tail-f (caddr tr)))))
    (if (is-filled? tr)
	(<mroi.JZipper> lefts (current-f (cadr tr)) rights)
	(<mroi.NZipper> lefts rights))))

;;(triple->zipper '((1 2 3) 5 (6 7 8)) id id)

(define (roi-set->scheme z) 
  (let* ((convert-zipper (lambda (p) (zipper->triple p
						     (lambda (k) (map display-roi-container k))
						     display-roi-container
						     'null))))
    (map->alist z id convert-zipper)))

(define (scheme->string s)
  (letrec ((p (open-output-string))
	   (f (lambda (ss) (if (null? ss) '()
			       (begin (display "(" p)
				      (write (caar ss) p)
				      (display " . " p)
				      (write (cadar ss) p)
				      (display ")" p)
				      (newline p)
				      (f (cdr ss)))))))
    (display "(" p)
    (f s)
    (display ")" p)
    (get-output-string p)))


(define (scheme->roi-set sch)
  (let* ((convert-triple (lambda (p) (triple->zipper p
						     (lambda (k) (map read-roi-container k))
						     read-roi-container))))
    (alist->map sch id convert-triple)))

(define (scheme-to-file file-name sch)
  (call-with-output-file file-name
    (lambda (p) (display (scheme->string sch) p))))

(define (read-from-file file-name)
  (call-with-input-file file-name
    (lambda (p) (scheme->roi-set (read p)))))

(define (write-to-file file-name s)
  (scheme-to-file file-name (roi-set->scheme s)))

;;(define myread (read-from-file "/Users/ross/Desktop/boris.mroi"))

;;(define inf 
;;  (call-with-input-file "/Users/ross/Desktop/2008-10-09-point10.mroi"
;;    (lambda (p) (scheme->roi-set (read p)))))
;;(call-with-input-file "/Users/ross/Desktop/minny.mroi"
;;  (lambda (p) (scheme->roi-set (read p))))

;;(write-to-file "/Users/ross/Desktop/boris2.mroi" myread)

;;(define myread2 (read-from-file "/Users/ross/Desktop/boris2.mroi"))
;;(write-to-file "/Users/ross/Desktop/boris3.mroi" myread2)

;;(eq? myread myread2)


















;; ;; Some code to exercise things.

;; ;;(define mroi-filename "/Users/ross/Desktop/2008-10-09-point10.mroi")
;; ;; (define mroi-filename "/Users/ross/Desktop/junk.mroi")
;; ;; (define g (call-with-input-file mroi-filename read))

;; ;; (define (frame-list g) (map car g))

;; (*:set-property (<java.lang.System>:get-properties) "plugins.dir" "/Users/ross/data/reference/imagej/mroi/plugins")
;; (<ij.ImageJ>)

;; (define (get-current-canvas)
;;    (<ij.gui.ImageWindow>:get-canvas (WM:get-current-window)))

;; (define mycanvas (get-current-canvas))
;; (define mystate (*:.state mycanvas))
;; (define crois (*:.current (*:.rois mystate)))

;; (define (const p) (lambda (x) p))

;; (call-with-output-file "/Users/ross/Desktop/minny.mroi"
;;   (lambda (p) (write (roi-set->scheme crois) p)))


;;map->alist crois id (lambda (x) (zipper->triple x (const 'tail) (const 'just) 'nothing))) p)))

;;(set! (<mroi.State>:.rois (as <mroi.State> mystate)) (<mroi.MZipper>:insert-and-step (<mroi.State>:.rois mystate) g))