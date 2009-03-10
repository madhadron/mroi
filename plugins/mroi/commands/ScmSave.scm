;(load "srfi-13.scm")



(define-namespace Map "class:java.util.Map")
(define-namespace List "class:java.util.List")
(define-namespace Set "class:java.util.Set")
(define-namespace JFC "class:javax.swing.JFileChooser")
(define-namespace WM "class:ij.WindowManager")
(define-namespace RoiC "class:mroi.RoiContainer")



;; I can never get to this class unless there is an open image with mroi running on it.  Therefore
;; WM:get-current-window is not null.

;; Either a user accepts a file name and the system writes, successfully or unsuccessfully, or the user aborts.

(define (get-file-name)
  (let* ((canvas (<ij.gui.ImageWindow>:get-canvas (WM:get-current-window)))
	 (fc (JFC))
	 (save? (eq? (JFC:.APPROVE_OPTION) (JFC:show-save-dialog fc canvas))))
    (if save?
	(*:get-canonical-path (JFC:get-selected-file fc))
	#f)))

(define (map->alist (m :: <java.util.Map>) key-f val-f)
  (letrec ((map-over-iterator (lambda (iter)  
				(let ((k (*:next iter)))
				  (cons
				   (pair (key-f k)
					 (val-f (Map:get m k)))
				   (if (*:has-next iter)
				       (map-over-iterator iter)
				       '())))))
	   (iter-on-m (Set:iterator (Map:key-set m))))
    (if (*:has-next iter-on-m)
	(map-over-iterator iter-on-m)
	'())))

(define (jlist->list jl)
  (letrec ((map-over-iterator (lambda (iter) (cons (*:next iter) 
						   (if (*:has-next iter)
						       (map-over-iterator iter)
						       '()))))
	   (iter-on-m (List:iterator jl)))
    (if (*:has-next iter-on-m)
	(map-over-iterator iter-on-m)
	'())))

(define (display-roi-container r)
  (list (RoiC:.id r) (*:to-string (RoiC:get-geometry r)) 
	(if (eq? #!null (RoiC:get-predecessor-id r)) 'null (RoiC:get-predecessor-id r))))


(define (has-current? z)
  (eq? (*:.class z) <mroi.JZipper>))

(define (zipper->triple z tail-f current-f null-current-value)
  (let ((lefts (tail-f (*:.lefts z)))
	(current (if (has-current? z) (current-f (<mroi.JZipper>:.current z)) null-current-value))
	(rights (tail-f (*:.rights z))))
    (list lefts current rights)))
(define (id x) x)

(define (roi-set->scheme z) 
  (let* ((convert-zipper (lambda (p) (zipper->triple p
						     (lambda (k) (map display-roi-container (jlist->list k)))
						     display-roi-container
						     'null))))
    (map->alist z id convert-zipper)))



(define-simple-class <mroi.commands.ScmSave> (<mroi.commands.RoiContainerCommand>)
	((isInvoked lbl) (<String>:equals-ignore-case lbl "scmsave"))
	((operation mz) #!null)
	((exec z frame) (let ((crois (*:.current z)))
			  (call-with-output-file "/Users/ross/Desktop/minny.mroi"
			    (lambda (p) (write (roi-set->scheme crois) p)))
			  z)))




;;(<ij.IJ>:show-message "Save!" (string-append "I tried to save to " (get-file-name (get-current-ij-canvas)))) z))





;;(define mroi-filename "/Users/ross/Desktop/2008-10-09-point10.mroi")
;; (define mroi-filename "/Users/ross/Desktop/junk.mroi")
;; (define g (call-with-input-file mroi-filename read))

;; (define (frame-list g) (map car g))


;; (*:set-property (<java.lang.System>:get-properties) "plugins.dir" "/Users/ross/data/reference/imagej/mroi/plugins")
;; (<ij.ImageJ>)

;; (define (get-current-canvas)
;;   (<ij.gui.ImageWindow>:get-canvas (WM:get-current-window)))

;; (define mycanvas (get-current-canvas))
;; (define mystate (*:.state mycanvas))
;; (define crois (*:.current (*:.rois mystate)))

;; (define (const p) (lambda (x) p))

;; (call-with-output-file "/Users/ross/Desktop/minny.mroi"
;;   (lambda (p) (write (roi-set->scheme crois) p)))


;;map->alist crois id (lambda (x) (zipper->triple x (const 'tail) (const 'just) 'nothing))) p)))