(define (safe-car p) (if (null? p) p (car p)))
(define (safe-cdr p) (if (null? p) p (cdr p)))
(define (list->zipper l) (list '() (safe-car l) (safe-cdr l)))
(define (zipper->list z) (append (car z) (cons (cadr z) (caddr z))))

(define-macro zipper-action
  (lambda (name filled-form null-form)
    `(define (,name z)
       (let ((l (car z))
	     (lh (caar z))
	     (ls (safe-cdr (car z)))
	     (c (cadr z))
	     (r (caddr z))
	     (rh (caaddr z))
	     (rs (safe-cdr (caddr z))))
	 (if (null? c) ,null-form ,filled-form)))))

(define-macro zipper-action-1
  (lambda (name filled-form null-form)
    `(define (,name z a)
       (let ((l (car z))
	     (lh (caar z))
	     (ls (safe-cdr (car z)))
	     (c (cadr z))
	     (r (caddr z))
	     (rh (caaddr z))
	     (rs (safe-cdr (caddr z))))
	 (if (null? c) ,null-form ,filled-form)))))

(zipper-action zipper-left  (list ls lh (cons c r)) (list ls lh r))
(zipper-action zipper-right (list (cons c l) rh rs) (list l rh rs))
(zipper-action zipper-clear (list (cons c l) () r)  (list l c r))
(zipper-action-1 zipper-add (list (cons c l) a r)   (list l a r))
(zipper-action zipper-delete (list l () r)          (list l () r))
(zipper-action-1 zipper-push (list (cons c l) a ()) (list l a ()))
(zipper-action zipper-current c ())
(zipper-action zipper? (and (list? l) (list? r)) (and (list? l) (list? r)))

(define (range a b k)
  (if (<= (+ a k) b)
      (cons a (range (+ a k) b k))
      (list a)))

(define (empty-map n fill-proc)
  (map (lambda (fr) (cons fr (fill-proc))) (range 1 n 1)))

(define-simple-class <KawaMroiController> (<MroiAbstractController>)
  (rois)
  (currentSlice type: <java.lang.Integer>)

  ((*init* (numberOfSlices :: <java.lang.Integer>))

           (set! rois (list->zipper 
		       (empty-map numberOfSlices 
				  (lambda () (list->zipper '()))))))

  ((<goToFrame> (slice :: <java.lang.Integer>) 
		(imp :: <ij.ImagePlus>)) <void>

		(syncRoiFrom imp)
		(if (not (null? slice))
		    (*:setSlice imp slice))
		(set! currentSlice (*:getCurrentSlice imp))
		(*:updateAndRepaintWindow imp)
		(syncRoiTo imp))

  ((paint (g :: <java.awt.Graphics>) 
	  (visibleWindow :: <java.awt.Rectangle>)
	  (magnification :: <java.lang.Double>)) <void>

	  )

  ((mousePressedOnAt (imp :: <ij.ImagePlus>)
		     (x :: <java.lang.Integer>)
		     (y :: <java.lang.Integer>)
		     (button :: <int>)
		     (modifiers :: <int>)) <void>

		     )

  ((executeCommandOn (lbl :: <java.lang.String>)
		     (imp :: <ij.ImagePlus>)) <void>

		     ))