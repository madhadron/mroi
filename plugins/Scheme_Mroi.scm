(define-namespace ij "class:ij.IJ")

(define (make-mroi-window current-imp current-canvas)
  (object (<ij.gui.StackWindow> 
	   <java.awt.event.AdjustmentListener>)
	  (imp :: <ij.ImagePlus>)
	  (canvas)
	  (state :: <mroi.State>)
	  ((*init*)
	   (invoke-special <ij.gui.StackWindow> (this) 
			   '*init* current-imp current-canvas)
	   (set! imp current-imp)
	   (set! canvas current-canvas)
	   (set! state (*:getState canvas)))
	  ((frame-changed)
	   (<mroi.State>:goToFrameOn state imp))
	  ((adjustmentValueChanged adjustment-event)
	   (invoke-special <ij.gui.StackWindow> (this) 'adjustmentValueChanged adjustment-event)
	   (frame-changed))
	  ((mouseWheelMoved mouse-wheel-event)
	   (invoke-special <ij.gui.StackWindow> (this) 'mouseWheelMoved mouse-wheel-event)
	   (frame-changed))))

	   

(define (make-mroi-canvas current-imp)
  (object (<ij.gui.ImageCanvas>
	   <java.awt.event.MouseListener>)
	  (state :: <mroi.State> access: 'public)
	  ((getState) state)
	  (mouse-cont :: <mroi.MouseController>)
	  (imp)

	  ((*init*)
	   (invoke-special <ij.gui.ImageCanvas> (this)
			   '*init* current-imp)
	   (set! state (<mroi.State> (<ij.ImagePlus>:getNSlices current-imp)))
	   (set! mouse-cont (<mroi.MouseController> state))
	   (set! imp current-imp))

	  ((paint g)
	   (invoke-special <ij.gui.ImageCanvas> (this)
			   'paint g)
	   (invoke (as <mroi.State> state) 'paint g (<ij.gui.ImageCanvas>:getSrcRect (this)) (*:getMagnification (this))))

	  ((outside-source-rectangle? x y rect)
	   (let ((rectx (*:.x rect))
		 (recty (*:.y rect))
		 (rectwidth (*:.width rect))
		 (rectheight (*:.height rect)))
	     (or (< x rectx)
		 (> x (+ rectx rectwidth))
		 (< y recty)
		 (> y (+ recty rectheight)))))

	  ((button->integer mouse-event)
	   (let ((b (<java.awt.event.MouseEvent>:getButton mouse-event)))
	     (cond ((= (<java.awt.event.MouseEvent>:.BUTTON1) b) 1)
		   ((= (<java.awt.event.MouseEvent>:.BUTTON2) b) 2)
		   ((= (<java.awt.event.MouseEvent>:.BUTTON3) b) 3))))

	  ((mousePressed mouse-event)
 	   (let ((x (<ij.gui.ImageCanvas>:offScreenX (this) (*:getX mouse-event)))
 		 (y (<ij.gui.ImageCanvas>:offScreenY (this) (*:getY mouse-event)))
 		 (rect (<ij.gui.ImageCanvas>:getSrcRect (this))))
	     (cond ((outside-source-rectangle? x y rect)
		    (invoke-special <ij.gui.ImageCanvas> (this) 
				    'mousePressed mouse-event))
		   ((eq? 1 (button->integer mouse-event))
		    (let ((roi (<ij.ImagePlus>:getRoi imp)))
		      (invoke-special <ij.gui.ImageCanvas> 
				      (this) 'mousePressed mouse-event)
		      (if (not (eq? #!null roi)) (<ij.ImagePlus>:setRoi imp roi))))
		   (else 
		    (<mroi.MouseController>:mousePressedOnAt 
		     mouse-cont
		     imp x y (<java.awt.event.MouseEvent>:getButton mouse-event)
		     (<java.awt.event.MouseEvent>:getModifiers mouse-event))))))

	  ((mouseReleased mouse-event)
 	   (let ((x (<ij.gui.ImageCanvas>:offScreenX (this) (*:getX mouse-event)))
 		 (y (<ij.gui.ImageCanvas>:offScreenY (this) (*:getY mouse-event)))
 		 (rect (<ij.gui.ImageCanvas>:getSrcRect (this))))
	     (cond ((outside-source-rectangle? x y rect)
		    (invoke-special <ij.gui.ImageCanvas> (this) 'mouseReleased mouse-event))
		   ((eq? 1 (button->integer mouse-event))
		    (invoke-special <ij.gui.ImageCanvas> 
				    (this) 'mouseReleased mouse-event))
		   (else 

		    (<mroi.MouseController>:mouseReleasedOnAt 
		     mouse-cont
		     imp x y (<java.awt.event.MouseEvent>:getButton mouse-event)
		     (<java.awt.event.MouseEvent>:getModifiers mouse-event))))))))

;; 		    (<ij.IJ>:showMessage 
;; 			  (string-append "Pressed mouse at " 
;; 					 (number->string x) ", " 
;; 					 (number->string y) " "
;; 					 (if (outside-source-rectangle? x y rect)
;; 					     "outside" "inside")
;; 					 "button " (number->string (button->integer mouse-event))
;; 					 "roi is " (if (roi-is-null? imp) "null" "not null")))

;;  	     (<ij.IJ>:showMessage 
;;  	      (string-append "Pressed mouse at " 
;;  			     (number->string x) ", " 
;;  			     (number->string y) " "
;;  			     (if (outside-source-rectangle? x y rect)
;; 				 "outside" "inside")
;; 			     "button " (symbol->string (button->symbol mouse-event))))))))


;;	  (else (invoke-special <ij.gui.ImageCanvas> (this)
;;					 'mousePressed mouse-event)))))))
;; 		   ((eq? 1 (button->integer mouse-event))
;; 		    (execute-and-replace-roi 'mousePressed mouse-event))
;; 		   (else (<mroi.MouseController>:mousePressedOnAt 
;; 			  mouse-controller
;; 			  imp x y (button->integer mouse-event)
;; 			  (modifier->integer mouse-event))))))))

;; 	     (if (outside-source-rectangle? x y rect)
;;  		 (begin (invoke-special <ij.gui.ImageCanvas> (this)
;;  					'mousePressed mouse-event)
;;  			(<ij.IJ>:showMessage "Outside region -- passing on."))
;;  		 (if (= (MouseEvent:.BUTTON1) (*:getButton mouse-event))
;;  		     (let ((roi (<ij.ImagePlus>:getRoi imp)))
;;  		       (invoke-special <ij.gui.ImageCanvas> (this)
;;  				       'mousePressed mouse-event)
;;  		       (<ij.IJ>:showMessage "Passing mouse down button 1 to superclass")
;;  		       (if (not (null? roi))
;;  			   (<ij.ImagePlus>:setRoi imp roi))
;;  		       (<ij.ImagePlus>:updateAndRepaintWindow imp))
;;  		     (*:mousePressedOnAt mouse-controller imp x y (*:getButton mouse-event) (*:getModifiers mouse-event))))))))
;; 	  ((mousePressed mouse-event)
;; 	   (let ((x (<ij.gui.ImageCanvas>:offScreenX (this) (*:getX mouse-event)))
;; 		 (y (*:offScreenY (this) (*:getY mouse-event)))
;; 		 (rect (*:getSrcRect (this))))
;; 	     (if (outside-source-rectangle? x y rect)
;; 		 (invoke-special <ij.gui.ImageCanvas> (this)
;; 				 'mouseReleased mouse-event)
;; 		 (if (= (MouseEvent:.BUTTON1) (*:getButton mouse-event))
;; 		     (let ((roi (<ij.ImagePlus>:getRoi imp)))
;; 		       (invoke-special <ij.gui.ImageCanvas> (this)
;; 				       'mouseReleased mouse-event)
;; 		       (if (not (null? roi))
;; 			   (<ij.ImagePlus>:setRoi imp roi))
;; 		       (<ij.ImagePlus>:updateAndRepaintWindow imp))
;; 		     (*:mouseReleasedOnAt mouse-controller imp x y (*:getButton mouse-event) (*:getModifiers mouse-event))))))))

(let* ((current-imp (<ij.IJ>:getImage))
       (mroi-canvas (make-mroi-canvas current-imp)))
;;       (mroi-canvas (<mroi.MroiCanvas> current-imp)))
  (make-mroi-window current-imp mroi-canvas))


