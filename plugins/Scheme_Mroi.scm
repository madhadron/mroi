(define-namespace ij "class:ij.IJ")

(define (make-mroi-window current-imp current-canvas)
  (object (<ij.gui.StackWindow> 
	   <java.awt.event.AdjustmentListener>)
	  ((*init*)
	   (invoke-special <ij.gui.StackWindow> (this) 
			   '*init* current-imp current-canvas))
	  ((frame-changed)
	   (let* ((canvas (*:getCanvas (this)))
		  (imp (*:getImagePlus (this)))
		  (state (<mroi.MroiCanvas>:.state canvas)))
	     (*:goToFrameOn state imp)))
	  ((adjustmentValueChanged adjustment-event)
	   (invoke-special <ij.gui.StackWindow> (this) 'adjustmentValueChanged adjustment-event)
	   (frame-changed))
	  ((mouseWheelMoved mouse-wheel-event)
	   (invoke-special <ij.gui.StackWindow> (this) 'mouseWheelMoved mouse-wheel-event)
	   (frame-changed))))
  

(let* ((current-imp (<ij.IJ>:getImage))
       (mroi-canvas (<mroi.MroiCanvas> current-imp)))
  (make-mroi-window current-imp mroi-canvas))


