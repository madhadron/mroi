package mroi;
import static mroi.geometry.GeometryUtilities.gfact;
import ij.IJ;
import ij.ImagePlus;
import ij.process.ImageProcessor;

import java.awt.event.*;

import mroi.commands.Select;
import mroi.geometry.ContainmentPredicate;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import gnu.math.IntNum;

public class MouseController {
	private RoiContainer selected;
	State state;
	public MouseController(State st) {
		this.state = st;
	}
	
	private int clickedx, clickedy;
	
	public void mousePressedOnAt(ImagePlus imp, int x, int y,
			int button, int modifiers) {
		if (button == MouseEvent.BUTTON3 || button == MouseEvent.BUTTON2) {
			if ((modifiers & MouseEvent.CTRL_DOWN_MASK) == MouseEvent.CTRL_DOWN_MASK) {
				clickedx = x;
				clickedy = y;
			} else if (state.previousSliceVisible()) {
				selected = state.chooseRoiAt(x, y, state.getCurrentSlice());
			} else {
				state.syncRoiFrom(imp);
				state.selectAt(x, y);
				state.syncRoiTo(imp);
			}
		}
	}
	
	public void mouseReleasedOnAt(ImagePlus imp, int x, int y,
			int button, int modifiers) {
		if (button == MouseEvent.BUTTON3 || button == MouseEvent.BUTTON2) {
			if ((modifiers & MouseEvent.CTRL_DOWN_MASK) == MouseEvent.CTRL_DOWN_MASK) {
				ImageProcessor ip = imp.getProcessor();
				int label1 = ip.getPixel(clickedx, clickedy);
				int label2 = ip.getPixel(x, y);
				if (label1 != 0 && label2 != 0) {
				//IJ.showMessage("Changing " + label2 + " selected at (" + x +"," + y
	//						+ ") to " + label1 + " selected at (" + clickedx + ","
							//+ clickedy +")");
					for (int i = 0; i < ip.getWidth()*ip.getHeight(); i++) {
						if (ip.get(i) == label2)
							ip.set(i, label1);
					}
					imp.updateAndRepaintWindow();
				}
			} else if (state.previousSliceVisible() && selected != null) {
				RoiContainer prev = state.chooseRoiAt(x, y, state.previousNonemptyFrame());
				selected.setPredecessor(prev);
				imp.updateAndRepaintWindow();
			}
		}
	}
	
	public void mousePressedOnAt(ImagePlus imp, IntNum x, IntNum y, IntNum button, IntNum modifiers) {
		mousePressedOnAt(imp,x.intValue(),y.intValue(),button.intValue(),modifiers.intValue());
	}
	public void mouseReleasedOnAt(ImagePlus imp, IntNum x,
				IntNum y, IntNum button, IntNum modifiers) {
		mouseReleasedOnAt(imp, x.intValue(), y.intValue(),
					button.intValue(), modifiers.intValue());
		
	}
}
