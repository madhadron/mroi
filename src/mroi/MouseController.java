package mroi;
import static mroi.geometry.GeometryUtilities.gfact;
import ij.IJ;
import ij.ImagePlus;

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
	

	public void mousePressedOnAt(ImagePlus imp, int x, int y,
			int button, int modifiers) {
		if (button == MouseEvent.BUTTON3 || button == MouseEvent.BUTTON2) {
			if (state.previousSliceVisible()) {
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
			if (state.previousSliceVisible() && selected != null) {
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
