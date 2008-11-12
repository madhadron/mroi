package mroi;
import static mroi.geometry.GeometryUtilities.gfact;

import ij.ImagePlus;

import java.awt.event.*;

import mroi.commands.Select;
import mroi.geometry.ContainmentPredicate;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

public class MouseController {
	private RoiContainer selected;
	State state;
	public MouseController(State st) {
		this.state = st;
	}
	

	public void mousePressedOnAt(ImagePlus imp, Integer x, Integer y,
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
	
	public void mouseReleasedOnAt(ImagePlus imp, Integer x, Integer y,
			int button, int modifiers) {
		if (button == MouseEvent.BUTTON3 || button == MouseEvent.BUTTON2) {
			if (state.previousSliceVisible() && selected != null) {
				RoiContainer prev = state.chooseRoiAt(x, y, state.previousNonemptyFrame());
				selected.setPredecessor(prev);
				imp.updateAndRepaintWindow();
			}
		}
	}
}
