import static mroi.geometry.GeometryUtilities.gfact;

import ij.ImagePlus;

import java.awt.event.*;

import mroi.commands.Select;
import mroi.geometry.ContainmentPredicate;
import mroi.AbstractState;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

public class MouseController {
	AbstractState state;
	public MouseController(AbstractState st) {
		this.state = st;
	}
	

	public void mousePressedOnAt(ImagePlus imp, Integer x, Integer y,
			int button, int modifiers) {
		if (button == MouseEvent.BUTTON3 || button == MouseEvent.BUTTON2) {
			state.syncRoiFrom(imp);
			state.selectAt(x, y);
			state.syncRoiTo(imp);
		}
	}
}
