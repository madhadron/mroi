package mroi.commands;

import ij.WindowManager;
import ij.gui.GenericDialog;

import java.util.Map;

import com.vividsolutions.jts.geom.Point;
import mroi.geometry.*;
import mroi.*;

public class GoToID implements Command<RoiContainer> {
	public boolean isInvoked(String lbl) {
		return lbl.equalsIgnoreCase("gotoid");
	}
	public MZipper<RoiContainer> operation(MZipper<RoiContainer> mz) { return mz; }

	public Zipper<Map<Integer,MZipper<RoiContainer>>> exec(Zipper<Map<Integer,MZipper<RoiContainer>>> z, int frame) {
		MroiCanvas canvas = ((MroiCanvas)WindowManager.getCurrentWindow().getCanvas()); 
		State state = canvas.state;
		GenericDialog gd = new GenericDialog("ID to select");
		gd.addNumericField("ID: ", 0, 0);
		gd.showDialog();
		if (gd.wasCanceled())
			return z;
		int id = (int) gd.getNextNumber();
		Map<Integer,MZipper<RoiContainer>> m = z.current;
		MZipper<RoiContainer> mz;
		for (int fr : m.keySet()) {
			for (RoiContainer r : m.get(fr).asList()) {
				if (r.id == id) {
					state.goToFrameOn(fr, canvas.imp);
					Point centroid = r.getGeometry().getCentroid();
					mz = m.get(fr).select(new IsIdPredicate(id));
					m.put(fr, mz);
				}
			}
		}
		return z;
	}

}
