package mroi.commands;

import mroi.MroiCanvas;
import mroi.RoiContainer;
import ij.WindowManager;
import ij.gui.ImageCanvas;
import ij.gui.MessageDialog;
import mroi.AbstractState;
import mroi.MZipper;
import mroi.NoSuchCommandException;
import com.vividsolutions.jts.geom.Geometry;


public class ToggleVisible extends NonmutatingCommand<RoiContainer> {

	@Override
	public boolean isInvoked(String lbl) {
		return lbl.equalsIgnoreCase("togglevisible");
	}

	@Override
	public MZipper operation(MZipper mz) {
		ImageCanvas can = WindowManager.getCurrentWindow().getCanvas();
		if (can instanceof MroiCanvas) {
			AbstractState c = ((MroiCanvas)can).state;
			c.togglePreviousFrameVisible();
		}
		return mz;
	}

}
