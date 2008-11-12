package mroi.commands;

import mroi.MroiCanvas;
import ij.WindowManager;
import ij.gui.ImageCanvas;
import ij.gui.MessageDialog;
import mroi.AbstractState;
import mroi.MZipper;
import mroi.NoSuchCommandException;

public class ToggleVisible extends NonmutatingCommand {

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
