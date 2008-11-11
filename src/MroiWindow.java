/*
    This file is part of Mroi, an ImageJ plugin to handle multiple regions
    of interest in image stacks.
    Copyright (C) 2007 Frederick Ross

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
import ij.ImagePlus;
import java.awt.event.AdjustmentListener;
import ij.gui.StackWindow;

import java.awt.event.AdjustmentEvent;

public class MroiWindow extends StackWindow implements AdjustmentListener {
	MroiCanvas can;
	ImagePlus imp;
	
	public MroiWindow(ImagePlus imp) {
		this(imp, new MroiCanvas(imp));
	}
	public MroiWindow(ImagePlus imp, MroiCanvas ic) {
		super(imp,ic);
		this.imp=imp;
		can = ic;
	}
	
	@Override
	public synchronized void adjustmentValueChanged(AdjustmentEvent e) {
		super.adjustmentValueChanged(e);
		can.con.goToFrameOn(null, imp);
	}
	
	@Override
	public synchronized void mouseWheelMoved(java.awt.event.MouseWheelEvent e) {
		super.mouseWheelMoved(e);
		can.con.goToFrameOn(null, imp);
	}
}
