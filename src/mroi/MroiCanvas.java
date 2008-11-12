package mroi;
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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.LinkedList;

import ij.gui.ImageCanvas;
import ij.gui.Roi;
import ij.ImagePlus;
import mroi.*;
import mroi.geometry.*;
import static mroi.geometry.GeometryUtilities.*;
import mroi.commands.*;

import com.vividsolutions.jts.geom.*;

import java.util.*;
import java.awt.event.*;

public class MroiCanvas extends ImageCanvas {
	public AbstractState state;
	public MouseController mc;
	public ImagePlus imp;

	public MroiCanvas(ImagePlus im) {
		super(im);
		imp = im;
		state = new State(imp.getNSlices());
		mc = new MouseController(state);
	}

	public void paint(Graphics g) {
		super.paint(g);
		state.paint(g, getSrcRect(), getMagnification());
	}

	public void mousePressed(MouseEvent e) {
		Integer x = offScreenX(e.getX());
		Integer y = offScreenY(e.getY());
		Rectangle rect = getSrcRect();
		if (x < rect.x || x > rect.x + rect.width || y < rect.y
				|| y > rect.y + rect.width) {
			// If the mouse is outside the actual image, 
			// just pass the click to the superclass.
			super.mousePressed(e);
		} else if (e.getButton() == MouseEvent.BUTTON1) {
			// This is necessary to make ImageJ not drop
			// the current ROI when we click outside of it
			// with the left button.
			Roi current = imp.getRoi();
			super.mousePressed(e);
			if (current != null) {
				imp.setRoi(current);
			}
			imp.updateAndRepaintWindow();
		} else {
			// If the right mousebutton (BUTTON3) is pressed, 
			// go through selecting a new polygon.
			// If the left is pressed, go through manipualting a 
			// polygon, and make sure it isn't
			// dropped when the user clicks outside of it.
			mc.mousePressedOnAt(imp, x, y, e.getButton(), e.getModifiers());
		}
	}
}
