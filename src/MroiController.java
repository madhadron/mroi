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

public class MroiController {
	public Zipper<Map<Integer, MZipper<Geometry>>> rois;
	public ArrayList<Command<Geometry>> keyCommands = new ArrayList<Command<Geometry>>();
	public int currentSlice;

	public MroiController(int numberOfSlices) {
		rois = new Zipper<Map<Integer, MZipper<Geometry>>>(null,
				new HashMap<Integer, MZipper<Geometry>>(), null);
		for (int i = 1; i <= numberOfSlices; i++) {
			rois.current.put(i, new NZipper<Geometry>(null, null));
		}
		keyCommands.add(new Undo<Geometry>());
		keyCommands.add(new Redo<Geometry>());
		keyCommands.add(new Save());
		keyCommands.add(new Load());
		keyCommands.add(new Delete<Geometry>());
		keyCommands.add(new ExportSql());
		keyCommands.add(new Copy<Geometry>());
		keyCommands.add(new Paste());
		keyCommands.add(new PadLine());
	}

	public void goToFrameOn(Integer slice, ImagePlus imp) {
		if (slice == null) {
			currentSlice = imp.getCurrentSlice();
			imp.updateAndRepaintWindow();
		} else {
			currentSlice = slice;
			imp.setSlice(slice);
			imp.updateAndRepaintWindow();
		}
	}

	public void executeCommandOn(String lbl, ImagePlus imp)
			throws NoSuchCommandException {
		boolean invoked = false;
		for (Command<Geometry> c : keyCommands) {
			if (c.isInvoked(lbl)) {
				invoked = true;
				syncRoiFrom(imp);
				rois = c.exec(rois, currentSlice);
				syncRoiTo(imp);
				imp.updateAndRepaintWindow();
			}
		}
		if (!invoked) {
			throw new NoSuchCommandException(lbl);
		}
	}

	public void syncRoiFrom(ImagePlus imp) {
		if (imp.getRoi() != null) {
			if (rois.current.get(currentSlice) == null) {
				rois.current.put(currentSlice, new NZipper<Geometry>(
						new ArrayList<Geometry>(), new ArrayList<Geometry>()));
			}
			if (rois.current.get(currentSlice) instanceof JZipper) {
				Update<Geometry> u = new Update<Geometry>(getRoiFrom(imp));
				rois = u.exec(rois, currentSlice);
			} else if (rois.current.get(currentSlice) instanceof NZipper) {
				Add<Geometry> a = new Add<Geometry>(getRoiFrom(imp));
				rois = a.exec(rois, currentSlice);
			}
		}
	}

	public void syncRoiTo(ImagePlus imp) {
		if (rois.current.get(currentSlice) instanceof NZipper)
			imp.setRoi((Roi) null);
		else if (rois.current.get(currentSlice) instanceof JZipper)
			setRoiOn(
					imp,
					((JZipper<Geometry>) rois.current.get(currentSlice)).current);
	}

	public void paint(Graphics g, Rectangle visibleWindow, double magnification) {
		for (Geometry geo : rois.current.get(currentSlice)
				.asListWithoutCurrent()) {
			drawGeometry(geo, g, visibleWindow, magnification);
		}
	}

	public void drawGeometry(Geometry geom, Graphics g,
			Rectangle visibleWindow, double magnification) {
		if (geom.isValid()) {
			g.setColor(Color.green);
		} else {
			g.setColor(Color.red);
		}
		drawUnselectedRoi(geomToRoi(geom), g, visibleWindow, magnification);
	}

	public void drawUnselectedRoi(Roi r, Graphics g, Rectangle rect, double mag) {
		int[] x = r.getPolygon().xpoints;
		int[] y = r.getPolygon().ypoints;
		for (int i = 1; i < r.getPolygon().npoints; i++) {
			int px = (int) Math.round((x[i - 1] - rect.x) * mag);
			int py = (int) Math.round((y[i - 1] - rect.y) * mag);
			int qx = (int) Math.round((x[i] - rect.x) * mag);
			int qy = (int) Math.round((y[i] - rect.y) * mag);
			g.drawLine(px, py, qx, qy);
		}
		int n = r.getPolygon().npoints;
		int px = (int) Math.round((x[n - 1] - rect.x) * mag);
		int py = (int) Math.round((y[n - 1] - rect.y) * mag);
		int qx = (int) Math.round((x[0] - rect.x) * mag);
		int qy = (int) Math.round((y[0] - rect.y) * mag);
		g.drawLine(px, py, qx, qy);
	}

	public void setRoiOn(ImagePlus imp, Geometry g) {
		if (g == null) {
			imp.setRoi((Roi) null);
		} else
			imp.setRoi(geomToRoi(g));
	}

	public Geometry getRoiFrom(ImagePlus imp) {
		if (imp.getRoi() == null)
			return null;
		else
			return roiToGeom(imp.getRoi());
	}

	public void mousePressedOnAt(ImagePlus imp, Integer x, Integer y,
			int button, int modifiers) {
		if (button == MouseEvent.BUTTON3 || button == MouseEvent.BUTTON2) {
			syncRoiFrom(imp);
			Point p = gfact.createPoint(new Coordinate(x, y));
			ContainmentPredicate cpr = new ContainmentPredicate(p);
			Select<Geometry> sel = new Select<Geometry>(cpr);
			sel.exec(rois, currentSlice);
			syncRoiTo(imp);
		}
	}
}