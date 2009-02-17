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
import java.awt.Font;
import java.awt.font.*;
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
import kawa.standard.*;
import java.util.*;
import java.awt.event.*;
import java.awt.font.TextLayout;

public class State {
	public Zipper<Map<Integer, MZipper<RoiContainer>>> rois;
	public ArrayList<Command<RoiContainer>> keyCommands = new ArrayList<Command<RoiContainer>>();
	public int currentSlice;
	public boolean showPreviousSlice;
	public boolean showRoiIds;

	public State(int numberOfSlices) {
		Scheme s = new Scheme();
		Scheme.registerEnvironment();
		rois = new Zipper<Map<Integer, MZipper<RoiContainer>>>(null,
				new HashMap<Integer, MZipper<RoiContainer>>(), null);
		for (int i = 1; i <= numberOfSlices; i++) {
			rois.current.put(i, new NZipper<RoiContainer>(null, null));
		}
		keyCommands.add(new Undo<RoiContainer>());
		keyCommands.add(new Redo<RoiContainer>());
//		keyCommands.add(new Save());
		keyCommands.add(new LegacyLoad());
		keyCommands.add(new Load());
		keyCommands.add(new Delete<RoiContainer>());
		keyCommands.add(new ExportSql());
		keyCommands.add(new Copy<RoiContainer>());
		keyCommands.add(new Paste());
		keyCommands.add(new ToggleVisible());
		keyCommands.add(new ToggleNumbers());
		keyCommands.add(new GoToID());
		keyCommands.add(new ShowMean());
		keyCommands.add(new ScmSave());
		this.currentSlice = 1;
		this.showPreviousSlice = false;
		this.showRoiIds = false;
	}

	public void goToFrameOn(Integer slice, ImagePlus imp) {
		syncRoiFrom(imp);
		if (slice == null || slice == 0) {
			this.currentSlice = imp.getCurrentSlice();
			imp.updateAndRepaintWindow();
		} else {
			this.currentSlice = slice;
			imp.setSlice(slice);
			imp.updateAndRepaintWindow();
		}
		syncRoiTo(imp);
	}

	public void goToFrameOn(ImagePlus imp) {
		goToFrameOn(null,imp);
	}
	
	public void executeCommandOn(String lbl, ImagePlus imp)
			throws NoSuchCommandException {
		boolean invoked = false;
		for (Command<RoiContainer> c : keyCommands) {
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
	
	public Geometry fetchCurrentRoiAsGeometry() {
		if (rois.current.get(currentSlice) instanceof JZipper)
			return ((JZipper<RoiContainer>) rois.current.get(currentSlice)).current.getGeometry();
		else
			return null;
	}
	
	public RoiContainer fetchCurrentRoi() {
		if (rois.current.get(currentSlice) instanceof JZipper)
			return ((JZipper<RoiContainer>) rois.current.get(currentSlice)).current;
		else
			return null;	
	}
	
	public Roi fetchCurrentRoiAsRoi() {
		return geomToRoi(fetchCurrentRoiAsGeometry());
	}

	public Integer previousNonemptyFrame() {
		for (int i = currentSlice-1; i > 0; i--) {
			if (rois.current.get(i).size() > 0) {
				return i;
			}
		}
		return null;
	}
	
	public void syncRoiFrom(ImagePlus imp) {
		if (imp.getRoi() != null) {
			if (rois.current.get(currentSlice) == null) {
				rois.current.put(currentSlice, new NZipper<RoiContainer>(
						new ArrayList<RoiContainer>(), new ArrayList<RoiContainer>()));
			}
			if (rois.current.get(currentSlice) instanceof JZipper) {
				Update<RoiContainer> u = new Update<RoiContainer>(getRoiFrom(imp));
				rois = u.exec(rois, currentSlice);
			} else if (rois.current.get(currentSlice) instanceof NZipper) {
				Add<RoiContainer> a = new Add<RoiContainer>(getRoiFrom(imp));
				rois = a.exec(rois, currentSlice);
				fetchCurrentRoi().setPredecessor(choosePreviousRoi());
			}
		}
	}

	public void syncRoiTo(ImagePlus imp) {
		setRoiOn(imp, fetchCurrentRoi());
	}

	public void paint(Graphics g, Rectangle visibleWindow, double magnification) {
		paintFrame(g, visibleWindow, magnification, currentSlice, Color.green, Color.red, false);
		if (showPreviousSlice) {
			Integer prevFrame = previousNonemptyFrame();
			if (prevFrame != null) {
				paintFrame(g, visibleWindow, magnification, previousNonemptyFrame(),
						Color.blue, Color.gray, true);
				drawCentroidConnections(g, visibleWindow, magnification, currentSlice, Color.orange);
			}
		}
		if (showRoiIds) {
			paintRoiNumbers(g,visibleWindow,magnification,currentSlice,Color.green, Color.red);
		}
	}
	
	public void paintFrame(Graphics g, Rectangle visibleWindow, 
						double magnification, Integer frame,
						Color validColor, Color invalidColor, boolean showCurrent) {
		List<RoiContainer> k;
		if (showCurrent) {
			k = rois.current.get(frame).asList();
		} else {
			k = rois.current.get(frame).asListWithoutCurrent();
		}
		for (RoiContainer geo : k) {
			drawGeometry(geo.getGeometry(), g, visibleWindow, magnification, validColor, invalidColor);
		}
		
	}
	

	public void paintRoiNumber(Graphics g, Rectangle visWindow, double mag, RoiContainer r, Color validColor, Color invalidColor) {
		if (r.getGeometry().isValid()) {
			g.setColor(validColor);
			g.drawString(r.id.toString(), getCentroidX(visWindow,mag,r), getCentroidY(visWindow,mag,r));
		} else {
			g.setColor(invalidColor);
			Coordinate c = r.getGeometry().getCoordinate();
			g.drawString(r.id.toString(), toScreenX(visWindow,mag,c.x), toScreenY(visWindow,mag,c.y));
		}

	}
	
	public void paintRoiNumbers(Graphics g, Rectangle visibleWindow,
			double magnification, Integer frame,
			Color validColor, Color invalidColor) {
		for (RoiContainer geo : rois.current.get(frame).asList()) {
			paintRoiNumber(g,visibleWindow,magnification,geo,validColor,invalidColor);
		}
	}

	
	public void drawCentroidConnections(Graphics g, Rectangle visibleWindow,
			double magnification, Integer frame, Color color) {
		for (RoiContainer r : rois.current.get(frame).asList()) {
			drawCentroidConnection(g, visibleWindow, magnification, r, color);
		}
	}
	
	public int toScreenX(Rectangle visibleWindow, double magnification, double x) {
		return ((int) Math.round(magnification*(x - visibleWindow.x)));
		
	}
	
	public int toScreenY(Rectangle visibleWindow, double magnification, double y) {
		return((int) Math.round(magnification*(y - visibleWindow.y)));
		
	}
	
	public int getCentroidX(Rectangle visibleWindow, double magnification, RoiContainer roi) {
		return toScreenX(visibleWindow,magnification,roi.getGeometry().getCentroid().getX());
	}
	
	public int getCentroidY(Rectangle visibleWindow, double magnification, RoiContainer roi) {
		Point centroid = roi.getGeometry().getCentroid();
		return toScreenY(visibleWindow,magnification,centroid.getY());
		
	}
	
	public void drawCentroidConnection(Graphics g, Rectangle visibleWindow,
			double magnification, RoiContainer roi, Color color) {
		if (roi.predecessor != null) {
			int px = getCentroidX(visibleWindow,magnification,roi);
			int py = getCentroidY(visibleWindow,magnification,roi);
			int qx = getCentroidX(visibleWindow,magnification,roi.predecessor);
			int qy = getCentroidY(visibleWindow,magnification,roi.predecessor);
			g.setColor(color);
			g.drawLine(px, py, qx, qy);
		}
	}

	public void drawGeometry(Geometry geom, Graphics g,
			Rectangle visibleWindow, double magnification,
			Color validColor, Color invalidColor) {
		if (geom.isValid()) {
			g.setColor(validColor);
		} else {
			g.setColor(invalidColor);
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

	public void setRoiOn(ImagePlus imp, RoiContainer g) {
		if (g == null) {
			imp.setRoi((Roi) null);
		} else
			imp.setRoi(g.getRoi());
	}

	public RoiContainer getRoiFrom(ImagePlus imp) {
		if (imp.getRoi() == null)
			return null;
		else {
			RoiContainer c = fetchCurrentRoi();
			if (c == null) {
				return new RoiContainer(roiToGeom(imp.getRoi()));
			} else {
				return new RoiContainer(c.id, roiToGeom(imp.getRoi()), c.predecessor);
			}
		}
	}
	
	public Geometry selectAt(Integer x, Integer y) {
		Point p = gfact.createPoint(new Coordinate(x, y));
		ContainmentPredicate cpr = new ContainmentPredicate(p);
		Select<RoiContainer> sel = new Select<RoiContainer>(cpr);
		sel.exec(rois, currentSlice);
		return fetchCurrentRoiAsGeometry();
	}
	
	public Integer getCurrentSlice() { return currentSlice; }
	
	public RoiContainer chooseRoiAt(Integer x, Integer y, int frame) {
		Point p = gfact.createPoint(new Coordinate(x, y));
		ContainmentPredicate cpr = new ContainmentPredicate(p);
		MZipper<RoiContainer> res =  rois.current.get(frame).select(cpr);
		if (res instanceof JZipper)
			return ((JZipper<RoiContainer>)res).current;
		else
			return null;
	}

	public RoiContainer choosePreviousRoi() {
		Geometry current = fetchCurrentRoiAsGeometry();
		Integer previousFrame = previousNonemptyFrame();
		if (previousFrame == null) return null;
		else {
			List<RoiContainer> previousRois = rois.current.get(previousFrame).asList();
			RoiContainer res = previousRois.get(0);
			double maxarea = current.intersection(res.getGeometry()).getArea();
			double trialarea;
			RoiContainer test;
			for (int i = 1; i < previousRois.size(); i++) {
				test = previousRois.get(i);
				trialarea = current.intersection(previousRois.get(i).getGeometry()).getArea();
				if (trialarea > maxarea) {
					res = test;
					maxarea = trialarea;
				}
			}
			return res;
		}
	}
	
	public void togglePreviousFrameVisible() {
		this.showPreviousSlice = !showPreviousSlice;
	}
	
	public void toggleShowRoiIds() {
		this.showRoiIds = !showRoiIds;
	}
	
	public boolean previousSliceVisible() {
		return this.showPreviousSlice;
	}

}