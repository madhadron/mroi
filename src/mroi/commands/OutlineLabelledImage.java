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
package mroi.commands;

import java.awt.AWTEvent;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.AWTEvent;
import java.awt.event.KeyEvent;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;
import java.io.*;
import java.awt.*;
import ij.plugin.filter.Analyzer;
import mroi.io.MalformedGeometryFileException;
import mroi.io.MroiListReader;
import mroi.io.MroiListWriter;
import ij.*;
import ij.process.*;
import ij.gui.*;
import java.util.*;
import static mroi.geometry.GeometryUtilities.*;
import mroi.MZipper;
import mroi.Zipper;

public class OutlineLabelledImage extends MutatingCommand<Geometry> {
    static public GeometryFactory gfact = new GeometryFactory();
    public final ImagePlus imp;

    public boolean isInvoked(String lbl) {
	return lbl.equalsIgnoreCase("outline");
    }

    public OutlineLabelledImage(ImagePlus imp) {
	this.imp = imp;
    }

    public MZipper<Geometry> operation(MZipper<Geometry> mz) {
	ImageProcessor ip = this.imp.getProcessor();
	MZipper<Geometry> res = mz;
	double maxVal = ip.getMax();
	for (int lvl = 1; lvl < maxVal; lvl++) {
	    ImageProcessor mask = ip.duplicate();
	    for (int i = 0; i <= ip.getWidth(); i++) {
		for (int j = 0; j <= ip.getHeight(); j++) {
		    if (mask.get(i,j) == lvl) {
			mask.set(i,j,255);
		    } else {
			mask.set(i,j,0);
		    }
		}
	    }
	    res = res.add(roiToGeom(outlineBlob(mask)));
	}
	return res;
    }

    public PolygonRoi outlineBlob(ImageProcessor ip) {
	ImagePlus imp = new ImagePlus("",ip);
	imp.getProcessor().invert();
	ImageStatistics stat = new ImageStatistics();
	//	stat.min = minMouse_Area;
	//	stat.max = maxMouse_Area;
	stat = imp.getStatistics(Analyzer.CENTER_OF_MASS);

	// selects only mouse for ellipse fitting
	Wand wand = new Wand(ip);
	wand.autoOutline((int) stat.xCenterOfMass,(int) stat.yCenterOfMass,0,128);
        
	PolygonRoi roi = new PolygonRoi(wand.xpoints, wand.ypoints, wand.npoints, Roi.TRACED_ROI);
	return roi;
    }
}
	    