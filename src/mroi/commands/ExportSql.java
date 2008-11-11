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
import ij.gui.GenericDialog;
import java.awt.event.KeyEvent;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;
import java.io.*;
import java.awt.*;
import mroi.MZipper;
import ij.WindowManager;
import mroi.Zipper;
import mroi.io.MroiListWriter;
import ij.IJ;
import java.util.*;

import javax.swing.JFileChooser;

public class ExportSql implements Command<Geometry> {

    public Zipper<Map<Integer,MZipper<Geometry>>> exec(final Zipper<Map<Integer,MZipper<Geometry>>> z, int frame) {
	JFileChooser fc = new JFileChooser();
	int returnval = fc.showSaveDialog(WindowManager.getCurrentWindow().getCanvas());
	GenericDialog gd = new GenericDialog("Point number");
	gd.addNumericField("Point: ", 0, 0);
	gd.showDialog();
	if (gd.wasCanceled()) return z;
	int point = (int) gd.getNextNumber();
	if (returnval == JFileChooser.APPROVE_OPTION) {
	    try {
		File f = new File(fc.getSelectedFile().getCanonicalPath());
		BufferedWriter out = new BufferedWriter(new FileWriter(f));
		Map<Integer,MZipper<Geometry>> rs = z.current;
		out.append("INSERT INTO points(pointid) VALUES (" + point + ");\n");
		for (Integer e : rs.keySet()) {
		    MZipper<Geometry> zp = rs.get(e);
		    if (zp.size() > 0) {
			out.append("INSERT INTO frames (frameid,framepoint) VALUES (" + e + "," + point + ");\n");
			for (Geometry g : zp.asList()) {
			    if (g.isValid()) {
				out.append("INSERT INTO polygons (polyshape,polyframe,polypoint) VALUES (" + 
					 "GeomFromText('" + g.toText()+ "'), " + e + ", " + point + ");\n");
			    }
			}
		    }
		}
		out.append("SELECT updatematviews();");
		out.flush();
	    } catch (IOException e) {
	    	IJ.error("Couldn't write SQL to " + fc.getSelectedFile().getName() + ": " + e.getMessage());
	    }

	}
    return z;
    }
	

	public boolean isInvoked(String lbl) {
		return lbl.equalsIgnoreCase("exportsql");
	}
	
	public MZipper operation(MZipper mz) {
	    // Not used.
	    return null;
	}
}
