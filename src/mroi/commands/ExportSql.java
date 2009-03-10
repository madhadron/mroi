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
import ij.ImagePlus;
import java.util.*;
import mroi.RoiContainer;
import static mroi.geometry.GeometryUtilities.*;

import javax.swing.JFileChooser;

public class ExportSql implements Command<RoiContainer> {
	final String insertPoint = "INSERT INTO points(id) VALUES(%d);\n";
	final String insertFrame = "INSERT INTO frames(id,point) VALUES (%d,%d);\n";
	final String insertPolygon = "INSERT INTO polygons(id,shape,frame,point,predecessor,meaninten) " +
		"VALUES(%d,GeomFromText('%s'),%d,%d,%s,%.2f);\n";
	static final Formatter f = new Formatter();
	
	public Zipper<Map<Integer, MZipper<RoiContainer>>> exec(
			final Zipper<Map<Integer, MZipper<RoiContainer>>> z, int frame) {
		JFileChooser fc = new JFileChooser();
		int returnval = fc.showSaveDialog(WindowManager.getCurrentWindow()
				.getCanvas());
		GenericDialog gd = new GenericDialog("Point number");
		gd.addNumericField("Point: ", 0, 0);
		gd.showDialog();
		if (gd.wasCanceled())
			return z;
		int point = (int) gd.getNextNumber();
		if (returnval == JFileChooser.APPROVE_OPTION) {
			ImagePlus imp = IJ.getImage();
			try {
				File f = new File(fc.getSelectedFile().getCanonicalPath());
				BufferedWriter out = new BufferedWriter(new FileWriter(f));
				Map<Integer, MZipper<RoiContainer>> rs = z.current;

				
				out.append(String.format(insertPoint, point));
				for (Integer e : rs.keySet()) {
					MZipper<RoiContainer> zp = rs.get(e);
					if (zp.size() > 0) {
						out.append(String.format(insertFrame, e, point));
						for (RoiContainer g : zp.asList()) {
							if (g.getGeometry().isValid()) {
								out.append(String.format(insertPolygon, g.id, g.getGeometry().toString(),
										e, point, g.getPredecessorIdAsString(), meanIntensityOfOn(g.getGeometry(), imp)));
							}
						}
					}
				}
				out.flush();
				out.close();
				IJ.showMessage("Finished exporting SQL.");
			} catch (IOException e) {
				IJ.error("Couldn't write SQL to "
						+ fc.getSelectedFile().getName() + ": "
						+ e.getMessage());
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
