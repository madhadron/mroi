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
import mroi.MroiLisp;
import mroi.RoiContainer;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.AWTEvent;
import java.awt.event.KeyEvent;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;
import java.io.*;
import java.awt.*;

import mroi.io.MalformedGeometryFileException;
import mroi.io.MroiListReader;
import mroi.io.MroiListWriter;
import ij.*;
import java.util.*;

import javax.swing.JFileChooser;

import mroi.MZipper;
import mroi.Zipper;

public class Load implements Command<RoiContainer> {

	public Zipper<Map<Integer,MZipper<RoiContainer>>> exec(Zipper<Map<Integer,MZipper<RoiContainer>>> z, int frame) {
		JFileChooser fc = new JFileChooser();
		int returnVal = fc.showOpenDialog(WindowManager.getCurrentWindow().getCanvas());
		Map<Integer,MZipper<RoiContainer>> newRois;
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			try {
				FileInputStream f = new FileInputStream(fc.getSelectedFile().getCanonicalPath()); 
				MroiLisp parser = new MroiLisp(f);
				parser.ReInit(f);
	            newRois = parser.roiFile();
//				z.rights.clear();
//				z.rights.add(newRois);
//				z = z.right();
//				return z;
				return z.insertAndStep(newRois);
			} catch (IOException e) {
				IJ.error("Couldn't open from " + fc.getSelectedFile().getName() + ": " + e.getMessage());
			} catch (mroi.ParseException e) {
				IJ.error("Failed in parsing: " + e.getMessage());
			} catch (Exception e) {
				IJ.error("Malformed input file: " + e.getMessage());
			}
		}
		return z;
	}

	public boolean isInvoked(String lbl) {
		return lbl.equalsIgnoreCase("load");
	}


	public MZipper operation(MZipper mz) {
		// Not used.
		return null;
	}

}
