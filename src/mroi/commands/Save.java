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
import java.awt.event.KeyEvent;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;
import java.io.*;
import java.awt.*;
import mroi.MZipper;
import ij.WindowManager;
import mroi.Zipper;
import mroi.RoiContainer;
import mroi.io.MroiListWriter;
import ij.IJ;
import java.util.*;

import javax.swing.JFileChooser;

public class Save implements Command<RoiContainer> {

	public Zipper<Map<Integer,MZipper<RoiContainer>>> exec(final Zipper<Map<Integer,MZipper<RoiContainer>>> z, int frame) {
//		Frame fr = new Frame();
//		FileDialog fd = new FileDialog(fr, "Select file to write Rois to", FileDialog.SAVE);
//		fd.setVisible(true);
		JFileChooser fc = new JFileChooser();
		int returnval = fc.showSaveDialog(WindowManager.getCurrentWindow().getCanvas());
		if (returnval == JFileChooser.APPROVE_OPTION) {
			try {
				File f = new File(fc.getSelectedFile().getCanonicalPath());
				BufferedWriter out = new BufferedWriter(new FileWriter(f));
				MroiListWriter m = new MroiListWriter();
				m.write(out, z.current);
				out.close();
			} catch (IOException e) {
				IJ.error("Couldn't write Rois to " + fc.getSelectedFile().getName() + ": " + e.getMessage());
			}
		}
		return z;
	}

	public boolean isInvoked(String lbl) {
		return lbl.equalsIgnoreCase("save");
	}


	public MZipper operation(MZipper mz) {
		// Not used.
		return null;
	}

}
