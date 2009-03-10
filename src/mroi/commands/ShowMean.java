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
import java.util.*;
import mroi.RoiContainer;
import mroi.MZipper;
import mroi.Zipper;
import ij.*;
import static mroi.geometry.GeometryUtilities.*;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;



public class ShowMean extends SingleRoiNonmutatingCommand<RoiContainer> {

	public boolean isInvoked(String lbl) {
		return lbl.equalsIgnoreCase("showmean");
	}
	
	public RoiContainer singleRoiOperation(RoiContainer r) {
		ImagePlus imp = IJ.getImage();
		IJ.showMessage("Mean intensity of current ROI is " + meanIntensityOfOn(r.getGeometry(), imp));
		return r;
	}

}
