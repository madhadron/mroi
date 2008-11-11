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

import mroi.*;
import com.vividsolutions.jts.geom.*;

public class PadLine extends MutatingCommand<Geometry> {
    static GeometryFactory g = new GeometryFactory();
    static double bufferWidth = 4;
    @Override public boolean isInvoked(String lbl) {
	return lbl.equalsIgnoreCase("padline");
    }

    @Override public MZipper<Geometry> operation(MZipper<Geometry> mz) {
	if (mz instanceof JZipper) {
	    Coordinate[] source = ((JZipper<Geometry>)mz).current.getCoordinates();
	    Coordinate[] newcoords = new Coordinate[source.length - 1];
	    for (int i = 0; i < source.length - 1; i++) {
		newcoords[i] = source[i];
	    }
	    Geometry gm = g.createLineString(newcoords);
	    return new JZipper<Geometry>(((JZipper<Geometry>)mz).lefts, 
				  gm.buffer(bufferWidth), 
				  ((JZipper<Geometry>)mz).rights);
	} else {
	    return mz;
	}
    }
}