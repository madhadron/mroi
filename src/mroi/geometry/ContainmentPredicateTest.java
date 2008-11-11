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

package mroi.geometry;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;
import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import com.vividsolutions.jts.geom.GeometryFactory;

public class ContainmentPredicateTest {
	public GeometryFactory gfact = new GeometryFactory();

	@Test
	public void testApply() throws Exception {
		Geometry poly = (new WKTReader(gfact)).read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))");
		Point pin = gfact.createPoint(new Coordinate(5,5));
		Point pout = gfact.createPoint(new Coordinate(50,50));
		ContainmentPredicate prin = new ContainmentPredicate(pin);
		ContainmentPredicate prout = new ContainmentPredicate(pout);
		assertTrue("Doesn't contain inside point.", prin.apply(poly));
		assertFalse("Contains outside point.", prout.apply(poly));
	}

}
