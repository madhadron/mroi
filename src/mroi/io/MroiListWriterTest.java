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
package mroi.io;

import static org.junit.Assert.*;
import java.util.*;
import mroi.*;
import mroi.test.*;
import java.io.*;

import org.junit.Before;
import org.junit.Test;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;

public class MroiListWriterTest {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testWrite() throws Exception {
		GeometryFactory gfact = new GeometryFactory();
		WKTReader rr = new WKTReader();
		List<Geometry> ls = new ArrayList<Geometry>(5);
		for (int i = 0; i < 2; i++)
			ls.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		Geometry c = rr.read("POLYGON((0 0, 20 0, 20 20, 0 20, 0 0))");
		MZipper<Geometry> j = new JZipper<Geometry>(ls, c, ls);
		Map<Integer,MZipper<Geometry>> toWrite = new HashMap<Integer,MZipper<Geometry>>();
		toWrite.put(1, j);
		toWrite.put(2, new JZipper<Geometry>(new ArrayList<Geometry>(0), c, ls));
		toWrite.put(3, new NZipper<Geometry>(ls, new ArrayList<Geometry>(0)));
		
		MroiListWriter m = new MroiListWriter();
		StringWriter s = new StringWriter();
		BufferedWriter sprime = new BufferedWriter(s);
		m.write(sprime, toWrite);
		sprime.flush();
		String correctOutput="{"+
		    "2=JZipper([],POLYGON((0 0, 20 0, 20 20, 0 20, 0 0)),[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		    "1=JZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))],POLYGON((0 0, 20 0, 20 20, 0 20, 0 0)),[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)),POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		    "3=NZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))],[])}";
		String expected = correctOutput.replaceAll("\\s+","");
		String found = s.getBuffer().toString().replaceAll("\\s+","");
		assertEquals("Wrong value written:\n" + expected + "\n" + found, expected, found);
		
	}

}
