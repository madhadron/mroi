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
import mroi.*;
import java.util.*;
import java.io.*;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;

import org.junit.Before;
import org.junit.Test;

public class MroiListReaderTest {
	WKTReader rr = new WKTReader();
	MroiListReader m = new MroiListReader();

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testRead() throws Exception {
		GeometryFactory gfact = new GeometryFactory();

		String input="{1=JZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]"+
		",POLYGON((0 0, 20 0, 20 20, 0 20, 0 0)),[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)),POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		"2=JZipper([],POLYGON((0 0, 20 0, 20 20, 0 20, 0 0)),[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		"3=NZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))],[])}";
		BufferedReader b = new BufferedReader(new StringReader(input));
		Map<Integer,MZipper<Geometry>> geoms = m.read(b);
		
		List<Geometry> g = new ArrayList<Geometry>(2);
		g.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		g.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		Geometry c = rr.read("POLYGON((0 0, 20 0, 20 20, 0 20, 0 0))");
		List<Geometry> empty = new ArrayList<Geometry>(0);
		Map<Integer,MZipper<Geometry>> testGeoms = new HashMap<Integer,MZipper<Geometry>>();
		testGeoms.put(1, new JZipper<Geometry>(g,c,g));
		testGeoms.put(2, new JZipper<Geometry>(empty,c,g));
		testGeoms.put(3, new NZipper<Geometry>(g,empty));
		
		assertEquals("Wrong contents read.", testGeoms.keySet(), geoms.keySet());
	}
	
	@Test(expected=MalformedGeometryFileException.class)
	public void testReadBadGeometryFile() throws Exception {
		String input="{1=JZier([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]"+
		",POLYGON((0 0, 20 0, 20 20, 0 20, 0 0)),[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)),POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		"2=JZipper([],POLYGON((0 0, 2OLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		"3=NZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))],[])}";
		BufferedReader b = new BufferedReader(new StringReader(input));
		Map<Integer,MZipper<Geometry>> geoms = (new MroiListReader()).read(b);
	}
	
	@Test
	public void testParseList() throws Exception {
		String input = "[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]";
		List<Geometry> correctVersion = new ArrayList<Geometry>(2);
		correctVersion.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		correctVersion.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		List<Geometry> testVersion = m.parseList(input);
		assertEquals("List has wrong length.", 2, testVersion.size());
		assertTrue("Entry 0 is wrong.", correctVersion.get(0).equals(testVersion.get(0)));
		assertTrue("Entry 1 is wrong.", correctVersion.get(1).equals(testVersion.get(1)));
	}
	
	@Test(expected=MalformedGeometryFileException.class)
	public void testParseListBadFormat() throws Exception {
		String input = "[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), POLGON((0 0, 10 0, 10 10, 0 10, 0 0))]";
		m.parseList(input);
	}
	
	@Test
	public void testParseNZipper() throws Exception {
		String input = "NZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 20 0, 10 10, 0 10, 0 0))],[])";
		List<Geometry> g = new ArrayList<Geometry>(2);
		g.add(rr.read("POLYGON((0 0, 20 0, 10 10, 0 10, 0 0))"));
		g.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		List<Geometry> empty = new ArrayList<Geometry>(0);
		NZipper<Geometry> correctVersion = new NZipper<Geometry>(g,empty);
		NZipper<Geometry> testVersion = m.parseNZipper(input);

		assertEquals("Lefts has wrong size.", correctVersion.lefts.size(), testVersion.lefts.size());
		assertEquals("Rights has wrong size.", correctVersion.lefts.size(), testVersion.lefts.size());
		assertTrue("Lefts 0 is wrong.", correctVersion.lefts.get(0).equals(testVersion.lefts.get(0)));
		assertTrue("Lefts 1 is wrong.", correctVersion.lefts.get(1).equals(testVersion.lefts.get(1)));
	}
	
	@Test(expected=MalformedGeometryFileException.class)
	public void testParseNZipperBadFile() throws Exception {
		String input = "NZipper([POLYON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 20 0, 10 10, 0 10, 0 0)][])";
		m.parseNZipper(input);
	}
	
	@Test
	public void testParseJZipper() throws Exception {
		String input = "JZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 20 0, 10 10, 0 10, 0 0))],POLYGON((0 0, 5 0, 5 5, 0 5, 0 0)),[])";
		List<Geometry> g = new ArrayList<Geometry>(2);
		g.add(rr.read("POLYGON((0 0, 20 0, 10 10, 0 10, 0 0))"));
		g.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		List<Geometry> empty = new ArrayList<Geometry>(0);
		JZipper<Geometry> correctVersion = new JZipper<Geometry>(g,rr.read("POLYGON((0 0, 5 0, 5 5, 0 5, 0 0))"), g);
		JZipper<Geometry> testVersion = m.parseJZipper(input);

		assertEquals("Lefts has wrong size.", correctVersion.lefts.size(), testVersion.lefts.size());
		assertEquals("Rights has wrong size.", correctVersion.lefts.size(), testVersion.lefts.size());
		assertTrue("Lefts 0 is wrong.", correctVersion.lefts.get(0).equals(testVersion.lefts.get(0)));
		assertTrue("Lefts 1 is wrong.", correctVersion.lefts.get(1).equals(testVersion.lefts.get(1)));
		assertTrue("Current is wrong.", correctVersion.current.equals(testVersion.current));
	}
	
	@Test(expected=MalformedGeometryFileException.class)
	public void testParseJZipperBadFile() throws Exception {
		String input = "JZipper([PLYGON((0 0, 10 0, 10 10, 0 10, 0 0), POLYGON((0 0, 20 0, 10 10, 0 10, 0 0))],POLYGON(0 0, 5 0, 5 5, 0 5, 0 0)),])";
		m.parseJZipper(input);
	}
	
	@Test
	public void testParseMap() throws Exception {
		String input="{1=JZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]"+
		",POLYGON((0 0, 20 0, 20 20, 0 20, 0 0)),[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)),POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		"2=JZipper([],POLYGON((0 0, 20 0, 20 20, 0 20, 0 0)),[POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))]), " +
		"3=NZipper([POLYGON((0 0, 10 0, 10 10, 0 10, 0 0)), POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))],[])}";
		List<Geometry> g = new ArrayList<Geometry>(2);
		g.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		g.add(rr.read("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"));
		Geometry c = rr.read("POLYGON((0 0, 20 0, 20 20, 0 20, 0 0))");
		List<Geometry> empty = new ArrayList<Geometry>(0);
		Map<Integer,MZipper<Geometry>> correctVersion = new HashMap<Integer,MZipper<Geometry>>();
		correctVersion.put(1, new JZipper<Geometry>(g,c,g));
		correctVersion.put(2, new JZipper<Geometry>(empty,c,g));
		correctVersion.put(3, new NZipper<Geometry>(g,empty));
		
		Map<Integer,MZipper<Geometry>> testVersion = m.parseMap(input);
		assertEquals("Different keysets.", correctVersion.keySet(), testVersion.keySet());
		assertTrue("Slice 1, wrong type.", correctVersion.get(1) instanceof JZipper);
		assertTrue("Slice 2, wrong type.", correctVersion.get(2) instanceof JZipper);
		assertTrue("Slice 3, wrong type.", correctVersion.get(3) instanceof NZipper);
		
	}
	
}