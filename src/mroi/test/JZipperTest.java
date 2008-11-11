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
package mroi.test;

import static org.junit.Assert.*;
import java.util.*;

import mroi.JZipper;
import mroi.MZipper;
import mroi.NZipper;
import mroi.test.*;
import org.junit.Before;
import org.junit.Test;

public class JZipperTest {
	JZipper<IntCon> j;
	@Before
	public void setUp() throws Exception {
		List<IntCon> ls = new ArrayList<IntCon>(5);
		for (int i = 0; i < 5; i++)
			ls.add(new IntCon(4-i));
		List<IntCon> rs = new ArrayList<IntCon>(7);
		for (int i = 6; i < 13; i++)
			rs.add(new IntCon(i));
		IntCon c = new IntCon(5);
		j = new JZipper<IntCon>(ls, c, rs);
	} 

	@Test
	public void testJZipper() {
		assertEquals("Left list has right size from constructor.", 5, j.lefts.size());
		assertEquals("Right list has right size from constructor.", 7, j.rights.size());
		assertEquals("Current entry has right value from constructor.", (Integer)5, j.current.i);
	}

	@Test
	public void testAdd() {
		JZipper<IntCon> jprime = j.add(new IntCon(55));
		assertEquals("Current entry is wrong after add.", (Integer)55, jprime.current.i);
		assertEquals("Left side list length is wrong after add.", 6, jprime.lefts.size());
		assertEquals("Right side list length is wrong after add.", 7, jprime.rights.size());
		assertEquals("Left side entry 0 is wrong after add.", (Integer)5, jprime.lefts.get(0).i);
	}

	@Test
	public void testDelete() {
		NZipper<IntCon> jprime = j.delete();
		assertEquals("Left side list length is wrong after add.", 5, jprime.lefts.size());
		assertEquals("Right side list length is wrong after add.", 7, jprime.rights.size());
	}

        @Test
	public void testSize() {
	    assertEquals("Size returns correct value.", 13, j.size());
	}

	@Test
	public void testSelect() {
		JZipper<IntCon> jprime = (JZipper<IntCon>)j.select(new IntegerEquality(1));
		assertEquals("Current value is wrong after select for 1.", (Integer)1, jprime.current.i);
		assertEquals("Left side list length is wrong after select for 1.", 1, jprime.lefts.size());
		assertEquals("Right side list length is wrong after select for 1.", 11, jprime.rights.size());
		assertEquals("Left side list head is wrong after select for 1.", (Integer)0, jprime.lefts.get(0).i);
		assertEquals("Right side list head is wrong after select for 1.", (Integer)2, jprime.rights.get(0).i);
		assertEquals("Right side list cadr is wrong after select for 1.", (Integer)3, jprime.rights.get(1).i);
		JZipper<IntCon> j2 = (JZipper<IntCon>)j.select(new IntegerEquality(0));
		assertEquals("Current value is wrong after select for 0.", (Integer)0, j2.current.i);
		assertEquals("Left side list length is wrong after select for 0.", 0, j2.lefts.size());
		assertEquals("Right side list length is wrong after select for 0.", 12, j2.rights.size());
		assertEquals("Right side list head is wrong after select for 0.", (Integer)1, j2.rights.get(0).i);
		assertEquals("Right side list cadr is wrong after select for 0.", (Integer)2, j2.rights.get(1).i);
		JZipper<IntCon> j3 = (JZipper<IntCon>)j.select(new IntegerEquality(12));
		assertEquals("Current value is wrong after select for 12.", (Integer)12, j3.current.i);
		assertEquals("Left side list length is wrong after select for 12.", 12, j3.lefts.size());
		assertEquals("Right side list length is wrong after select for 12.", 0, j3.rights.size());
		assertEquals("Left side list head is wrong after select for 12.", (Integer)11, j3.lefts.get(0).i);
		assertEquals("Left side list cadr is wrong after select for 12.", (Integer)10, j3.lefts.get(1).i);
		MZipper<IntCon> j4 = j.select(new IntegerEquality(100));
		assertEquals("Class is wrong after selecting nonexistent entry.", NZipper.class, j4.getClass());
		NZipper<IntCon> j4p = (NZipper<IntCon>)j4;
		assertEquals("Total number of entries is wrong.", 13, j4p.lefts.size() + j4p.rights.size());
		assertEquals("Left head is not immediately before right head.", (Integer)(j4p.lefts.get(0).i), (Integer)(j4p.rights.get(0).i - 1));
	}

	@Test
	public void testEquals() {
		List<IntCon> ls = new ArrayList<IntCon>(5);
		for (int i = 0; i < 5; i++)
			ls.add(new IntCon(4-i));
		List<IntCon> rs = new ArrayList<IntCon>(7);
		for (int i = 6; i < 13; i++)
			rs.add(new IntCon(i));
		IntCon c = new IntCon(5);
		MZipper<IntCon> works = new JZipper<IntCon>(ls, c, rs);
		assertTrue("Identical MZipper not considered equal.", j.equals(works));
		MZipper<IntCon> noworks = new JZipper<IntCon>(rs, c, ls);
		assertFalse("Non-identical MZipper considered equal.", j.equals(noworks));
	}

	@Test
	public void testClone() {
		JZipper<IntCon> j2 = (JZipper<IntCon>)j.clone();
		assertEquals("Left length is incorrect after clone.", 5, j2.lefts.size());
		assertEquals("Right length is incorrect after clone.", 7, j2.rights.size());
		assertEquals("Current value is wrong after clone.", (Integer)5, j2.current.i);
		j2.lefts.remove(0);
		assertEquals("Original is changed by modifying clone.", 5, j.lefts.size());
	}

	@Test
	public void testToString() {
		assertEquals("Wrong string returned for encoding of zipper.", "JZipper([0,1,2,3,4],5,[6,7,8,9,10,11,12])", j.toString().replaceAll("\\s+",""));
	}
	

}
