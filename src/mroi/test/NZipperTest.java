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

import java.util.ArrayList;
import java.util.List;

import mroi.JZipper;
import mroi.MZipper;
import mroi.NZipper;

import org.junit.Before;
import org.junit.Test;

public class NZipperTest {
	NZipper<IntCon> j;
	@Before
	public void setUp() throws Exception {
		List<IntCon> ls = new ArrayList<IntCon>(5);
		for (int i = 0; i < 5; i++)
			ls.add(new IntCon(4-i));
		List<IntCon> rs = new ArrayList<IntCon>(7);
		for (int i = 6; i < 13; i++)
			rs.add(new IntCon(i));
		j = new NZipper<IntCon>(ls,rs);
	}

	@Test
	public void testNZipper() {
		assertEquals("Constructor leaves wrong length on left list.", 5, j.lefts.size());
		assertEquals("Constructor leaves wrong length of right list.", 7, j.rights.size());
		assertEquals("Constructor leaves wrong head on left list.", (Integer)4, j.lefts.get(0).i);
		assertEquals("Constructor leaves wrong head on right list.", (Integer)6, j.rights.get(0).i);
	}

	@Test
	public void testAdd() {
		JZipper<IntCon> jprime = j.add(new IntCon(55));
		assertEquals("Wrong left list length on add.", 5, jprime.lefts.size());
		assertEquals("Wrong right list length on add.", 7, jprime.rights.size());
		assertEquals("Wrong current after add.", (Integer)55, jprime.current.i);
		assertEquals("Wrong left head after add.", (Integer)4, jprime.lefts.get(0).i);
		assertEquals("Wrong right head after add.", (Integer)6, jprime.rights.get(0).i);
	}

	@Test
	public void testDelete() {
		NZipper<IntCon> jprime = j.delete();
		assertEquals("Wrong left list length after delete.", 5, jprime.lefts.size());
		assertEquals("Wrong right list length after delete.", 7, jprime.rights.size());
		assertEquals("Wrong left head after delete.", (Integer)4, jprime.lefts.get(0).i);
		assertEquals("Wrong right head after delete.", (Integer)6, jprime.rights.get(0).i);
	}

        @Test
	public void testSize() {
	    assertEquals("Size returns correct value.", 12, j.size());
	}

	@Test
	public void testSelect() {
		JZipper<IntCon> jprime = (JZipper<IntCon>)j.select(new IntegerEquality(1));
		assertEquals("Current value is wrong after select for 1.", (Integer)1, jprime.current.i);
		assertEquals("Left side list length is wrong after select for 1.", 1, jprime.lefts.size());
		assertEquals("Right side list length is wrong after select for 1.", 10, jprime.rights.size());
		assertEquals("Left side list head is wrong after select for 1.", (Integer)0, jprime.lefts.get(0).i);
		assertEquals("Right side list head is wrong after select for 1.", (Integer)2, jprime.rights.get(0).i);
		assertEquals("Right side list cadr is wrong after select for 1.", (Integer)3, jprime.rights.get(1).i);
		JZipper<IntCon> j2 = (JZipper<IntCon>)j.select(new IntegerEquality(0));
		assertEquals("Current value is wrong after select for 0.", (Integer)0, j2.current.i);
		assertEquals("Left side list length is wrong after select for 0.", 0, j2.lefts.size());
		assertEquals("Right side list length is wrong after select for 0.", 11, j2.rights.size());
		assertEquals("Right side list head is wrong after select for 0.", (Integer)1, j2.rights.get(0).i);
		assertEquals("Right side list cadr is wrong after select for 0.", (Integer)2, j2.rights.get(1).i);
		JZipper<IntCon> j3 = (JZipper<IntCon>)j.select(new IntegerEquality(12));
		assertEquals("Current value is wrong after select for 12.", (Integer)12, j3.current.i);
		assertEquals("Left side list length is wrong after select for 0.", 11, j3.lefts.size());
		assertEquals("Right side list length is wrong after select for 0.", 0, j3.rights.size());
		assertEquals("Left side list head is wrong after select for 0.", (Integer)11, j3.lefts.get(0).i);
		assertEquals("Left side list cadr is wrong after select for 0.", (Integer)10, j3.lefts.get(1).i);
		MZipper<IntCon> j4 = j.select(new IntegerEquality(100));
		assertEquals("Class is wrong after selecting nonexistent entry.", NZipper.class, j4.getClass());
		NZipper<IntCon> j4p = (NZipper<IntCon>)j4;
		assertEquals("Total number of entries is wrong.", 12, j4p.lefts.size() + j4p.rights.size());
	}

	@Test
	public void testEquals() {
		List<IntCon> ls = new ArrayList<IntCon>(5);
		for (int i = 0; i < 5; i++)
			ls.add(new IntCon(4-i));
		List<IntCon> rs = new ArrayList<IntCon>(7);
		for (int i = 6; i < 13; i++)
			rs.add(new IntCon(i));
		MZipper<IntCon> works = new NZipper<IntCon>(ls,rs);
		assertTrue("Identical MZipper not considered equal.", j.equals(works));
		MZipper<IntCon> noworks = new NZipper<IntCon>(rs, ls);
		assertFalse("Non-identical MZipper considered equal.", j.equals(noworks));
	}

	@Test
	public void testClone() {
		NZipper<IntCon> j2 = (NZipper<IntCon>)j.clone();
		assertEquals("Left length is incorrect after clone.", 5, j2.lefts.size());
		assertEquals("Right length is incorrect after clone.", 7, j2.rights.size());
		j2.lefts.remove(0);
		assertEquals("Original is changed by modifying clone.", 5, j.lefts.size());
	}

	@Test
	public void testToString() {
		assertEquals("Wrong string returned for encoding of zipper.", "NZipper([0,1,2,3,4],[6,7,8,9,10,11,12])", j.toString().replaceAll("\\s+",""));
	}

}
