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

import mroi.Zipper;

import org.junit.Before;
import org.junit.Test;

public class ZipperTest {
	Zipper<IntCon> j, jleftmost, jrightmost, jcenter;
	
	@Before
	public void setUp() throws Exception {
		List<IntCon> ls = new ArrayList<IntCon>(5);
		for (int i = 0; i < 5; i++)
			ls.add(new IntCon(4-i));
		List<IntCon> rs = new ArrayList<IntCon>(7);
		for (int i = 6; i < 13; i++)
			rs.add(new IntCon(i));
		IntCon c = new IntCon(5);
		j = new Zipper<IntCon>(ls, c, rs);
		List<IntCon> empty = new ArrayList<IntCon>(0);
		jleftmost = new Zipper<IntCon>(new ArrayList<IntCon>(empty), c, rs);
		jrightmost = new Zipper<IntCon>(ls, c, new ArrayList<IntCon>(empty));
		jcenter = new Zipper<IntCon>(new ArrayList<IntCon>(empty), c, new ArrayList<IntCon>(empty));
		
	}

	@Test
	public void testZipper() {
		assertEquals("Constructor leaves wrong length on left list.", 5, j.lefts.size());
		assertEquals("Constructor leaves wrong length of right list.", 7, j.rights.size());
		assertEquals("Constructor leaves wrong head on left list.", (Integer)4, j.lefts.get(0).i);
		assertEquals("Constructor leaves wrong head on right list.", (Integer)6, j.rights.get(0).i);
	}

	@Test
	public void testLeft() {
		Zipper<IntCon> j2 = j.left();
		assertEquals("Current wrong after left()", (Integer)4, j2.current.i);
		assertEquals("Left size wrong after left()", 4, j2.lefts.size());
		assertEquals("Right size wrong after left()", 8, j2.rights.size());
		assertEquals("Left head wrong after left()", (Integer)3, j2.lefts.get(0).i);
		assertEquals("Right head wrong after left()", (Integer)5, j2.rights.get(0).i);
		Zipper<IntCon> jleftmost = this.jleftmost.left();
		assertEquals("Current wrong after left on leftmost.", (Integer)5, jleftmost.current.i);
		assertEquals("Lefts size wrong after left on leftmost.", 0, jleftmost.lefts.size());
		assertEquals("Rights size wrong after left on leftmost.", 7, jleftmost.rights.size());
		assertEquals("Right head wrong after left on leftmost.", (Integer)6, jleftmost.rights.get(0).i);
		Zipper<IntCon> jrightmost = this.jrightmost.left();
		assertEquals("Current wrong after left on rightmost.", (Integer)4, jrightmost.current.i);
		assertEquals("Lefts size wrong after left on rightmost.", 4, jrightmost.lefts.size());
		assertEquals("Rights size wrong after left on rightmost.", 1, jrightmost.rights.size());
		assertEquals("Lefts head wrong after left on rightmost.", (Integer)3, jrightmost.lefts.get(0).i);
		assertEquals("Rights head wrong after left on rightmost.", (Integer)5, jrightmost.rights.get(0).i);
	}

	@Test
	public void testRight() {
		Zipper<IntCon> j = this.j.right();
		assertEquals("Current wrong after right()", (Integer)6, j.current.i);
		assertEquals("Left size wrong after right()", 6, j.lefts.size());
		assertEquals("Right size wrong after right()", 6, j.rights.size());
		assertEquals("Left head wrong after right()", (Integer)5, j.lefts.get(0).i);
		assertEquals("Right head wrong after right()", (Integer)7, j.rights.get(0).i);
		Zipper<IntCon> jleftmost = this.jleftmost.right();
		assertEquals("Current wrong after right on leftmost.", (Integer)6, jleftmost.current.i);
		assertEquals("Lefts size wrong after right on leftmost.", 1, jleftmost.lefts.size());
		assertEquals("Rights size wrong after right on leftmost.", 6, jleftmost.rights.size());
		assertEquals("Right head wrong after right on leftmost.", (Integer)7, jleftmost.rights.get(0).i);
		assertEquals("Left head wrong after right on leftmost.", (Integer)5, jleftmost.lefts.get(0).i);
		Zipper<IntCon> jrightmost = this.jrightmost.right();
		assertEquals("Current wrong after right on rightmost.", (Integer)5, jrightmost.current.i);
		assertEquals("Lefts size wrong after right on rightmost.", 5, jrightmost.lefts.size());
		assertEquals("Rights size wrong after right on rightmost.", 0, jrightmost.rights.size());
		assertEquals("Lefts head wrong after right on rightmost.", (Integer)4, jrightmost.lefts.get(0).i);
		jcenter.rights.add(new IntCon(55));
		Zipper<IntCon> jcenter = this.jcenter.right();
		assertEquals("Current wrong after right()", (Integer)55, jcenter.current.i);
		assertEquals("Left wrong size after right()", 1, jcenter.lefts.size());
		assertEquals("Right wrong size after right()", 0, jcenter.rights.size());
	}

	@Test
	public void testEqualsObject() {
		List<IntCon> ls = new ArrayList<IntCon>(5);
		for (int i = 0; i < 5; i++)
			ls.add(new IntCon(4-i));
		List<IntCon> rs = new ArrayList<IntCon>(7);
		for (int i = 6; i < 13; i++)
			rs.add(new IntCon(i));
		IntCon c = new IntCon(5);
		Zipper<IntCon> works = new Zipper<IntCon>(ls, c, rs);
		assertTrue("Identical Zipper not considered equal.", j.equals(works));
		Zipper<IntCon> noworks = new Zipper<IntCon>(rs, c, ls);
		assertFalse("Non-identical Zipper considered equal.", j.equals(noworks));
	}

	@Test
	public void testClone() {
		Zipper<IntCon> j2 = (Zipper<IntCon>)j.clone();
		assertEquals("Left length is incorrect after clone.", 5, j2.lefts.size());
		assertEquals("Right length is incorrect after clone.", 7, j2.rights.size());
		assertEquals("Current value is wrong after clone.", (Integer)5, j2.current.i);
		j2.lefts.remove(0);
		assertEquals("Original is changed by modifying clone.", 5, j.lefts.size());
	}

	@Test
	public void testToString() {
		assertEquals("Wrong string returned for encoding of zipper.", "Zipper([0,1,2,3,4],5,[6,7,8,9,10,11,12])", j.toString().replaceAll("\\s+",""));
	}
	

}
