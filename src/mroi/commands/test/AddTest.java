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
package mroi.commands.test;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import mroi.*;
import mroi.commands.*;

import java.util.*;

import mroi.test.*;

public class AddTest {
	Zipper<Map<Integer,MZipper<IntCon>>> urm;
	Add<IntCon> m;
	@Before
	public void setUp() throws Exception {
		List<IntCon> ls = new ArrayList<IntCon>(5);
		for (int i = 0; i < 5; i++)
			ls.add(new IntCon(4-i));
		List<IntCon> rs = new ArrayList<IntCon>(7);
		for (int i = 6; i < 13; i++)
			rs.add(new IntCon(i));
		IntCon c = new IntCon(5);
		JZipper<IntCon> j = new JZipper<IntCon>(ls, c, rs);
		urm = new Zipper<Map<Integer,MZipper<IntCon>>>(null, new HashMap<Integer,MZipper<IntCon>>(), null);
		urm.current.put(1, j);
		m = new Add<IntCon>(new IntCon(55));
	}

	@Test
	public void testOperation() {
		Zipper<Map<Integer,MZipper<IntCon>>> urm2 = m.exec(urm, 1);
		assertEquals("Lefts of urm2 wrong length after add.", 1, urm2.lefts.size());
		assertEquals("Rights of urm2 wrong length after add.", 0, urm2.rights.size());
		assertEquals("Current of urm2 wrong number.", (Integer)55, ((JZipper<IntCon>)urm2.current.get(1)).current.i);
		assertEquals("Left head of urm2 wrong number.", (Integer)5, ((JZipper<IntCon>)urm2.current.get(1)).lefts.get(0).i);
		assertEquals("Previous frame current wrong number.", (Integer)5, ((JZipper<IntCon>)urm2.lefts.get(0).get(1)).current.i);
	}

}
