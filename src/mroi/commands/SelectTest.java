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

import static org.junit.Assert.*;
import mroi.test.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import mroi.JZipper;
import mroi.MZipper;
import mroi.Zipper;
import mroi.test.IntCon;

import org.junit.Before;
import org.junit.Test;

public class SelectTest {
	Zipper<Map<Integer,MZipper<IntCon>>> urm;
	Select<IntCon> m;
	
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
		m = new Select<IntCon>(new IntegerEquality(2));
	}

	@Test
	public void testOperation() {
		Zipper<Map<Integer,MZipper<IntCon>>> urm2 = m.exec(urm, 1);
		assertEquals("Doesn't move undo/redo stack", 0, urm2.lefts.size());
		assertEquals("Correct current.", (Integer)2, ((JZipper<IntCon>)urm2.current.get(1)).current.i);
	}

}
