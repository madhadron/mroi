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

import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

import mroi.*;
import mroi.commands.*;
import mroi.test.IntCon;
import java.util.*;

public class SingleRoiMutatingCommandTest {
    class AddOneToCurrent extends SingleRoiMutatingCommand<IntCon> {
	@Override public IntCon singleRoiOperation(IntCon z) {
	    return new IntCon(z.i + 1);
	}

	@Override public boolean isInvoked(String e) { return true; }
    }
    
    SingleRoiMutatingCommand<IntCon> m = new AddOneToCurrent();
    Zipper<Map<Integer,MZipper<IntCon>>> urm;
    
    @Before public void setUp() throws Exception {
	List<IntCon> ls = new ArrayList<IntCon>(5);
	for (int i = 0; i < 5; i++)
	    ls.add(new IntCon(4-i));
	List<IntCon> rs = new ArrayList<IntCon>(7);
	for (int i = 6; i < 13; i++)
	    rs.add(new IntCon(i));
	IntCon c = new IntCon(5);
	JZipper<IntCon> j = new JZipper<IntCon>(ls, c, rs);
	urm = new Zipper<Map<Integer,MZipper<IntCon>>>(null, 
		   new HashMap<Integer,MZipper<IntCon>>(), null);
	urm.current.put(1, j);
	
    }

    @Test public void testExec() {
	Zipper<Map<Integer,MZipper<IntCon>>> urm2 = m.exec(urm,1);
	assertEquals("Wrong left size after exec.", 1, urm2.lefts.size());
	assertEquals("Wrong right size after exec.", 0, urm2.rights.size());
	assertEquals("Not 6 in current after exec.", (Integer)6, ((JZipper<IntCon>)urm2.current.get(1)).current.i);
	assertEquals("Lefts length is changed.", 5, ((JZipper<IntCon>)urm2.current.get(1)).lefts.size());
	assertEquals("Rights length is changed.", 7, ((JZipper<IntCon>)urm2.current.get(1)).rights.size());
	assertEquals("Incorrect undo frame after exec.", (Integer)5, ((JZipper<IntCon>)urm2.lefts.get(0).get(1)).current.i);
    }

    @Test public void testSingleRoiOperation() {
	assertEquals("Doesn't add 1 properly in operation.", (Integer)5, (m.singleRoiOperation(new IntCon(4))).i);
    }
}
