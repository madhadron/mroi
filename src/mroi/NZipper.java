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

package mroi;

import java.util.*;

public class NZipper<T> implements MZipper<T> {
	public List<T> lefts, rights;
	public NZipper(List<T> ls, List<T> rs) {
		if (ls == null)
			lefts = new LinkedList<T>();
		else 
			lefts = new LinkedList<T>(ls);
		if (rs == null)
			rights = new LinkedList<T>();
		else
			rights = new LinkedList<T>(rs);
	}
	
	
	public JZipper<T> add(T r) {
		return new JZipper<T>(lefts, r, rights);
	}

	public int size() {
	    return(lefts.size() + rights.size());
	}
	
	public NZipper<T> delete() {
		return this;
	}
	public MZipper<T> select(Predicate<T> p) {
		int lsize = lefts.size()+rights.size();
		ArrayList<T> l = new ArrayList<T>(lsize);
		ArrayList<T> tmp = new ArrayList<T>(lefts);
		Collections.reverse(tmp);
		l.addAll(tmp);
		l.addAll(rights);
		for (int i = 0; i < lsize; i++) {
			if (p.apply(l.get(i))) {
				List<T> k = l.subList(0, i);
				Collections.reverse(k);
				return new JZipper<T>(k, l.get(i), l.subList(i+1, lsize));
			}
		}
		List<T> k2 = l.subList(0, lefts.size());
		Collections.reverse(k2);
		return new NZipper<T>(k2, l.subList(lefts.size(), lsize));
	}
	
	private boolean listsEqual(List<T> a, List<T> b) {
		boolean lequals = true;
		if (a.size() == b.size()) {
			for (int i = 0; i < a.size(); i++) {
				if (!a.get(i).equals(b.get(i)))
					lequals = false;
			}
		} else
			lequals = false;
		return lequals;
	}
	
	public boolean equals(Object z) {
		if (z instanceof NZipper) {
			NZipper<T> z2 = (NZipper<T>)z;
			return listsEqual(lefts, z2.lefts) && listsEqual(rights, z2.rights);
		} else {
			return false;
		}
	}
	
	@Override
	public NZipper<T> clone() {
		return new NZipper<T>(lefts, rights);
	}
	
	@Override
	public String toString() {
		List<T> ls = new ArrayList<T>(lefts);
		Collections.reverse(ls);
		return "NZipper("+ls.toString()+","+rights.toString()+")";
	}
	
	public List<T> asListWithoutCurrent() {
		List<T> ls = new LinkedList<T>(lefts);
		Collections.reverse(ls);
		ls.addAll(rights);
		return ls;
	}

	public List<T> asList() {
		List<T> ls = new LinkedList<T>(lefts);
		Collections.reverse(ls);
		ls.addAll(rights);
		return ls;
	}

}
