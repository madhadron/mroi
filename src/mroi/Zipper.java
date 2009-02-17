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
/**
 * @author fross
 *
 */
public class Zipper<T> {
	public LinkedList<T> lefts, rights;
	public T current;
	
	public Zipper(List<T> ls, T c, List<T> rs) {
		if (ls == null)
			lefts = new LinkedList<T>();
		else 
			lefts = new LinkedList<T>(ls);
		if (rs == null)
			rights = new LinkedList<T>();
		else
			rights = new LinkedList<T>(rs);
		current = c;
	}
	
	public Zipper<T> insertAndStep(T x) {
		rights.clear();
		rights.add(x);
		return right();
	}
	
	public Zipper<T> left() {
		if (lefts.size() > 0) {
			LinkedList<T> newRights = new LinkedList<T>(rights);
			newRights.addFirst(current);
			LinkedList<T> newLefts = new LinkedList<T>(lefts);
			T newCurrent = newLefts.removeFirst();
			return new Zipper<T>(newLefts, newCurrent, newRights);
		} else {
			return this.clone();
		}
		
	}
	
	public Zipper<T> right() {
		if (rights.size() > 0) {
			LinkedList<T> newLefts = new LinkedList<T>(lefts);
			newLefts.addFirst(current);
			LinkedList<T> newRights = new LinkedList<T>(rights);
			T newCurrent = newRights.removeFirst();
			return new Zipper<T>(newLefts, newCurrent, newRights);
		} else {
			return this.clone();
		}
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
	
	@Override
	public boolean equals(Object z) {
		if (z instanceof Zipper) {
			Zipper<T> z2 = (Zipper<T>)z;
			return z2.current.equals(current) && listsEqual(lefts, z2.lefts) && listsEqual(rights, z2.rights);
		} else {
			return false;
		}
	}
	
	@Override
	public Zipper<T> clone() {
		return new Zipper<T>(lefts, current, rights);
	}
	
	@Override
	public String toString() {
		List<T> ls = new ArrayList<T>(lefts);
		Collections.reverse(ls);
		return "Zipper("+ls.toString()+","+current.toString()+","+rights.toString()+")";
	}
	

}
