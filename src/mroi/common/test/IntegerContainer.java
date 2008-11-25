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
package mroi.common.test;

import mroi.common.Readable;
import mroi.common.LispToLists;

public class IntegerContainer implements Cloneable,Readable {
	public Integer i;
	
	public IntegerContainer(int x) {
		i = x;
	}
	
	@Override
	public IntegerContainer clone() {
		return new IntegerContainer(i);
	}
	
	@Override
	public boolean equals(Object e) {
		if (!e.getClass().equals(IntegerContainer.class)) 
			return false;
		else
			return i==((IntegerContainer)e).i;
	}
	
	@Override
	public String toString() {
		return i.toString();
	}
	
	public IntegerContainer read(String s) throws Exception {
		return new IntegerContainer(Integer.decode(s));
	}
	
	public IntegerContainer read(LispToLists.LispNode s) throws Exception {
		if (s instanceof LispToLists.LispString) {
			return(new IntegerContainer(Integer.decode(((LispToLists.LispString)s).val)));
		} else {
			throw new Exception("IntegerContainer can't be built from list!");
		}
	}
}

