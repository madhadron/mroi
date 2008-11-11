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

import java.awt.AWTEvent;
import java.util.*;

import mroi.*;

public abstract class MutatingCommand<T> implements Command<T> {

	public Zipper<Map<Integer,MZipper<T>>> exec(Zipper<Map<Integer,MZipper<T>>> z, int frame) {
		HashMap<Integer,MZipper<T>> n = new HashMap<Integer,MZipper<T>>(z.current);
		n.put(frame, operation(n.get(frame)));
		z.rights.clear();
		z.rights.add(n);
		z = z.right();
		return z;
	}

	public abstract boolean isInvoked(String lbl);
	public abstract MZipper<T> operation(MZipper<T> mz);
}
