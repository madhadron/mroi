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
import java.awt.event.KeyEvent;

import mroi.MZipper;
import mroi.Zipper;
import java.util.*;

public class Undo<T> implements Command<T> {

	public Zipper<Map<Integer,MZipper<T>>> exec(Zipper<Map<Integer,MZipper<T>>> z, int frame) {
		Zipper<Map<Integer,MZipper<T>>> z2 = z.left();
		z.current = z2.current;
		z.lefts = z2.lefts;
		z.rights = z2.rights;
		return z;
	}

	public boolean isInvoked(String lbl) {
		return lbl.equalsIgnoreCase("undo");
	}

	public MZipper<T> operation(MZipper<T> mz) {
		return null;
	}

}
