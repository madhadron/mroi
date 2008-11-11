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

import mroi.MZipper;

public class Add<T> extends MutatingCommand<T> {
	T r;
	public Add(T r) {
		this.r = r;
	}
	
	@Override
	public boolean isInvoked(String lbl) {
		return false;
	}

	@Override
	public MZipper<T> operation(MZipper<T> mz) {
		return mz.add(r);
	}

}
