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

import mroi.*;
import com.vividsolutions.jts.geom.*;
import ij.ImagePlus;
import big.ij.snake2D.Snake2D;
import big.ij.snake2D.Snake2DKeeper;
import big.ij.snake2D.Snake2DNode;
import big.ij.snake2D.Snake2DScale;

public abstract class SnakeCommand<T extends Snake2D> extends MutatingCommand<Geometry> {
    public final Snake2DKeeper keeper;
    public final ImagePlus imp;

    public SnakeCommand(ImagePlus imp) {
	this.keeper = new Snake2DKeeper();
	this.imp = imp;
    }

    public abstract T roiToSnake(Geometry g);
    public abstract Geometry snakeToRoi(T snake);

    public MZipper<Geometry> operation(MZipper<Geometry> mz) {
	if (mz instanceof JZipper) {
	    T snake = roiToSnake(((JZipper<Geometry>)mz).current);
	    keeper.interactAndOptimize(snake, imp);
	    return new JZipper<Geometry>(((JZipper<Geometry>)mz).lefts, 
					 snakeToRoi(snake),
					 ((JZipper<Geometry>)mz).rights);
	} else {
	    return mz;
	}
    }
}