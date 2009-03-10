package mroi.commands;

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
import mroi.*;
import java.awt.AWTEvent;
import java.util.*;


/* For Kawa commands, it's easier to go ahead and put RoiContainer in place and not worry about
   the generics.  This is just an abstract class implementing Command for RoiContainer. */
public abstract class RoiContainerCommand implements Command<RoiContainer> {
	public abstract boolean isInvoked(String lbl);
	public abstract MZipper<RoiContainer> operation(MZipper<RoiContainer> mz);
	public abstract Zipper<Map<Integer,MZipper<RoiContainer>>> exec(Zipper<Map<Integer,MZipper<RoiContainer>>> z, int frame);

}