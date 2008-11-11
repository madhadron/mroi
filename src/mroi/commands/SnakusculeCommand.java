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
import ij.ImagePlus;
import big.ij.snake2D.Snake2D;
import big.ij.snake2D.Snake2DNode;
import com.vividsolutions.jts.geom.*;

public class SnakusculeCommand extends SnakeCommand<MySnakuscule> {
    static public GeometryFactory gfact = new GeometryFactory();

    public SnakusculeCommand(ImagePlus imp) {
	super(imp);
    }

    @Override public boolean isInvoked(String lbl) {
	return lbl.equalsIgnoreCase("snakuscule");
    }

    @Override public MySnakuscule roiToSnake(Geometry g) {
	MySnakuscule snake = new MySnakuscule(this.imp.getProcessor());
	Point centroid = g.getCentroid();
	double cx = centroid.getX();
	double cy = centroid.getY();
	double area = g.getArea();
	double radius = Math.sqrt(area / Math.PI);
	Snake2DNode[] nodes = new Snake2DNode[2];
	nodes[0] = new Snake2DNode(cx - radius, cy);
	nodes[1] = new Snake2DNode(cx + radius, cy);
	snake.setNodes(nodes);
	return snake;
    }

    @Override public Geometry snakeToRoi(MySnakuscule snake) {
	Snake2DNode[] nodes = snake.getNodes();
	double x1 = nodes[0].x;
	double y1 = nodes[0].y;
	double x2 = nodes[1].x;
	double y2 = nodes[1].y;
	double cx = (x1 + x2) / 2.0;
	double cy = (y1 + y2) / 2.0;
	double radius = Math.sqrt( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) ) / 2.0;
	int N = (int)(2 * Math.PI * radius / 10.0);
	double theta;
	Coordinate[] coords = new Coordinate[N+1];
	for (int i = 0; i < N; i++) {
	    theta = 2 * Math.PI * i / (double)N;
	    coords[i] = new Coordinate(cx + radius*Math.cos(theta),
				       cy + radius*Math.sin(theta));
	}
	coords[N] = new Coordinate(cx+radius, cy);
	return gfact.createPolygon(gfact.createLinearRing(coords), null);
    }
}