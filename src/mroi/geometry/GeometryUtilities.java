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

package mroi.geometry;

import ij.gui.PolygonRoi;
import ij.gui.Roi;
import ij.ImagePlus;
import ij.process.ImageProcessor;
import java.util.*;
import java.lang.reflect.Array;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;

public class GeometryUtilities {
	static public GeometryFactory gfact = new GeometryFactory();

	public static Geometry roiToGeom(Roi r) {
		java.awt.Polygon p = sparsifyPolygon(r.getPolygon(), 5);
		if (p.npoints < 4) p = r.getPolygon();
		if (r.getPolygon().npoints < 4) return null;
		/* java.awt.Polygon p = r.getPolygon(); */
		Coordinate[] cs = (Coordinate[]) Array.newInstance(Coordinate.class,
				p.npoints + 1);
		for (int i = 0; i < p.npoints; i++) {
			cs[i] = new Coordinate(p.xpoints[i], p.ypoints[i]);
		}
		cs[p.npoints] = new Coordinate(p.xpoints[0], p.ypoints[0]);
		try {
			Geometry g = gfact.createPolygon(gfact.createLinearRing(cs), null);
			return g;
		} catch (Exception e) {
			return null;
		}
	}

	public static Roi geomToRoi(Geometry g) {
		Coordinate[] cs = g.getCoordinates();
		int n = cs.length - 1;
		int[] xs = new int[n];
		int[] ys = new int[n];
		for (int i = 0; i < n; i++) {
			xs[i] = (int) cs[i].x;
			ys[i] = (int) cs[i].y;
		}
		return new PolygonRoi(xs, ys, n, PolygonRoi.POLYGON);
	}

	public static java.awt.Polygon sparsifyPolygon(java.awt.Polygon p,
			double minDistance) {
		int n = p.npoints;
		int[] xs = p.xpoints;
		ArrayList<Integer> nxs = new ArrayList<Integer>(n);
		int[] ys = p.ypoints;
		ArrayList<Integer> nys = new ArrayList<Integer>(n);
		nxs.add(xs[0]);
		nys.add(ys[0]);
		int px = xs[0];
		int py = ys[0];
		int qx, qy;
		double dist;
		for (int i = 1; i < n; i++) {
			qx = xs[i];
			qy = ys[i];
			dist = Math.sqrt((qx - px) * (qx - px) + (qy - py) * (qy - py));
			if (dist > minDistance) {
				nxs.add(qx);
				nys.add(qy);
				px = qx;
				py = qy;
			}
		}
		dist = Math.sqrt((px - xs[0]) * (px - xs[0]) + (py - ys[0])
				* (py - ys[0]));
		if (dist <= minDistance) {
			nxs.set(nxs.size() - 1, xs[0]);
			nys.set(nys.size() - 1, ys[0]);
		} else {
			nxs.add(xs[0]);
			nys.add(ys[0]);
		}
		int[] anxs = new int[nxs.size()];
		int[] anys = new int[nys.size()];
		for (int i = 0; i < nxs.size(); i++) {
			anxs[i] = nxs.get(i);
			anys[i] = nys.get(i);
		}
		return new java.awt.Polygon(anxs, anys, nxs.size());
	}

	public static PolygonRoi sparsifyRoi(PolygonRoi r, double minDistance) {
		return new PolygonRoi(sparsifyPolygon(r.getPolygon(), minDistance),
				Roi.POLYGON);
	}
	
	public static double meanIntensityOfOn(Geometry g, ImagePlus imp) {
		Envelope env = g.getEnvelopeInternal();
		if (env == null) return 0;
		
		int minX = (int)Math.max(Math.floor(env.getMinX()), 0);
		int maxX = (int)Math.min(Math.ceil(env.getMaxX()), imp.getWidth());
		int minY = (int)Math.max(Math.floor(env.getMinY()), 0);
		int maxY = (int)Math.min(Math.ceil(env.getMaxY()), imp.getHeight());
		ImageProcessor ip = imp.getProcessor();
		double res = 0;
		int N = 0;
		for (int i = minX; i <= maxX; i++) {
			for (int j = minY; j <= maxY; j++) {
				if (g.contains(gfact.createPoint(new Coordinate(i,j)))) {
					res += ip.get(i,j);
					N++;
				}
			}
		}
		return res/(double)N;
	}

}

/*
 * (head qs) : sparsify (tail qs) where qs = dropWhile (\x -> dist p x < 4)
 */