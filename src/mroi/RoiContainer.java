package mroi;

import com.vividsolutions.jts.geom.*;
import static mroi.geometry.GeometryUtilities.*;
import ij.gui.Roi;

public class RoiContainer {
	static int counter = 1;
	Geometry roi;
	public Integer id;
	public RoiContainer predecessor;
	
	public RoiContainer(Integer id, Geometry roi, RoiContainer pred) {
		this.roi = roi;
		this.id = id;
		this.predecessor = pred;
	}
	public RoiContainer(Integer id, Geometry roi) {
		this(id, roi, null);
	}
	public RoiContainer(Geometry roi) {
		this(counter++, roi, null);
	}
	
	public Geometry getGeometry() { return roi; }
	public Roi getRoi() { return geomToRoi(roi); }
	public Geometry setGeometry(Geometry g) { this.roi = g; return g; }
	public Roi setRoi(Roi r) { this.roi = roiToGeom(r); return r; }
	
	public Integer getPredecessorId() {
		if (predecessor != null) {
			return predecessor.id;
		} else {
			return null;
		}
	}
	
	public RoiContainer setPredecessor(RoiContainer pr) {
		this.predecessor = pr;
		return pr;
	}
	
	public String toString() {
		return("(" + id + " \"" + roi.toString() + "\" " + getPredecessorId() + ")");
	}

}
