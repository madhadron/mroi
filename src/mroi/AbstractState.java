package mroi;
import java.awt.Graphics;
import java.awt.Rectangle;
import com.vividsolutions.jts.geom.*;


import ij.ImagePlus;
import ij.gui.Roi;

public interface AbstractState {
	public void goToFrameOn(Integer slice, ImagePlus imp);

	public void paint(Graphics g, Rectangle visibleWindow, double magnification);

	public void executeCommandOn(String lbl, ImagePlus imp)
			throws NoSuchCommandException;

	public void syncRoiFrom(ImagePlus imp);
	public void syncRoiTo(ImagePlus imp);
	public Geometry selectAt(Integer x, Integer y);
	public Geometry fetchCurrentRoiAsGeometry();
	public Roi fetchCurrentRoiAsRoi();
	public void togglePreviousFrameVisible();
}
