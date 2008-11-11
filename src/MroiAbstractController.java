import java.awt.Graphics;
import java.awt.Rectangle;

import mroi.NoSuchCommandException;

import ij.ImagePlus;

public interface MroiAbstractController {
	public void goToFrameOn(Integer slice, ImagePlus imp);

	public void paint(Graphics g, Rectangle visibleWindow, double magnification);

	public void mousePressedOnAt(ImagePlus imp, Integer x, Integer y,
			int button, int modifiers);

	public void executeCommandOn(String lbl, ImagePlus imp)
			throws NoSuchCommandException;
}
