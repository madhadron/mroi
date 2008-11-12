package mroi.commands;

import java.util.* ;
import mroi.RoiContainer;
import com.vividsolutions.jts.geom.*;

public class Singleton{
	
	public static boolean state = false;
	public static Singleton instance;
	public ArrayList<RoiContainer> clipboard = new ArrayList<RoiContainer>();
	
	private Singleton(){
		state = true; 
	}
	public static Singleton getInstance()
	{
		if (state == false)					
			 {
			instance = new Singleton();	
			 	state = true;
			 }
		return instance;
	}
	

	public RoiContainer copyInstance(RoiContainer x)
	{	
		clipboard.add(x);
		return clipboard.get(0);
	}
	public void copyInstance(List<RoiContainer> x)
	{
		clipboard.addAll(x);
	}
	
	
	public List<RoiContainer> pasteInstance()
	{
		return clipboard;
	}
	public void clear()
	{
		clipboard.removeAll(clipboard);	
	}
	
}

