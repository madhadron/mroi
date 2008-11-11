package mroi.commands;

import java.util.* ;
import com.vividsolutions.jts.geom.*;

public class Singleton{
	
	public static boolean state = false;
	public static Singleton instance;
	public ArrayList<Geometry> clipboard = new ArrayList<Geometry>();
	
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
	

	public Geometry copyInstance(Geometry x)
	{	
		clipboard.add(x);
		return clipboard.get(0);
	}
	public void copyInstance(List<Geometry> x)
	{
		clipboard.addAll(x);
	}
	
	
	public List<Geometry> pasteInstance()
	{
		return clipboard;
	}
	public void clear()
	{
		clipboard.removeAll(clipboard);	
	}
	
}

