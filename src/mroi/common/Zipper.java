package mroi.common;

import java.util.*;
import java.io.StringReader;
import static mroi.common.LispToLists.LispNode;
import static mroi.common.LispToLists.LispString;
import static mroi.common.LispToLists.LispList;


public class Zipper<T extends Readable> {
	List<T> lefts;
	T current;
	List<T> rights;
	
	public Zipper() {
		lefts = new ArrayList<T>();
		current = null;
		rights = new ArrayList<T>();
	}
	
	public Zipper(LispList ls) throws Exception {
		if (((LispList)ls).val.size() != 3 || 
				((LispList)ls).val.get(0) instanceof LispString || 
				((LispList)ls).val.get(1) instanceof LispList || 
				((LispList)ls).val.get(2) instanceof LispString) {
			throw new Exception("Improper length for zipper.");
		}
		lefts = new ArrayList<T>();
		rights = new ArrayList<T>();
		for (LispNode a : ((LispList) ((LispList)ls).val.get(0)).val) {
			lefts.add((T)(current.read(a)));
		}
		
		for (LispNode a : ((LispList) ((LispList)ls).val.get(0)).val) {
			lefts.add((T)(current.read(a)));
		}

	}
	
	public Zipper(String s) throws Exception {
		this((LispList)(new LispToLists(new StringReader(s)).read()));
	}
}
