package mroi.common.test;


import mroi.common.LispToLists;
import mroi.common.LispToLists.LispList;
import mroi.common.LispToLists.LispNode;
import mroi.common.LispToLists.LispString;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import java.util.*;
import java.io.StringReader;

public class LispToListsTest {
	LispToLists.LispNode a;
	
	@Before
	public void setUp() throws Exception {
		LinkedList<LispToLists.LispNode> a = new LinkedList<LispToLists.LispNode>();
		a.add(new LispToLists.LispString("alpha"));
		a.add(new LispToLists.LispString("beta"));
		a.add(new LispToLists.LispString("gamma"));
		this.a = new LispToLists.LispList(a);
	}

	@After
	public void tearDown() throws Exception {
	}
	
	
	@Test
	public void testSimpleLists() throws Exception {
		assertEquals("One level lists with only atoms.", a.toString(), new LispToLists(new StringReader("(alpha beta gamma)")).read().toString());
		assertEquals("Empty list", "()", new LispToLists(new StringReader("()")).read().toString());
		assertEquals("One element", "(a)", new LispToLists(new StringReader("(a)")).read().toString());
	}
	
	@Test
	public void testLayeredLists() throws Exception {
		String s = "(alpha (beta (gamma delta) epsilon) eta)"; 
		LispToLists.LispNode a = new LispToLists(new StringReader(s)).read();
		assertEquals("Three level list with only atoms.", s, a.toString());
	}
	
	@Test
	public void testLayeredWithStrings() throws Exception {
		String s1 = "((\"Boris and Natasha\" hildebrand) hvord)";
		assertEquals("With strings", s1, new LispToLists(new StringReader(s1)).read().toString());
	}
}
