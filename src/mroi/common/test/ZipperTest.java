/**
 * 
 */
package mroi.common.test;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import mroi.common.Zipper;


/**
 * @author ross
 *
 */
public class ZipperTest {

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	
	@Test
	public void testRead() throws Exception {
		String s1 = "((3 2 1) 4 (4 5 6))";
		Zipper<IntegerContainer> z1 = new Zipper<IntegerContainer>(s1);
		assertEquals("Read works on Zipper.", s1, z1.toString());
		
	}
	
}
