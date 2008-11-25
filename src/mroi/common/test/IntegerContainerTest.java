package mroi.common.test;

import static org.junit.Assert.*;
import org.junit.Test;

public class IntegerContainerTest {

	@Test
	public void testEquals() {
		assertEquals("Equality fails", new IntegerContainer(5), new IntegerContainer(5));
	}
	
	@Test
	public void testWriting() {
		assertEquals("Doesn't produce right output from toString()", "5", (new IntegerContainer(5)).toString());
	}
	
	@Test
	public void testReading() throws Exception {
		assertEquals("Reading isn't right.", new IntegerContainer(5), (new IntegerContainer(3)).read("5"));
	}
}
