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
package mroi.io;

import java.util.*;
import java.util.regex.*;

import mroi.*;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.*;

import java.io.*;

public class MroiListReader {
//	Pattern file = Pattern.compile("{(\\d+=.+?,)*}");
//	Pattern slice = Pattern.compile("^\\s*(\\d+)\\s*=\\s*(.+)\\s*$");
//	Pattern jzipper = Pattern.compile("JZipper(\\s*\\[(.+?)\\]\\s*,\\s*(.+?),\\s*[(.+?)\\]\\s*)");
//	Pattern nzipper = Pattern.compile("JZipper(\\s*\\[(.+?)\\]\\s*,\\s*[(.+?)\\]\\s*)");
//	Pattern list = Pattern.compile("^\\s*(?:([A-Z]+\\([\\(\\)\\s0-9]\\)\\s*)s*");
	public Map<Integer,MZipper<RoiContainer>> read(BufferedReader in) throws IOException, MalformedGeometryFileException {
		StringBuffer b = new StringBuffer();
		String t = in.readLine();
		while (t != null) {
			b.append(t);
			t = in.readLine();
		}
		String normalized = b.toString().replaceAll("\\s*([\\(\\)\\[\\]\\{\\}\\,=])\\s*", "$1"); // Remove whitespace around punctuation
		
		
		Map<Integer,MZipper<RoiContainer>> result = parseMap(normalized);

		return result;
	}

	public Map<Integer,MZipper<RoiContainer>> parseMap(String input) throws MalformedGeometryFileException {
		Map<Integer,MZipper<RoiContainer>> result = new HashMap<Integer,MZipper<RoiContainer>>();
		String in = input.substring(input.indexOf('{')+1, input.indexOf('}'));
		if (in.length() == 0) return result;
		try {
			while (in.indexOf('=') != -1) {
				int equalsPosition = in.indexOf('=');
				String digitChars = in.substring(0, equalsPosition);
				int slice = Integer.parseInt(digitChars);
				
				int nextEqualsPosition = in.substring(equalsPosition+1).indexOf('=');
				if (nextEqualsPosition != -1) nextEqualsPosition+= equalsPosition+1;
				MZipper<RoiContainer> entry;
				if (nextEqualsPosition == -1) {
					entry = parseZipper(in.substring(equalsPosition+1));
					in = "";
				} else {
					int endingCommaPosition = nextEqualsPosition-1;
					while (Character.isDigit(in.charAt(endingCommaPosition)))
						endingCommaPosition = endingCommaPosition - 1;
					String zipperSection = in.substring(equalsPosition+1, endingCommaPosition);
					in = in.substring(endingCommaPosition+1);
					entry = parseZipper(zipperSection);
				}
				result.put(slice, entry);
			}
		} catch (NumberFormatException e) {
			throw new MalformedGeometryFileException("Bad slice number: " + in.substring(0, in.indexOf('=')));
		}
		return result;
	}

	public MZipper<RoiContainer> parseZipper(String input) throws MalformedGeometryFileException {
		if (input.startsWith("JZipper")) {
			return parseJZipper(input);
		} else if (input.startsWith("NZipper")) {
			return parseNZipper(input);
		} else {
			throw new MalformedGeometryFileException("Cannot parse as an MZipper:" + input);
		}
	}
	
	public JZipper<RoiContainer> parseJZipper(String input) throws MalformedGeometryFileException {
		int leftSquareOpening = input.indexOf('[');
		int leftSquareClosing = input.indexOf(']');
		String rightInput = input.substring(leftSquareClosing+1);
		int rightSquareOpening = rightInput.indexOf('[');
		int rightSquareClosing = rightInput.indexOf(']');
		List<RoiContainer> lefts = parseList(input.substring(leftSquareOpening, leftSquareClosing+1));
		Collections.reverse(lefts);
		List<RoiContainer> rights = parseList(rightInput.substring(rightSquareOpening, rightSquareClosing+1));
		Geometry current;
		try {
			current = (new WKTReader()).read(input.substring(leftSquareClosing+2, leftSquareClosing+rightSquareOpening));
		} catch (ParseException e) {
			throw new MalformedGeometryFileException("Couldn't parse Geometry: " + e.getMessage());
		}
		return new JZipper<RoiContainer>(lefts, new RoiContainer(current), rights);
	}
	
	public NZipper<RoiContainer> parseNZipper(String input) throws MalformedGeometryFileException {
		int leftSquareOpening = input.indexOf('[');
		int leftSquareClosing = input.indexOf(']');
		String rightInput = input.substring(leftSquareClosing+1);
		int rightSquareOpening = rightInput.indexOf('[');
		int rightSquareClosing = rightInput.indexOf(']');
		List<RoiContainer> lefts = parseList(input.substring(leftSquareOpening, leftSquareClosing+1));
		Collections.reverse(lefts);
		List<RoiContainer> rights = parseList(rightInput.substring(rightSquareOpening, rightSquareClosing+1));
		return new NZipper<RoiContainer>(lefts, rights);
	}
	
	public List<RoiContainer> parseList(String input) throws MalformedGeometryFileException {
		if (input.equals("[]")) {
			return new ArrayList<RoiContainer>(0);
		}
		String[] bits = input.substring(input.indexOf('[')+1, input.indexOf(']')).split("\\),");
		WKTReader rr = new WKTReader();
		LinkedList<RoiContainer> ls = new LinkedList<RoiContainer>();
		try {
			for (int i = 0; i < bits.length; i++) {
				ls.add(new RoiContainer(rr.read(bits[i]+")")));
			}
		} catch (ParseException e) {
			throw new MalformedGeometryFileException("Could not parse Geometry: " + e.getMessage());
		}
		return ls;
	}
}
