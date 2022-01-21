package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ParsingUtilsTest {

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
	}

	@AfterEach
	void tearDown() throws Exception {
	}
	
	@Test
	final void testParseString() {
		String val = "1";
		Integer expected = 1;
		Integer actual = ParsingUtils.parseString(val);
		Assertions.assertEquals(expected, actual);
	}
	
	@Test
	final void testParseString_empty() {
		String val = "";
		Integer expected = 0;
		Integer actual = ParsingUtils.parseString(val);
		Assertions.assertEquals(expected, actual);
	}
	
	@Test
	final void testParseString_fail() {
		String val = "not an integer";		
		Assertions.assertThrows(RuntimeException.class, () -> {
			ParsingUtils.parseString(val);
		});
	}

	@Test
	final void testParseStringToDouble() {
		String val = "1";
		Double expected = 1.0;
		Double actual = ParsingUtils.parseStringToDouble(val);
		Assertions.assertEquals(expected, actual);
	}
	
	@Test
	final void testParseStringToDouble_empty() {
		String val = "";
		Double expected = 0.0;
		Double actual = ParsingUtils.parseStringToDouble(val);
		Assertions.assertEquals(expected, actual);
	}
	
	@Test
	final void testParseStringToDouble_invalid() {
		String val = "not an integer";		
		Assertions.assertThrows(RuntimeException.class, () -> {
			ParsingUtils.parseStringToDouble(val);
		});
	}

	@Test
	final void testSplitValue() {
		Integer expected = 2;
		String value = "2-1";
		String delim = "-";
		Integer index = 0;
		Integer actual = ParsingUtils.splitValue(value, delim, index);
		Assertions.assertEquals(expected, actual);
	}
	
	@Test
	final void testSplitValue_empty() {
		Integer expected = 0;
		String value = "-";
		String delim = "-";
		Integer index = 1;
		Integer actual = ParsingUtils.splitValue(value, delim, index);
		Assertions.assertEquals(expected, actual);
	}
	
	@Test
	final void testSplitValue_negative() {
		Integer expected = -3;
		String value = "2--3";
		String delim = "-";
		Integer index = 1;
		Integer actual = ParsingUtils.splitValue(value, delim, index);
		Assertions.assertEquals(expected, actual);
	}

}
