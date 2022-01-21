package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo;

class UrlUtilsTest {

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

//	@Test
//	final void testParse_classCast() {
//		String baseUrl = "https://data.ncaa.com/casablanca/game/5851535/pbp.json";
//		Assertions.assertThrows(IOException.class, () -> {
//			UrlUtils.parse(baseUrl);
//		});	
//
//	}
//	
//	@Test
//	final void testParse_malformed() {
//		String baseUrl = "testurl";
//		Assertions.assertThrows(MalformedURLException.class, () -> {
//			UrlUtils.parse(baseUrl);
//		});	
//
//	}
	
	@Test
	final void testParse_valid() {
		String baseUrl = "ww/.google.com";
				
		Assertions.assertDoesNotThrow(() -> {
			UrlUtils.parse(baseUrl);
		});
	}

	@Test
	final void testGet_valid() {
		String uri = "https://data.ncaa.com/casablanca/game/5851535/pbp.json";
		@SuppressWarnings("rawtypes")
		Class target = PlayByPlayPojo.class;
		Assertions.assertDoesNotThrow(() -> {
			UrlUtils.get(uri, target);
		});
	}
	
	@Test
	final void testGet_fail() {
		String uri = "htasdfas://data.ncaa.com/casablanca/game/5851535/pbp.json";
		@SuppressWarnings("rawtypes")
		Class target = PlayByPlayPojo.class;
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			UrlUtils.get(uri, target);
		});
	}

}
