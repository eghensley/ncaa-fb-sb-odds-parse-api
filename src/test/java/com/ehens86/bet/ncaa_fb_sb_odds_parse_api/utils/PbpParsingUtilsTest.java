/**
 * 
 */
package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayTeamPojo;

/**
 * @author eric.hensley@ibm.com
 *
 */
class PbpParsingUtilsTest {

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeEach
	void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterEach
	void tearDown() throws Exception {
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertMinSecToSec(java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum)}.
	 */
	@Test
	final void testConvertMinSecToSec_startGame() {
		Integer expected = 1800;
		String inputStr = "15:00";
		PlayPeriodEnum playPeriod = PlayPeriodEnum.FIRST;
		Integer value = PbpParsingUtils.convertMinSecToSec(inputStr, playPeriod);
		Assertions.assertEquals(expected, value);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertMinSecToSec(java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum)}.
	 */
	@Test
	final void testConvertMinSecToSec_startSecondQuarter() {
		Integer expected = 900;
		String inputStr = "15:00";
		PlayPeriodEnum playPeriod = PlayPeriodEnum.SECOND;
		Integer value = PbpParsingUtils.convertMinSecToSec(inputStr, playPeriod);
		Assertions.assertEquals(expected, value);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertMinSecToSec(java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum)}.
	 */
	@Test
	final void testConvertMinSecToSec_startSecondHalf() {
		Integer expected = 1800;
		String inputStr = "15:00";
		PlayPeriodEnum playPeriod = PlayPeriodEnum.THIRD;
		Integer value = PbpParsingUtils.convertMinSecToSec(inputStr, playPeriod);
		Assertions.assertEquals(expected, value);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertMinSecToSec(java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum)}.
	 */
	@Test
	final void testConvertMinSecToSec_midQuarter() {
		Integer expected = 325;
		String inputStr = "5:25";
		PlayPeriodEnum playPeriod = PlayPeriodEnum.SECOND;
		Integer value = PbpParsingUtils.convertMinSecToSec(inputStr, playPeriod);
		Assertions.assertEquals(expected, value);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertMinSecToSec(java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum)}.
	 */
	@Test
	final void testConvertMinSecToSec_overtime() {
		String inputStr = "5:25";
		PlayPeriodEnum playPeriod = PlayPeriodEnum.OT;
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.convertMinSecToSec(inputStr, playPeriod);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertMinSecToSec(java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum)}.
	 */
	@Test
	final void testConvertMinSecToSec_badInput() {
		String inputStr = "525";
		PlayPeriodEnum playPeriod = PlayPeriodEnum.OT;
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.convertMinSecToSec(inputStr, playPeriod);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractTackle(java.lang.String)}.
	 */
	@Test
	final void testExtractTackle_noMatch() {
		String[] expected = new String[0];
		String inputStr = "Adrian Olivo kickoff 30 yards to the ALCORN35, out-ofbounds, PENALTY NCCU IP 0 yards to the ALCORN35, 1st and 10";
		String[] actual = PbpParsingUtils.extractTackle(inputStr);
		Assertions.assertArrayEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractTackle(java.lang.String)}.
	 */
	@Test
	final void testExtractTackle_block() {
		String[] expected = new String[0];
		String inputStr = "xxxx Blocked By xxxxxx";
		String[] actual = PbpParsingUtils.extractTackle(inputStr);
		Assertions.assertArrayEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractTackle(java.lang.String)}.
	 */
	@Test
	final void testExtractTackle_oneMatch() {
		String[] expected = new String[] { "MANNY SMITH" };
		String inputStr = "F. Harper pass complete to CJBolar for 6 yards to the ALCORN41 (Manny Smith)";
		String[] actual = PbpParsingUtils.extractTackle(inputStr);
		Assertions.assertArrayEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractTackle(java.lang.String)}.
	 */
	@Test
	final void testExtractTackle_multMatch() {
		String[] expected = new String[] { "STEPHEN STOKES", "NOAH RAINBOW-DOUGLAS" };
		String inputStr = "S. Anderson rush for 3 yards to the NCCU47 (Stephen Stokes;Noah Rainbow-Douglas)";
		String[] actual = PbpParsingUtils.extractTackle(inputStr);
		Assertions.assertArrayEquals(expected, actual);
	}

//	/**
//	 * Test method for
//	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractTackle(java.lang.String)}.
//	 */
//	@Test
//	final void testExtractTackle_badInput() {
//		String inputStr = "S. Anderson (Stephen Stokes;Noah Rainbow-Douglas) rush for 3 yards to the NCCU47 (Stephen Stokes;Noah Rainbow-Douglas)";
//		Assertions.assertThrows(IllegalArgumentException.class, () -> {
//			PbpParsingUtils.extractTackle(inputStr);
//		});
//	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#evalMatch(java.lang.String, java.lang.String)}.
	 */
	@Test
	final void testEvalMatch_true() {
		String inputStr = "Central won the toss and elected to defer till second half";
		String regex = "w[io]ns?(?: the)?(?: coin)? toss ";
		boolean actual = PbpParsingUtils.evalMatch(inputStr, regex);
		Assertions.assertTrue(actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#evalMatch(java.lang.String, java.lang.String)}.
	 */
	@Test
	final void testEvalMatch_false() {
		String inputStr = "Central won the toss and elected to defer till second half";
		String regex = "[A-Z]\\.[A-Z][a-z]+ [A-Z] ";
		boolean actual = PbpParsingUtils.evalMatch(inputStr, regex);
		Assertions.assertFalse(actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#evalMatch(java.lang.String, java.lang.String)}.
	 */
	@Test
	final void testEvalMatch_badInput() {
		String inputStr = "Central won the toss and elected to defer till second half";
		String regex = "[";
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.evalMatch(inputStr, regex);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractCustom(java.lang.String, java.lang.String, java.lang.Integer)}.
	 */
	@Test
	final void testExtractCustom_success() {
		String expected = "ALCORN35~|ALCORN35~|";
		String inputStr = "Adrian Olivo kickoff 30 yards to the ALCORN35, out-of-bounds, PENALTY NCCU IP 0 yards to the ALCORN35, 1st and 10.";
		String regex = "(?:(?:to)|(?:at))(?: the)? ([A-Z]*?-?[aA-zZ]{2,3}\\s?\\d{1,2}|50 yardline)";
		Integer groups = 1;
		String actual = PbpParsingUtils.extractCustom(inputStr, regex, groups);
		Assertions.assertEquals(expected, actual);

	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractCustom(java.lang.String, java.lang.String, java.lang.Integer)}.
	 */
	@Test
	final void testExtractCustom_fail() {
		String inputStr = "Central won the toss and elected to defer till second half";
		String regex = "[A-Z]\\.[A-Z][a-z]+ [A-Z] ";
		Integer groups = 1;
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.extractCustom(inputStr, regex, groups);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#extractCustom(java.lang.String, java.lang.String, java.lang.Integer)}.
	 */
	@Test
	final void testExtractCustom_failToManyGroups() {
		String inputStr = "Adrian Olivo kickoff 30 yards to the ALCORN35, out-of-bounds, PENALTY NCCU IP 0 yards to the ALCORN35, 1st and 10.";
		String regex = "(?:(?:to)|(?:at))(?: the)? ([A-Z]*?-?[aA-zZ]{2,3}\\s?\\d{1,2}|50 yardline)";
		Integer groups = 6;
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.extractCustom(inputStr, regex, groups);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertDownAndDistance(java.lang.String)}.
	 */
	@ParameterizedTest
	@CsvSource({ "1st, FIRST", "2nd, SECOND", "3rd, THIRD", "4th, FOURTH" })
	final void testConvertDownAndDistance(String input, String output) {

		String inputStr = String.format("%s and 10 at ALCORN35", input);
		String[] actual = PbpParsingUtils.convertDownAndDistance(inputStr);

		String[] expected = new String[] { output, "10", "ALCORN35" };
		Assertions.assertArrayEquals(expected, actual);
	}

//	/**
//	 * Test method for
//	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertDownAndDistance(java.lang.String)}.
//	 */
//	@Test
//	final void testConvertDownAndDistance_secondDown() {
//
//		String inputStr = "2nd and 10 at ALCORN35";
//		String[] actual = PbpParsingUtils.convertDownAndDistance(inputStr);
//
//		String[] expected = new String[] { "SECOND", "10", "ALCORN35" };
//		Assertions.assertArrayEquals(expected, actual);
//	}
//
//	/**
//	 * Test method for
//	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertDownAndDistance(java.lang.String)}.
//	 */
//	@Test
//	final void testConvertDownAndDistance_thirdDown() {
//
//		String inputStr = "3rd and 10 at ALCORN35";
//		String[] actual = PbpParsingUtils.convertDownAndDistance(inputStr);
//
//		String[] expected = new String[] { "THIRD", "10", "ALCORN35" };
//		Assertions.assertArrayEquals(expected, actual);
//	}
//
//	/**
//	 * Test method for
//	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertDownAndDistance(java.lang.String)}.
//	 */
//	@Test
//	final void testConvertDownAndDistance_fourthDown() {
//
//		String inputStr = "4th and 10 at ALCORN35";
//		String[] actual = PbpParsingUtils.convertDownAndDistance(inputStr);
//
//		String[] expected = new String[] { "FOURTH", "10", "ALCORN35" };
//		Assertions.assertArrayEquals(expected, actual);
//	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#convertDownAndDistance(java.lang.String)}.
	 */
	@Test
	final void testConvertDownAndDistance_fifthDown() {

		String inputStr = "5th and 10 at ALCORN35";

		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.convertDownAndDistance(inputStr);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatName(java.lang.String)}.
	 */
	@Test
	final void testFormatName_standard() {
		String inputStrRaw = "Adrian Olivo";
		String actual = PbpParsingUtils.formatName(inputStrRaw);
		String expected = "ADRIAN OLIVO";

		Assertions.assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatName(java.lang.String)}.
	 */
	@Test
	final void testFormatName_comma() {
		String inputStrRaw = "Olivo, Adrian";
		String actual = PbpParsingUtils.formatName(inputStrRaw);
		String expected = "ADRIAN OLIVO";

		Assertions.assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatName(java.lang.String)}.
	 */
	@Test
	final void testFormatName_period() {
		String inputStrRaw = "A. Olivo";
		String actual = PbpParsingUtils.formatName(inputStrRaw);
		String expected = "A OLIVO";

		Assertions.assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatName(java.lang.String)}.
	 */
	@Test
	final void testFormatName_compound() {
		String inputStrRaw = "KBurgess-Martin";
		String actual = PbpParsingUtils.formatName(inputStrRaw);
		String expected = "K BURGESS-MARTIN";

		Assertions.assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatName(java.lang.String)}.
	 */
	@Test
	final void testFormatName_team() {
		String inputStrRaw = "TEAM";
		String actual = PbpParsingUtils.formatName(inputStrRaw);
		String expected = "TEAM";

		Assertions.assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatName(java.lang.String)}.
	 */
	@Test
	final void testFormatName_invalidSingle() {
		String inputStrRaw = "Bob";
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.formatName(inputStrRaw);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatName(java.lang.String)}.
	 */
	@Test
	final void testFormatName_null() {
		String inputStrRaw = "";
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.formatName(inputStrRaw);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#resolvePossesionTeam(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testResolvePossesionTeam_true() {
		String abbrev = "ALCORN";
		String possTeam = "2205";
		String defTeam = "912";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		PlayByPlayTeamPojo home = new PlayByPlayTeamPojo();
		home.setHomeTeam("true");
		home.setId(possTeam);
		PlayByPlayTeamPojo away = new PlayByPlayTeamPojo();
		away.setHomeTeam("false");
		away.setId(defTeam);
		teamAbbrevDict.put(possTeam, home);
		teamAbbrevDict.put(defTeam, away);
		boolean actual = PbpParsingUtils.resolvePossesionTeam(abbrev, possTeam, defTeam, teamAbbrevDict);
		Assertions.assertTrue(actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#resolvePossesionTeam(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testResolvePossesionTeam_false() {
		String abbrev = "ALCORN";
		String defTeam = "2205";
		String possTeam = "912";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		PlayByPlayTeamPojo home = new PlayByPlayTeamPojo();
		home.setHomeTeam("true");
		home.setId(possTeam);
		PlayByPlayTeamPojo away = new PlayByPlayTeamPojo();
		away.setHomeTeam("false");
		away.setId(defTeam);
		teamAbbrevDict.put(possTeam, home);
		teamAbbrevDict.put(defTeam, away);
		boolean actual = PbpParsingUtils.resolvePossesionTeam(abbrev, possTeam, defTeam, teamAbbrevDict);
		Assertions.assertFalse(actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#resolvePossesionTeam(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testResolvePossesionTeam_badInput() {
		String abbrev = "ACORN";
		String defTeam = "220";
		String possTeam = "91V2";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.resolvePossesionTeam(abbrev, possTeam, defTeam, teamAbbrevDict);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatYardLine(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testFormatYardLine_possTeam() {
		String inputStr = "ALCORN35";
		String possTeam = "2205";
		String defTeam = "912";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		PlayByPlayTeamPojo home = new PlayByPlayTeamPojo();
		home.setHomeTeam("true");
		home.setId(possTeam);
		PlayByPlayTeamPojo away = new PlayByPlayTeamPojo();
		away.setHomeTeam("false");
		away.setId(defTeam);
		teamAbbrevDict.put(possTeam, home);
		teamAbbrevDict.put(defTeam, away);

		Integer expected = 35;
		Integer actual = PbpParsingUtils.formatYardLine(inputStr, possTeam, defTeam, teamAbbrevDict);

		Assertions.assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatYardLine(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testFormatYardLine_defTeam() {
		String inputStr = "ALCORN35";
		String defTeam = "2205";
		String possTeam = "912";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		PlayByPlayTeamPojo home = new PlayByPlayTeamPojo();
		home.setHomeTeam("true");
		home.setId(possTeam);
		PlayByPlayTeamPojo away = new PlayByPlayTeamPojo();
		away.setHomeTeam("false");
		away.setId(defTeam);
		teamAbbrevDict.put(possTeam, home);
		teamAbbrevDict.put(defTeam, away);
		Integer expected = 65;

		Integer actual = PbpParsingUtils.formatYardLine(inputStr, possTeam, defTeam, teamAbbrevDict);
		Assertions.assertEquals(expected, actual);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatYardLine(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testFormatYardLine_midfield() {

		for (String inputStr : new String[] { "50 yardline", "the 50", "50 YARDLINE" }) {
			String possTeam = "2205";
			String defTeam = "912";
			Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
			PlayByPlayTeamPojo home = new PlayByPlayTeamPojo();
			home.setHomeTeam("true");
			home.setId(possTeam);
			PlayByPlayTeamPojo away = new PlayByPlayTeamPojo();
			away.setHomeTeam("false");
			away.setId(defTeam);
			teamAbbrevDict.put(possTeam, home);
			teamAbbrevDict.put(defTeam, away);

			Integer expected = 50;

			Integer actual = PbpParsingUtils.formatYardLine(inputStr, possTeam, defTeam, teamAbbrevDict);

			Assertions.assertEquals(expected, actual);
		}
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatYardLine(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testFormatYardLine_badInput() {
		String inputStr = "ALCORNxxx35";
		String defTeam = "2205";
		String possTeam = "912";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.formatYardLine(inputStr, possTeam, defTeam, teamAbbrevDict);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatYardLine(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testFormatYardLine_badInputTwo() {
		String inputStr = "TRIBE35";
		String defTeam = "2205";
		String possTeam = "912";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		PlayByPlayTeamPojo home = new PlayByPlayTeamPojo();
		home.setHomeTeam("true");
		home.setId(possTeam);
		PlayByPlayTeamPojo away = new PlayByPlayTeamPojo();
		away.setHomeTeam("false");
		away.setId(defTeam);
		teamAbbrevDict.put(possTeam, home);
		teamAbbrevDict.put(defTeam, away);
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.formatYardLine(inputStr, possTeam, defTeam, teamAbbrevDict);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils#formatYardLine(java.lang.String, java.lang.String, java.lang.String, java.util.Map)}.
	 */
	@Test
	final void testFormatYardLine_badInputThree() {
		String inputStr = "ALCORN35";
		String defTeam = "225";
		String possTeam = "9122";
		Map<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();
		PlayByPlayTeamPojo home = new PlayByPlayTeamPojo();
		home.setHomeTeam("true");
		home.setId(possTeam);
		PlayByPlayTeamPojo away = new PlayByPlayTeamPojo();
		away.setHomeTeam("false");
		away.setId(defTeam);
		teamAbbrevDict.put(possTeam, home);
		teamAbbrevDict.put(defTeam, away);
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			PbpParsingUtils.formatYardLine(inputStr, possTeam, defTeam, teamAbbrevDict);
		});
	}
}
