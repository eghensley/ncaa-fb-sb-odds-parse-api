/**
 * 
 */
package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.io.File;
import java.io.IOException;

import org.apache.logging.log4j.ThreadContext;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * @author eric.hensley@ibm.com
 *
 */
class PlayByPlayServiceTest {

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
	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.PlayByPlayService#parsePbP(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo)}.
	 */
	@ParameterizedTest
	@ValueSource(strings = {"1", "2", "3", "4", "5"})
	final void testParsePbP(String gameNumber) {
		ThreadContext.put(NcaaConstants.CONTEXT_DEBUG_KEY, NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE);
		ThreadContext.put(NcaaConstants.CONTEXT_STACK_KEY, NcaaConstants.CONTEXT_STACK_VALUE_TRUE);

		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		try {
			PlayByPlayPojo playByPlayRaw = mapper.readValue(new File(String.format("src/test/resources/testGame%sPlayByPlay.json", gameNumber)), PlayByPlayPojo.class);
			GamePojo game = mapper.readValue(new File(String.format("src/test/resources/testGame%sGame.json", gameNumber)), GamePojo.class);
			Assertions.assertDoesNotThrow(() -> {
				PlayByPlayService.parsePbP(playByPlayRaw, game);
			});	
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			PlayByPlayPojo playByPlayRaw = new PlayByPlayPojo();
			GamePojo game = new GamePojo();
			Assertions.assertDoesNotThrow(() -> {
				PlayByPlayService.parsePbP(playByPlayRaw, game);
			});	
		}
	}

//	/**
//	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.PlayByPlayService#parsePbP(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo)}.
//	 */
//	@Test
//	final void testParsePbP_gameTwo() {
//		ObjectMapper mapper = new ObjectMapper();
//		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
//		try {
//			PlayByPlayPojo playByPlayRaw = mapper.readValue(new File("src/test/resources/testGame2PlayByPlay.json"), PlayByPlayPojo.class);
//			GamePojo game = mapper.readValue(new File("src/test/resources/testGame2Game.json"), GamePojo.class);
//			Assertions.assertDoesNotThrow(() -> {
//				PlayByPlayService.parsePbP(playByPlayRaw, game);
//			});	
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//			PlayByPlayPojo playByPlayRaw = new PlayByPlayPojo();
//			GamePojo game = new GamePojo();
//			Assertions.assertDoesNotThrow(() -> {
//				PlayByPlayService.parsePbP(playByPlayRaw, game);
//			});	
//		}
//	}
//	
//	/**
//	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.PlayByPlayService#parsePbP(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo)}.
//	 */
//	@Test
//	final void testParsePbP_gameThree() {
//		ObjectMapper mapper = new ObjectMapper();
//		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
//		try {
//			PlayByPlayPojo playByPlayRaw = mapper.readValue(new File("src/test/resources/testGame3PlayByPlay.json"), PlayByPlayPojo.class);
//			GamePojo game = mapper.readValue(new File("src/test/resources/testGame3Game.json"), GamePojo.class);
//			Assertions.assertDoesNotThrow(() -> {
//				PlayByPlayService.parsePbP(playByPlayRaw, game);
//			});	
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//			PlayByPlayPojo playByPlayRaw = new PlayByPlayPojo();
//			GamePojo game = new GamePojo();
//			Assertions.assertDoesNotThrow(() -> {
//				PlayByPlayService.parsePbP(playByPlayRaw, game);
//			});	
//		}
//	}
//	
//	/**
//	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.PlayByPlayService#parsePbP(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo)}.
//	 */
//	@Test
//	final void testParsePbP_gameFour() {
//		ObjectMapper mapper = new ObjectMapper();
//		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
//		try {
//			PlayByPlayPojo playByPlayRaw = mapper.readValue(new File("src/test/resources/testGame4PlayByPlay.json"), PlayByPlayPojo.class);
//			GamePojo game = mapper.readValue(new File("src/test/resources/testGame4Game.json"), GamePojo.class);
//			Assertions.assertDoesNotThrow(() -> {
//				PlayByPlayService.parsePbP(playByPlayRaw, game);
//			});	
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//			PlayByPlayPojo playByPlayRaw = new PlayByPlayPojo();
//			GamePojo game = new GamePojo();
//			Assertions.assertDoesNotThrow(() -> {
//				PlayByPlayService.parsePbP(playByPlayRaw, game);
//			});	
//		}
//	}
}
