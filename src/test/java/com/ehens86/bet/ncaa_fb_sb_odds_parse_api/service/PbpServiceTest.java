/**
 * 
 */
package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.DriveData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.GameData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayerData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.defense.StatDefenseData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatPassingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatReceivingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatRushingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.penalty.StatPenaltyData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickoffData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickoffReturnData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatPuntData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatPuntReturnData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * @author eric.hensley@ibm.com
 *
 */
@ExtendWith(MockitoExtension.class)
class PbpServiceTest {

	@Mock
	GameService gameService;
	@Mock
	MappingUtils mappingUtils;
	@Mock
	TeamService teamService;
	@Mock
	PlayerService playerService;

	@InjectMocks
	PbpService pbpService;

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
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PbpService#PbpService(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.GameService, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.TeamRepository, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PlayerService)}.
	 */
	@Test
	final void testPbpService() {
		Assertions.assertDoesNotThrow(() -> {
			new PbpService(gameService, mappingUtils, teamService, playerService);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PbpService#addPbpData(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest)}.
	 */
	@Test
	final void testAddPbpData_completed() {

		ParseRequest req = new ParseRequest();
		req.setGameId(5851535);
		req.setTarget(DivisionEnum.FCS);
		req.setWeek(1);
		req.setYear(2021);

		GameData game = new GameData();
		game.setPbpComplete(true);
		game.setNcaaGameId(req.getGameId().toString());
		TeamData homeTeam = new TeamData();
		homeTeam.setTeamNameSeo("home team");
		game.setTeamHome(homeTeam);
		TeamData awayTeam = new TeamData();
		awayTeam.setTeamNameSeo("away team");
		game.setTeamAway(awayTeam);
		Mockito.when(gameService.getGame(req.getGameId().toString())).thenReturn(game);

		ParseResponse actual = pbpService.addPbpData(req);
		Assertions.assertNotNull(actual);
		Assertions.assertEquals(0, actual.getItemsFound());
		Assertions.assertEquals(0, actual.getItemsCompleted());

	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PbpService#addPbpData(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest)}.
	 */
	@Test
	final void testAddPbpData_incomplete() {
		String gameNumber = "1";
		GamePojo gamePojo;
		ModelMapper modelMapper = new ModelMapper();

		ParseRequest req = new ParseRequest();
		req.setGameId(5851535);
		req.setTarget(DivisionEnum.FCS);
		req.setWeek(1);
		req.setYear(2021);

		GameData game = new GameData();
		game.setPbpComplete(false);
		game.setNcaaGameId(req.getGameId().toString());

		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		try {
			gamePojo = mapper.readValue(new File(String.format("src/test/resources/testGame%sGame.json", gameNumber)),
					GamePojo.class);
		} catch (IOException e) {
			e.printStackTrace();
			gamePojo = new GamePojo();
		}

		game.setHomeScore(gamePojo.getTeamHome().getScore());
		game.setAwayScore(gamePojo.getTeamAway().getScore());
		TeamData homeTeam = new TeamData();
		homeTeam.setNcaaTeamId(gamePojo.getTeamHome().getNcaaTeamId());
		homeTeam.setTeamName(gamePojo.getTeamHome().getTeamName());
		homeTeam.setTeamNameSeo(gamePojo.getTeamHome().getTeamNameSeo());
		homeTeam.setTeamNameShort(gamePojo.getTeamHome().getTeamNameShort());
		homeTeam.setTeamNameSixChar(gamePojo.getTeamHome().getTeamNameSixChar());
		homeTeam.setTeamNickname(gamePojo.getTeamHome().getTeamNickname());

		TeamData awayTeam = new TeamData();
		awayTeam.setNcaaTeamId(gamePojo.getTeamAway().getNcaaTeamId());
		awayTeam.setTeamName(gamePojo.getTeamAway().getTeamName());
		awayTeam.setTeamNameSeo(gamePojo.getTeamAway().getTeamNameSeo());
		awayTeam.setTeamNameShort(gamePojo.getTeamAway().getTeamNameShort());
		awayTeam.setTeamNameSixChar(gamePojo.getTeamAway().getTeamNameSixChar());
		awayTeam.setTeamNickname(gamePojo.getTeamAway().getTeamNickname());

		Map<String, TeamData> teamMap = new HashMap<>();
		teamMap.put(gamePojo.getTeamHome().getNcaaTeamId(), homeTeam);
		teamMap.put(gamePojo.getTeamAway().getNcaaTeamId(), awayTeam);

		Mockito.when(gameService.getGame(req.getGameId().toString())).thenReturn(game);
		Mockito.when(mappingUtils.mapToDto(game, GamePojo.class)).thenReturn(gamePojo);
		Mockito.when(teamService.compileTeamMap(gamePojo.getTeamHome().getNcaaTeamId(),
				gamePojo.getTeamAway().getNcaaTeamId())).thenReturn(teamMap);
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(DrivePojo.class), ArgumentMatchers.eq(DriveData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), DriveData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PlayPojo.class), ArgumentMatchers.eq(PlayData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), PlayData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PlayerStatPenaltyPojo.class),
				ArgumentMatchers.eq(StatPenaltyData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatPenaltyData.class));
		Mockito.when(playerService.resolvePlayer(ArgumentMatchers.any(String.class), ArgumentMatchers.any(String.class),
				ArgumentMatchers.any(TeamData.class)))
				.thenAnswer(invocation -> new PlayerData(invocation.getArgument(0), invocation.getArgument(1),
						invocation.getArgument(2)));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatKickoffPojo.class),
				ArgumentMatchers.eq(StatKickoffData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatKickoffData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatReceivingPojo.class),
				ArgumentMatchers.eq(StatReceivingData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatReceivingData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatPassingPojo.class),
				ArgumentMatchers.eq(StatPassingData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatPassingData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatRushingPojo.class),
				ArgumentMatchers.eq(StatRushingData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatRushingData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatDefenseProductionPojo.class),
				ArgumentMatchers.eq(StatDefenseData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatDefenseData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatPuntReturnPojo.class),
				ArgumentMatchers.eq(StatPuntReturnData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatPuntReturnData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatPuntingPojo.class),
				ArgumentMatchers.eq(StatPuntData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatPuntData.class));
		Mockito.when(mappingUtils.mapToDto(ArgumentMatchers.any(PbpPlayerStatKickReturnPojo.class),
				ArgumentMatchers.eq(StatKickoffReturnData.class)))
				.thenAnswer(invocation -> modelMapper.map(invocation.getArgument(0), StatKickoffReturnData.class));

		ParseResponse actual = pbpService.addPbpData(req);
		Assertions.assertNotNull(actual);
		Assertions.assertEquals(1, actual.getItemsFound());
		Assertions.assertEquals(1, actual.getItemsCompleted());

	}

}
