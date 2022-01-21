/**
 * 
 */
package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

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
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;


/**
 * @author eric.hensley@ibm.com
 *
 */
@ExtendWith(MockitoExtension.class)
class ParseServiceTest {

	@Mock
	GameService gameService;

	@Mock
	TeamService teamService;

	@InjectMocks
	ParseService parseService;

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
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.ParseService#ParseService(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.TeamService, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.GameService)}.
	 */
	@Test
	final void testParseService() {
		Assertions.assertDoesNotThrow(() -> {
			new ParseService(teamService, gameService);
		});
	}

	public static <T> Answer<T> createAnswer(final T value) {
		Answer<T> dummy = new Answer<T>() {
			@Override
			public T answer(InvocationOnMock invocation) throws Throwable {
				return value;
			}
		};
		return dummy;
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.ParseService#addTeamsFromFcsWeek(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest)}.
	 */
	@Test
	final void testAddTeamsFromFcsWeek() {
		ParseRequest req = new ParseRequest();
		req.setTarget(DivisionEnum.FCS);
		req.setWeek(1);
		req.setYear(2021);

		Mockito.when(teamService.addTeam(ArgumentMatchers.any(TeamPojo.class), ArgumentMatchers.anyList()))
				.thenAnswer(invocation -> invocation.getArgument(0));
//		Mockito.when(gameService.addGame(ArgumentMatchers.any(GamePojo.class), ArgumentMatchers.anyList(),
//				ArgumentMatchers.any(TeamData.class), ArgumentMatchers.any(TeamData.class)))
//				.thenAnswer(invocation -> invocation.getArgument(0));

//						.thenAnswer(new Answer<TeamData>() {
//							@Override
//							public TeamData answer(InvocationOnMock invocation) throws Throwable {
//								Object[] args = invocation.getArguments();
//								TeamData teamArg = (TeamData) args[0];
//								List<TeamDto> listArg = (List<TeamDto>) args[1];
//								if (listArg.stream().filter(t -> teamArg.getNcaaTeamId().equals(t.getNcaaTeamId()))
//										.collect(Collectors.toList()).isEmpty()) {
//									TeamDto newDto = new TeamDto();
//									newDto.setNcaaTeamId(teamArg.getNcaaTeamId());
//									listArg.add(newDto);
//								}
//								return teamArg;
//							}
//						}));

		ParseResponse expected = parseService.addTeamsFromFcsWeek(req);
		Assertions.assertNotNull(expected);		
	}

}
