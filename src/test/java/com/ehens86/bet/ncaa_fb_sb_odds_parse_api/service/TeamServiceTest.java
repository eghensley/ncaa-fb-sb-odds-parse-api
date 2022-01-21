/**
 * 
 */
package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;


import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.TeamDto;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.GetResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.TeamRepository;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;

/**
 * @author eric.hensley@ibm.com
 *
 */
@ExtendWith(MockitoExtension.class)
class TeamServiceTest {

	@Mock
	TeamRepository teamRepo;
	
	@Mock
	MappingUtils mappingUtils;

	@InjectMocks
	TeamService teamService;
	
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
	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.TeamService#TeamService(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.TeamRepository, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils)}.
	 */
	@Test
	final void testTeamService() {
		Assertions.assertDoesNotThrow(() -> {
			new TeamService(teamRepo, mappingUtils);
		});
	}

	/**
	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.TeamService#fetchTeam(java.lang.String)}.
	 */
	@Test
	final void testFetchTeam_notFound() {
		String teamId = "12345";
		
		Mockito.when(teamRepo.findByNcaaTeamId("12345")).thenReturn(Optional.empty());

		GetResponse actual = teamService.fetchTeam( teamId);
		Integer expectedCount = 0;
		Assertions.assertEquals(expectedCount, actual.getItemsFound());
		Assertions.assertNull(actual.getPayload());		
	}
	
	/**
	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.TeamService#fetchTeam(java.lang.String)}.
	 */
	@Test
	final void testFetchTeam_found() {
		String teamId = "12345";
		TeamData expectedTeam = new TeamData();
		expectedTeam.setNcaaTeamId(teamId);
		
		TeamDto exptectedDto = new TeamDto();
		exptectedDto.setNcaaTeamId(teamId);
		Mockito.when(teamRepo.findByNcaaTeamId("12345")).thenReturn(Optional.of(expectedTeam));
		Mockito.when(mappingUtils.mapToDto(expectedTeam, TeamDto.class)).thenReturn(exptectedDto);

		GetResponse actual = teamService.fetchTeam( teamId);
		Integer expectedCount = 1;
		Assertions.assertEquals(expectedCount, actual.getItemsFound());
		Assertions.assertNotNull(actual.getPayload());	
	}

	/**
	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.TeamService#addTeam(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo, java.util.List)}.
	 */
	@Test
	final void testAddTeam_notFound() {
		String teamId = "12345";
		TeamPojo parsedTeam = new TeamPojo();
		parsedTeam.setNcaaTeamId(teamId);
		TeamData expectedTeam = new TeamData();
		expectedTeam.setNcaaTeamId(teamId);
		List<TeamDto> extractedTeams = new ArrayList<>();
		
		Mockito.when(teamRepo.findByNcaaTeamId(teamId)).thenReturn(Optional.empty());
		Mockito.when(mappingUtils.mapToDto(parsedTeam, TeamData.class)).thenReturn(expectedTeam);

		TeamData actual = teamService.addTeam(parsedTeam, extractedTeams);
		Assertions.assertEquals(expectedTeam, actual);
		Assertions.assertEquals(1, extractedTeams.size());
	}
	
	/**
	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.TeamService#addTeam(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo, java.util.List)}.
	 */
	@Test
	final void testAddTeam_found() {
		String teamId = "12345";
		TeamPojo parsedTeam = new TeamPojo();
		parsedTeam.setNcaaTeamId(teamId);
		TeamData expectedTeam = new TeamData();
		expectedTeam.setNcaaTeamId(teamId);
		List<TeamDto> extractedTeams = new ArrayList<>();
		
		Mockito.when(teamRepo.findByNcaaTeamId(teamId)).thenReturn(Optional.of(expectedTeam));
//		Mockito.when(mappingUtils.mapToDto(parsedTeam, TeamData.class)).thenReturn(expectedTeam);

		TeamData actual = teamService.addTeam(parsedTeam, extractedTeams);
		Assertions.assertEquals(expectedTeam, actual);
		Assertions.assertEquals(0, extractedTeams.size());
	}
	
	
	/**
	 * Test method for {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.TeamService#compileTeamMap(java.lang.String, java.lang.String)}.
	 */
	@Test
	final void testCompileTeamMap() {
		String homeTeamId = "12345";
		String awayTeamId = "6789";

		TeamData expectedHomeTeam = new TeamData();
		expectedHomeTeam.setNcaaTeamId(homeTeamId);
		TeamData expectedAwayTeam = new TeamData();
		expectedAwayTeam.setNcaaTeamId(awayTeamId);
		
		Mockito.when(teamRepo.findByNcaaTeamId(homeTeamId)).thenReturn(Optional.of(expectedHomeTeam));
		Mockito.when(teamRepo.findByNcaaTeamId(awayTeamId)).thenReturn(Optional.of(expectedAwayTeam));

		Map<String, TeamData> actual = teamService.compileTeamMap(homeTeamId, awayTeamId);
		Assertions.assertEquals(expectedHomeTeam, actual.get(homeTeamId));
		Assertions.assertEquals(expectedAwayTeam, actual.get(awayTeamId));
	}

}
