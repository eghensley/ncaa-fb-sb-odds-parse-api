/**
 * 
 */
package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;


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

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayerData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.PlayerRepository;

/**
 * @author eric.hensley@ibm.com
 *
 */
@ExtendWith(MockitoExtension.class)
class PlayerServiceTest {

	@Mock
	PlayerRepository playerRepository;

	@InjectMocks
	PlayerService playerService;

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
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PlayerService#PlayerService(com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.PlayerRepository)}.
	 */
	@Test
	final void testPlayerService() {
		Assertions.assertDoesNotThrow(() -> {
			new PlayerService(playerRepository);
		});
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PlayerService#resolvePlayer(java.lang.String, java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData)}.
	 */
	@Test
	final void testResolvePlayer() {
		TeamData expectedTeam = new TeamData();
		expectedTeam.setTeamName("Test Team");
		expectedTeam.setNcaaTeamId("1234");
		PlayerData expectedPlayer = new PlayerData();
		expectedPlayer.setFirstName("John");
		expectedPlayer.setLastName("Doe");
		expectedPlayer.setTeam(expectedTeam);
		Mockito.when(playerRepository.findPlayerByNamesAndTeam("John", "Doe", "1234")).thenReturn(Optional.of(expectedPlayer));

		String firstName = "John";
		String lastName = "Doe";
		PlayerData expected = playerService.resolvePlayer(firstName, lastName, expectedTeam);

		Assertions.assertEquals(expected, expectedPlayer);
	}

	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PlayerService#resolvePlayer(java.lang.String, java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData)}.
	 */
	@Test
	final void testResolvePlayer_teamMismatch() {
		TeamData expectedTeam = new TeamData();
		expectedTeam.setTeamName("Test Team");
		expectedTeam.setNcaaTeamId("1234");
		PlayerData expectedPlayer = new PlayerData();
		expectedPlayer.setFirstName("John");
		expectedPlayer.setLastName("Doe");
		expectedPlayer.setTeam(expectedTeam);
		Mockito.when(playerRepository.findPlayerByNamesAndTeam("John", "Doe", "4321")).thenReturn(Optional.of(expectedPlayer));

		TeamData unexpectedTeam = new TeamData();
		unexpectedTeam.setTeamName("Unexpected Team");
		unexpectedTeam.setNcaaTeamId("4321");
		String firstName = "John";
		String lastName = "Doe";
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			playerService.resolvePlayer(firstName, lastName, unexpectedTeam);
		});
	}
	
	/**
	 * Test method for
	 * {@link com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PlayerService#resolvePlayer(java.lang.String, java.lang.String, com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData)}.
	 */
	@Test
	final void testResolvePlayer_new() {
		TeamData expectedTeam = new TeamData();
		expectedTeam.setTeamName("Test Team");
		expectedTeam.setNcaaTeamId("1234");
		Mockito.when(playerRepository.findPlayerByNamesAndTeam("John", "Doe", "1234")).thenReturn(Optional.empty());

		String firstName = "John";
		String lastName = "Doe";
		PlayerData expected = playerService.resolvePlayer(firstName, lastName, expectedTeam);

		Assertions.assertEquals(firstName, expected.getFirstName());
		Assertions.assertEquals(lastName, expected.getLastName());
		Assertions.assertEquals(expectedTeam, expected.getTeam());
	}

}
