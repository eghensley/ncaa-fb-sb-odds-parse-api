package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.GameData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.exceptions.PbpProccessException;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.GameDto;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.GetResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.GameRepository;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;

@ExtendWith(MockitoExtension.class)
class GameServiceTest {

	@Mock
	GameRepository gameRepo;
	
	@Mock
	MappingUtils mappingUtils;

	@InjectMocks
	GameService gameService;
	
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
	final void testGameService() {
		Assertions.assertDoesNotThrow(() -> {
			new GameService(gameRepo, mappingUtils);
		});
	}

	@Test
	final void testFetchGame_notFound() {
		String gameId = "12345";
		
		Mockito.when(gameRepo.findByNcaaGameId(gameId)).thenReturn(Optional.empty());

		GetResponse actual = gameService.fetchGame( gameId);
		Integer expectedCount = 0;
		Assertions.assertEquals(expectedCount, actual.getItemsFound());
		Assertions.assertNull(actual.getPayload());		
	}
	
	@Test
	final void testFetchGame_error() {
		String gameId = "12345";
		
		Mockito.when(gameRepo.findByNcaaGameId(gameId)).thenThrow(new IllegalArgumentException("Error"));

		Assertions.assertThrows(PbpProccessException.class, () -> {
			gameService.fetchGame( gameId);
		});	
	}
	

	@Test
	final void testFetchGame_found() {
		String gameId = "12345";
		GameData expectedGame = new GameData();
		expectedGame.setNcaaGameId(gameId);
		
		GameDto exptectedDto = new GameDto();
		exptectedDto.setNcaaGameId(gameId);
		Mockito.when(gameRepo.findByNcaaGameId(gameId)).thenReturn(Optional.of(expectedGame));
		Mockito.when(mappingUtils.mapToDto(expectedGame, GameDto.class)).thenReturn(exptectedDto);

		GetResponse actual = gameService.fetchGame( gameId);
		Integer expectedCount = 1;
		Assertions.assertEquals(expectedCount, actual.getItemsFound());
		Assertions.assertNotNull(actual.getPayload());		
	}

	@Test
	final void testGetGame_found() {
		String gameId = "12345";
		GameData expectedGame = new GameData();
		expectedGame.setNcaaGameId(gameId);
		
		GameDto exptectedDto = new GameDto();
		exptectedDto.setNcaaGameId(gameId);
		Mockito.when(gameRepo.findByNcaaGameId(gameId)).thenReturn(Optional.of(expectedGame));
//		Mockito.when(mappingUtils.mapToDto(expectedGame, GameDto.class)).thenReturn(exptectedDto);

		GameData actual = gameService.getGame( gameId);
		Assertions.assertEquals(expectedGame, actual);
	}
	
	@Test
	final void testGetGame_notFound() {
		String gameId = "12345";
		GameData expectedGame = new GameData();
		expectedGame.setNcaaGameId(gameId);
		
		GameDto exptectedDto = new GameDto();
		exptectedDto.setNcaaGameId(gameId);
		Mockito.when(gameRepo.findByNcaaGameId(gameId)).thenReturn(Optional.empty());
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			gameService.getGame( gameId);
		});
	}

	@Test
	final void testAddGame() {
		String gameId = "1234";

		GamePojo parsedGame = new GamePojo();
		parsedGame.setGameDate("Saturday, August 28, 2021");
		parsedGame.setGameTime("07:00PM ET");
		parsedGame.setNcaaGameId(gameId);
		List<GameDto> extractedGames = new ArrayList<>();
		TeamData awayTeam = new TeamData();
		TeamData homeTeam = new TeamData();
		GameDto gameDto = new GameDto();
		gameDto.setNcaaGameId(gameId);
		GameData expectedGame = new GameData();
		expectedGame.setNcaaGameId(gameId);
		
		Mockito.when(mappingUtils.mapToDto(parsedGame, GameData.class)).thenReturn(expectedGame);
		Mockito.when(mappingUtils.mapToDto(parsedGame, GameDto.class)).thenReturn(gameDto);
		Mockito.when(gameRepo.save(ArgumentMatchers.any(GameData.class))).thenAnswer(invocation -> invocation.getArgument(0));

		GameData actual = gameService.addGame(parsedGame, extractedGames, awayTeam, homeTeam);
		Assertions.assertNotNull(actual);		

		
//		expectedGame.setTeamHome(homeTeam);
//		expectedGame.setTeamAway(awayTeam);
		
		
		
//		Mockito.when(gameRepo.findByNcaaGameId(gameId)).thenReturn(Optional.empty());
	}

	@Test
	final void testSaveGame() {
		GameData game = new GameData();
		Mockito.when(gameRepo.save(ArgumentMatchers.any(GameData.class))).thenAnswer(invocation -> invocation.getArgument(0));
		Assertions.assertDoesNotThrow(() -> {
			gameService.saveGame(game);;
		});
	}
	
	@Test
	final void testSaveGame_error() {
		GameData game = new GameData();
		Mockito.when(gameRepo.save(ArgumentMatchers.any(GameData.class))).thenThrow(new IllegalArgumentException());
		Assertions.assertThrows(NullPointerException.class, () -> {
			gameService.saveGame(game);;
		});
	}

}
