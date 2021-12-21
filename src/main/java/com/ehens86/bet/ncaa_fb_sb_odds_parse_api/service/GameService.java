package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.GameData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.GameDto;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.GetResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.GameRepository;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;
import com.google.common.base.Strings;

@Service
public class GameService {

	private final GameRepository gameRepo;
	private final MappingUtils mappingUtils;
	private final LoggingUtils loggingUtils;

	public GameService(GameRepository gameRepo, MappingUtils mappingUtils, LoggingUtils loggingUtils) {
		this.gameRepo = gameRepo;
		this.mappingUtils = mappingUtils;
		this.loggingUtils = loggingUtils;
	}

	public GetResponse fetchGame(String gameId) {
		Integer noInfoFound = 0;
		Integer infoFound = 1;
		GameDto gameDto;
		try {
			Optional<GameData> gameDataOpt = gameRepo.findByNcaaGameId(gameId);
			if (gameDataOpt.isPresent()) {
				gameDto = (GameDto) mappingUtils.mapToDto(gameDataOpt.get(), GameDto.class);
				return new GetResponse(infoFound, gameDto, null);
			} else {
				String logStr = String.format("No game found for ID: %s", gameId);
				loggingUtils.logInfo(logStr);
				return new GetResponse(noInfoFound, null, logStr);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, String.format("ERROR: Game fetch failed for ID: %s", gameId));
			return new GetResponse(noInfoFound, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	public GameData getGame(String gameId) {
		Optional<GameData> gameDataOpt = gameRepo.findByNcaaGameId(gameId);
		if (gameDataOpt.isPresent()) {
			return gameDataOpt.get();
		} else {
			String logStr = String.format("No game found for ID: %s", gameId);
			loggingUtils.logInfo(logStr);
			throw new IllegalArgumentException(logStr);
		}
	}
	
	@Transactional
	public GameData addGame(GamePojo parsedGame, List<GameDto> extractedGames, TeamData awayTeam, TeamData homeTeam) {
		GameData game;

		game = (GameData) mappingUtils.mapToDto(parsedGame, GameData.class);
		game.setTeamAway(awayTeam);
		game.setTeamHome(homeTeam);
		DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("EEEE, MMMM dd, yyyy");
		DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("hh:mma");

		if (Strings.isNullOrEmpty(parsedGame.getGameDate())) {
			System.out.println("catch");
		}
		if (Strings.isNullOrEmpty(parsedGame.getGameTime())) {
			game.setGameTime(null);
		} else {
			game.setGameTime(LocalTime.parse(parsedGame.getGameTime().split(" ")[0], timeFormatter));
		}
		game.setGameDate(LocalDate.parse(parsedGame.getGameDate(), dateFormatter));

		String newGameLogInfoStrFormat = "Game found to be added: %s vs %s";
		String existingGameLogInfoStrFormat = "Game already exists: %s vs %s";

		if (Boolean.FALSE.equals(gameRepo.findByNcaaGameId(game.getNcaaGameId()).isPresent())) {
			loggingUtils.logInfo(String.format(newGameLogInfoStrFormat, game.getTeamHome().getTeamName(),
					game.getTeamAway().getTeamNickname()));
			game.setPbpComplete(false);
			gameRepo.save(game);
			GameDto gameDto = (GameDto) mappingUtils.mapToDto(parsedGame, GameDto.class);
			extractedGames.add(gameDto);
		} else {
			loggingUtils.logInfo(String.format(existingGameLogInfoStrFormat, game.getTeamHome().getTeamName(),
					game.getTeamAway().getTeamNickname()));
		}
		return game;
	}

	@Transactional
	public void saveGame(GameData game) {
		try {
			gameRepo.save(game);
		} catch (Exception e) {
			loggingUtils.logException(e, String.format("ERROR: Save failed for game: %s - %s vs %s",
					game.getNcaaGameId(), game.getTeamHome().getTeamNameSeo(), game.getTeamAway().getTeamNameSeo()));
		}
	}
}
