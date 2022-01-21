package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
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

	public GameService(GameRepository gameRepo, MappingUtils mappingUtils) {
		this.gameRepo = gameRepo;
		this.mappingUtils = mappingUtils;
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
				LoggingUtils.logInfo(logStr);
				return new GetResponse(noInfoFound, null, logStr);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Game fetch failed for ID: %s", gameId));
			return new GetResponse(noInfoFound, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	@Transactional
	public GetResponse fetchGameComplete(String gameId) {
		Integer noInfoFound = 0;
		Integer infoFound = 1;
		try {
			Optional<GameData> gameDataOpt = gameRepo.findByNcaaGameId(gameId);
			if (gameDataOpt.isPresent()) {
				return new GetResponse(infoFound, gameDataOpt.get(), null);
			} else {
				String logStr = String.format("No game found for ID: %s", gameId);
				LoggingUtils.logInfo(logStr);
				return new GetResponse(noInfoFound, null, logStr);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Complete Game fetch failed for ID: %s", gameId));
			return new GetResponse(noInfoFound, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	public GetResponse fetchSeasonWeekGames(Integer season, Integer week, Boolean missing) {
		Integer infoFound = 0;
		List<GameDto> gameDtos = new ArrayList<>();
		try {
			Optional<List<GameData>> gameDataOpt = gameRepo.findByWeekAndSeason(week, season);
			if (gameDataOpt.isPresent()) {
				for (GameData game : gameDataOpt.get()) {
					if (Boolean.TRUE.equals(missing) && Boolean.TRUE.equals(game.isPbpComplete())) {
						continue;
					}
					infoFound += 1;
					GameDto gameDto = (GameDto) mappingUtils.mapToDto(game, GameDto.class);
					gameDtos.add(gameDto);
				}
				return new GetResponse(infoFound, gameDtos, null);
			} else {
				String logStr = String.format("No games found for Week %s of %s Season", week, season);
				LoggingUtils.logInfo(logStr);
				return new GetResponse(infoFound, gameDtos, logStr);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e,
					String.format("ERROR: Game fetch failed for Week %s of %s Season", week, season));
			return new GetResponse(infoFound, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	public GetResponse fetchSeasonWeekGamesOnlyIds(Integer season, Integer week, Boolean missing) {
		Integer infoFound = 0;
		List<String> gameIds = new ArrayList<>();
		try {
			Optional<List<GameData>> gameDataOpt = gameRepo.findByWeekAndSeason(week, season);
			if (gameDataOpt.isPresent()) {
				for (GameData game : gameDataOpt.get()) {
					if (Boolean.TRUE.equals(missing)
							&& (Boolean.TRUE.equals(game.isPbpComplete()) || Boolean.FALSE.equals(game.isValid()))) {
						continue;
					}
					infoFound += 1;
					GameDto gameDto = (GameDto) mappingUtils.mapToDto(game, GameDto.class);
					gameIds.add(gameDto.getNcaaGameId());
				}
				return new GetResponse(infoFound, gameIds, null);
			} else {
				String logStr = String.format("No games found for Week %s of %s Season", week, season);
				LoggingUtils.logInfo(logStr);
				return new GetResponse(infoFound, gameIds, logStr);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e,
					String.format("ERROR: Game ID fetch failed for Week %s of %s Season", week, season));
			return new GetResponse(infoFound, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	public GameData getGame(String gameId) {
		Optional<GameData> gameDataOpt = gameRepo.findByNcaaGameId(gameId);
		if (gameDataOpt.isPresent()) {
			return gameDataOpt.get();
		} else {
			String logStr = String.format("No game found for ID: %s", gameId);
			LoggingUtils.logInfo(logStr);
			throw new IllegalArgumentException(logStr);
		}
	}

	@Transactional
	public GameData addGame(GamePojo parsedGame, List<GameDto> extractedGames, TeamData awayTeam, TeamData homeTeam) {
		GameData game;
		DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("EEEE, MMMM dd, yyyy");
		DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("hh:mma");

		String noGameDateInfoStrFormat = "Game missing date string: %s vs %s";
		String newGameLogInfoStrFormat = "Game found to be added: %s vs %s";
		String existingGameLogInfoStrFormat = "Game already exists: %s vs %s";

		game = (GameData) mappingUtils.mapToDto(parsedGame, GameData.class);
		game.setTeamAway(awayTeam);
		game.setTeamHome(homeTeam);

		if (Strings.isNullOrEmpty(parsedGame.getGameDate())) {
			LoggingUtils.logInfo(String.format(noGameDateInfoStrFormat, game.getTeamHome().getTeamName(),
					game.getTeamAway().getTeamNickname()));
		}
		if (Strings.isNullOrEmpty(parsedGame.getGameTime())) {
			game.setGameTime(null);
		} else {
			game.setGameTime(LocalTime.parse(parsedGame.getGameTime().split(" ")[0], timeFormatter));
		}
		game.setGameDate(LocalDate.parse(parsedGame.getGameDate(), dateFormatter));

		if (Boolean.FALSE.equals(gameRepo.findByNcaaGameId(game.getNcaaGameId()).isPresent())) {
			LoggingUtils.logInfo(String.format(newGameLogInfoStrFormat, game.getTeamHome().getTeamName(),
					game.getTeamAway().getTeamNickname()));
			game.setPbpComplete(false);
			gameRepo.save(game);
			GameDto gameDto = (GameDto) mappingUtils.mapToDto(parsedGame, GameDto.class);
			extractedGames.add(gameDto);
		} else {
			LoggingUtils.logInfo(String.format(existingGameLogInfoStrFormat, game.getTeamHome().getTeamName(),
					game.getTeamAway().getTeamNickname()));
		}
		return game;
	}

	@Transactional
	public void saveGame(GameData game) {
		try {
			gameRepo.save(game);
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Save failed for game: %s - %s vs %s",
					game.getNcaaGameId(), game.getTeamHome().getTeamNameSeo(), game.getTeamAway().getTeamNameSeo()));
		}
	}

	@Transactional
	public void invalidGameUpdate(String gameId) {
		try {
			GameData game = getGame(gameId);
			game.setValid(false);
			saveGame(game);
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Invalid update failed for game: %s - %s", gameId));
		}
	}
}
