package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.GameDto;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.TeamDto;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.GameParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.UrlParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore.BoxScorePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo.GameInfoPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.teamstats.TeamStatsPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.BoxScoreService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.GameInfoService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.TeamStatService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.UrlUtils;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

@Service
public class ParseService {

	private final UrlUtils urlUtils;
	private final GameInfoService gameInfoService;
	private final TeamStatService teamStatService;
	private final BoxScoreService boxScoreService;
	private final LoggingUtils loggingUtils;
	private final TeamService teamService;
	private final GameService gameService;

	public ParseService(UrlUtils urlUtils, GameInfoService gameInfoService, TeamStatService teamStatService,
			BoxScoreService boxScoreService, LoggingUtils loggingUtils, TeamService teamService,
			GameService gameService) {
		this.urlUtils = urlUtils;
		this.gameInfoService = gameInfoService;
		this.teamStatService = teamStatService;
		this.boxScoreService = boxScoreService;
		this.loggingUtils = loggingUtils;
		this.teamService = teamService;
		this.gameService = gameService;
	}

	public ParseResponse addTeamsFromFcsWeek(ParseRequest req) {
		String url = String.format("https://www.ncaa.com/scoreboard/football/%s/%s/0%s",
				req.getTarget().toString().toLowerCase(), req.getYear(), req.getWeek());

		UrlParseRequest urlParseRequest;

		HtmlPage page;
		Integer infoFound = null;
		Integer infoCompleted = 0;
		String errorStr = null;
		List<HtmlElement> dayList;
		Set<String> gameIds = new HashSet<>();
		List<GamePojo> parsedGames = new ArrayList<>();
		List<TeamDto> extractedTeams = new ArrayList<>();
		List<GameDto> extractedGames = new ArrayList<>();

		try {
			urlParseRequest = urlUtils.parse(url);
			errorStr = urlParseRequest.getErrorStr();
			if (urlParseRequest.getSuccess()) {

				page = urlParseRequest.getPage();
				loggingUtils.logInfo("Completed Day Odds Parse");

				dayList = page.getByXPath("/html/body/div[1]/div/main/div/div/div/div[2]/div/div/div");

				for (HtmlElement day : dayList) {
					HtmlElement rawDate = (HtmlElement) day.getByXPath(".//h6").get(0);
					List<HtmlElement> gameList = day.getByXPath(".//div/div/a");

					for (HtmlElement rawGame : gameList) {
						if ("".equals(((HtmlElement) rawGame.getByXPath(".//ul/li/span").get(5)).asText())) {
							continue;
						}
						GamePojo game = parseGame(rawGame, rawDate);
						String gameInfoUrl = String.format("https://data.ncaa.com/casablanca/game/%s/gameInfo.json",
								game.getNcaaGameId());
						GameInfoPojo gameInfoRaw = (GameInfoPojo) urlUtils.get(gameInfoUrl, GameInfoPojo.class);
						if (!gameInfoRaw.getTabs().isBoxscore()) {
							game.setValid(false);
							continue;
						}

						if ("5851674".equals(gameInfoRaw.getId())) {
							gameInfoRaw.getChampionship().setDivision("FCS");
						}
						game = gameInfoService.parseGameInfo(gameInfoRaw, game);
						if (!gameIds.contains(game.getNcaaGameId())) {
							gameIds.add(game.getNcaaGameId());
							parsedGames.add(game);
						}
					}
				}
				infoFound = parsedGames.size();
				String logInfoStr = String.format("%s items found", infoFound);
				loggingUtils.logInfo(logInfoStr);

				for (GamePojo game : parsedGames) {
					extractTeamsFromGames(game, extractedTeams, extractedGames);
				}

				ParseResponse resp = new ParseResponse(req, infoFound, extractedGames.size(), HttpStatus.OK, "");
				GameParseResponse payload = new GameParseResponse();
				payload.setExtractedGames(extractedGames);
				payload.setExtractedTeams(extractedTeams);
				payload.setGamesAdded(extractedGames.size());
				payload.setTeamsAdded(extractedTeams.size());
				resp.setPayload(payload);
				return resp;
			} else {
				throw new IllegalArgumentException(errorStr);
			}
		} catch (Exception e) {
			return new ParseResponse(req, infoFound, infoCompleted, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	private void extractTeamsFromGames(GamePojo game, List<TeamDto> extractedTeams, List<GameDto> extractedGames) {
		String gameInfoUrl = String.format("https://data.ncaa.com/casablanca/game/%s/gameInfo.json",
				game.getNcaaGameId());
		String boxScoreUrl = String.format("https://data.ncaa.com/casablanca/game/%s/boxscore.json",
				game.getNcaaGameId());
		String teamStatsUrl = String.format("https://data.ncaa.com/casablanca/game/%s/teamStats.json",
				game.getNcaaGameId());

		TeamData homeTeam;
		TeamData awayTeam;
		try {
			GameInfoPojo gameInfoRaw = (GameInfoPojo) urlUtils.get(gameInfoUrl, GameInfoPojo.class);
			if (!gameInfoRaw.getTabs().isBoxscore()) {
				game.setValid(false);
				return;
			}

			if ("5851674".equals(gameInfoRaw.getId())) {
				gameInfoRaw.getChampionship().setDivision("FCS");
			}
			gameInfoService.parseGameInfo(gameInfoRaw, game);
			TeamStatsPojo teamStatsRaw = (TeamStatsPojo) urlUtils.get(teamStatsUrl, TeamStatsPojo.class);
			teamStatService.parseTeamStats(teamStatsRaw, game);
			BoxScorePojo boxScoreRaw = (BoxScorePojo) urlUtils.get(boxScoreUrl, BoxScorePojo.class);
			boxScoreService.addTeamNickname(boxScoreRaw, game);

			homeTeam = teamService.addTeam(game.getTeamHome(), extractedTeams);
			awayTeam = teamService.addTeam(game.getTeamAway(), extractedTeams);

			gameService.addGame(game, extractedGames, awayTeam, homeTeam);
		} catch (Exception e) {
			loggingUtils.logException(e, String.format("ERROR: Casablanca parse failed for %s vs %s - game ID %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), game.getNcaaGameId()));
		}
	}

	private GamePojo parseGame(HtmlElement rawGame, HtmlElement rawDate) {
		GamePojo game = new GamePojo();
		game.setGameDate(rawDate.asText());
		game.setNcaaGameId(rawGame.getAttribute("href").replace("/game/", ""));
		game.setTeamAway(new TeamPojo());
		game.setTeamHome(new TeamPojo());

		return game;
	}

	
//	public ParseResponse parseWeekFcsGames(ParseRequest req) {
//		String url = String.format("https://www.ncaa.com/scoreboard/football/%s/%s/0%s",
//				req.getTarget().toString().toLowerCase(), req.getYear(), req.getWeek());
//		UrlParseRequest urlParseRequest;
//
//		HtmlPage page;
//		Integer infoFound = null;
//		Integer infoCompleted = 0;
//		String errorStr = null;
//		List<HtmlElement> dayList;
//
//		List<GamePojo> parsedGames = new ArrayList<>();
//
//		try {
//			urlParseRequest = urlUtils.parse(url);
//			errorStr = urlParseRequest.getErrorStr();
//			if (urlParseRequest.getSuccess()) {
//
//				page = urlParseRequest.getPage();
//				loggingUtils.logInfo("Completed Day Odds Parse");
//
//				dayList = page.getByXPath("/html/body/div[1]/div/main/div/div/div/div[2]/div/div/div");
//
//				for (HtmlElement day : dayList) {
//					HtmlElement rawDate = (HtmlElement) day.getByXPath(".//h6").get(0);
//					List<HtmlElement> gameList = day.getByXPath(".//div/div/a");
//
//					for (HtmlElement rawGame : gameList) {
//
//						if ("".equals(((HtmlElement) rawGame.getByXPath(".//ul/li/span").get(5)).asText())) {
//							continue;
//						}
//
//						parsedGames.add(parseGame(rawGame, rawDate));
//						break;
//					}
//					break;
//				}
//				infoFound = parsedGames.size();
//				String logInfoStr = String.format("%s items found", infoFound);
//				loggingUtils.logInfo(logInfoStr);
//
//				for (GamePojo game : parsedGames) {
//					parseCasablancaInfo(game);
//					infoCompleted += 1;
//					break;
//				}
//
//				ParseResponse resp = new ParseResponse(req, infoFound, infoCompleted, HttpStatus.OK, "");
//
//				List<GamePojo> responsePayload = parsedGames.stream().filter(GamePojo::isValid)
//						.collect(Collectors.toList());
//				resp.setPayload(responsePayload);
//				return resp;
//			} else {
//				throw new IllegalArgumentException(errorStr);
//			}
//		} catch (Exception e) {
//			return new ParseResponse(req, infoFound, infoCompleted, HttpStatus.BAD_REQUEST, e.toString());
//		}
//
//	}
//
//	private void parseCasablancaInfo(GamePojo game) {
//		String gameInfoUrl = String.format("https://data.ncaa.com/casablanca/game/%s/gameInfo.json",
//				game.getNcaaGameId());
//		String boxScoreUrl = String.format("https://data.ncaa.com/casablanca/game/%s/boxscore.json",
//				game.getNcaaGameId());
//		String teamStatsUrl = String.format("https://data.ncaa.com/casablanca/game/%s/teamStats.json",
//				game.getNcaaGameId());
//		String scoringSummaryUrl = String.format("https://data.ncaa.com/casablanca/game/%s/scoringSummary.json",
//				game.getNcaaGameId());
//		String playByPlayUrl = String.format("https://data.ncaa.com/casablanca/game/%s/pbp.json", game.getNcaaGameId());
//		UrlParseRequest urlParseRequest;
//
//		HtmlPage page;
//		Integer infoFound = null;
//		Integer infoCompleted = 0;
//		String errorStr = null;
//		List<HtmlElement> rawTeamStats;
//
//		try {
//			GameInfoPojo gameInfoRaw = (GameInfoPojo) urlUtils.get(gameInfoUrl, GameInfoPojo.class);
//			if (!gameInfoRaw.getTabs().isBoxscore()) {
//				game.setValid(false);
//				return;
//			}
//
//			if ("5851674".equals(gameInfoRaw.getId())) {
//				gameInfoRaw.getChampionship().setDivision("FCS");
//			}
//			game = gameInfoService.parseGameInfo(gameInfoRaw, game);
//
//			TeamStatsPojo teamStatsRaw = (TeamStatsPojo) urlUtils.get(teamStatsUrl, TeamStatsPojo.class);
//			game = teamStatService.parseTeamStats(teamStatsRaw, game);
//
//			BoxScorePojo boxScoreRaw = (BoxScorePojo) urlUtils.get(boxScoreUrl, BoxScorePojo.class);
//			game = boxScoreService.parseBoxScore(boxScoreRaw, game);
//
//			applyTeamStatsFromPlayerStats(game);
//
//			ScoringSummaryPojo scoringSummaryRaw = (ScoringSummaryPojo) urlUtils.get(scoringSummaryUrl,
//					ScoringSummaryPojo.class);
//
//			for (ScoringSummaryPeriodPojo period : scoringSummaryRaw.getPeriods()) {
//				for (ScoringSummarySummaryPojo summary : period.getSummary()) {
//					if (summary.getScoreText().contains("INT")) {
//						applyInterceptionScores(game, summary);
//					} else if (summary.getScoreText().contains("fumble")) {
//						applyFumbleScores(game, summary);
//					} else if (summary.getScoreText().contains("punt")) {
//						applyPuntScores(game, summary);
//					} else if (summary.getScoreText().contains("kickoff")) {
//						applyKickScores(game, summary);
//					} else if (summary.getScoreText().contains("blocked")) {
//						String logInfoStr = String.format("BLOCKED not supported.  Text: %s", summary.getScoreText());
//						loggingUtils.logInfo(logInfoStr);
//						throw new IllegalArgumentException(logInfoStr);
//					}
//				}
//			}
//
//			// TODO move to new method
////			if (gameInfoRaw.getTabs().isPbp()) {
////				PlayByPlayPojo playByPlayRaw = (PlayByPlayPojo) urlUtils.get(playByPlayUrl, PlayByPlayPojo.class);
////				playByPlayService.parsePbP(playByPlayRaw, game);
////				validateGame(game);
////			}
//		} catch (Exception e) {
//			errorStr = String.format("ERROR: Casablanca parse failed for %s vs %s - game ID %s",
//					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), game.getNcaaGameId());
//			loggingUtils.logException(e, errorStr);
//		}
//	}
//
//	private void applyKickScores(GamePojo game, ScoringSummarySummaryPojo summary) {
//		try {
//			String[] scoreTextRaw = summary.getScoreText().split(" returns kickoff, runs ");
//			String player = scoreTextRaw[0];
//
//			TeamPojo returnTeam = game.pullTeamById(summary.getTeamId());
//			TeamPojo kickTeam = game.pullOpponentById(summary.getTeamId());
//
//			returnTeam.getTeamStat().getSpecialTeam().getKickoffReturn().addKickoffReturnTouchdown(1);
//
//			kickTeam.getTeamStat().getSpecialTeam().getKickoff().addKickoffReturnTouchdown(1);
//
//			if (player.isEmpty()) {
//				String logInfoStr = String.format(
//						"WARNING: No name provided for punt return touchdown: %s vs %s  -- %s",
//						game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), summary.getScoreText());
//				loggingUtils.logInfo(logInfoStr);
//			} else {
//				returnTeam.getPlayerStat().getSpecialTeam().findKickoffReturnByPlayerName(player)
//						.addKickReturnTouchdown(1);
//			}
//		} catch (Exception e) {
//			String errorStr = String.format("Kickoff score matching failed for %s vs %s",
//					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName());
//			loggingUtils.logException(e, errorStr);
//		}
//	}
//
//	private void applyPuntScores(GamePojo game, ScoringSummarySummaryPojo summary) {
//		try {
//			String[] scoreTextRaw = summary.getScoreText().split(" returns punt, runs ");
//			String player = scoreTextRaw[0];
//
//			TeamPojo returnTeam = game.pullTeamById(summary.getTeamId());
//			TeamPojo puntTeam = game.pullOpponentById(summary.getTeamId());
//
//			returnTeam.getTeamStat().getSpecialTeam().getPuntReturn().addPuntReturnTouchdown(1);
//
//			puntTeam.getTeamStat().getSpecialTeam().getPunt().addPuntReturnTouchdown(1);
//
//			if (player.isEmpty()) {
//				String logInfoStr = String.format("WARNING: No name provided for punt return touchdown: %s vs %s -- %s",
//						game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), summary.getScoreText());
//				loggingUtils.logInfo(logInfoStr);
//			} else {
//				returnTeam.getPlayerStat().getSpecialTeam().findPuntReturnByPlayerName(player)
//						.addPuntReturnTouchdown(1);
//			}
//		} catch (Exception e) {
//			String errorStr = String.format("Punt score matching failed for %s vs %s", game.getTeamHome().getTeamName(),
//					game.getTeamAway().getTeamName());
//			loggingUtils.logException(e, errorStr);
//		}
//	}
//
//	private void applyInterceptionScores(GamePojo game, ScoringSummarySummaryPojo summary) {
//		try {
//			String[] scoreTextRaw = summary.getScoreText().split(" returns INT, runs ");
//			String player = scoreTextRaw[0];
//
//			Integer yards = Integer.valueOf(scoreTextRaw[1].split(" ")[0]);
//			TeamPojo defense = game.pullTeamById(summary.getTeamId());
//			TeamPojo offense = game.pullOpponentById(summary.getTeamId());
//
//			offense.getTeamStat().getOffense().getOffensePassing().addPassingInterceptionTouchdownAndYard(1, yards);
//
//			defense.getTeamStat().getDefense().getDefensePassing().addPassingInterceptionTouchdownAndYard(1, yards);
//
//			defense.getPlayerStat().getDefense().findByPlayerName(player).addInterceptionTouchdownAndYards(1, yards);
//		} catch (Exception e) {
//			String errorStr = String.format("Interception score matching failed for %s vs %s",
//					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName());
//			loggingUtils.logException(e, errorStr);
//		}
//	}
//
//	private void applyFumbleScores(GamePojo game, ScoringSummarySummaryPojo summary) {
//		try {
//			String[] scoreTextRaw = summary.getScoreText().split(" recovers fumble, runs ");
//			String player = scoreTextRaw[0];
//
//			Integer yards = Integer.valueOf(scoreTextRaw[1].split(" ")[0]);
//			TeamPojo defense = game.pullTeamById(summary.getTeamId());
//			TeamPojo offense = game.pullOpponentById(summary.getTeamId());
//
//			defense.getPlayerStat().getDefense().findByPlayerName(player).addFumbleTouchdownAndYards(1, yards);
//
//			offense.getTeamStat().getOffense().getOffenseFumble().addFumbleTouchdown(1);
//			offense.getTeamStat().getOffense().getOffenseFumble().addFumbleYard(yards);
//
//			defense.getTeamStat().getDefense().getDefenseFumble().addFumbleTouchdown(1);
//			defense.getTeamStat().getDefense().getDefenseFumble().addFumbleYard(yards);
//
//		} catch (Exception e) {
//			String errorStr = String.format("WARN: Fumble score matching failed for %s vs %s",
//					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName());
//			loggingUtils.logException(e, errorStr);
//
//		}
//	}
//
//	private void applyTeamStatsFromPlayerStats(GamePojo game) {
//		try {
//			Integer awayPassingTd = 0;
//			for (PlayerStatPassingPojo awayPassingStat : game.getTeamAway().getPlayerStat().getOffense()
//					.getPassingStat()) {
//				awayPassingTd += awayPassingStat.getPassingTouchdown();
//			}
//			game.getTeamAway().getTeamStat().getOffense().getOffensePassing().setPassingTouchdown(awayPassingTd);
//			game.getTeamHome().getTeamStat().getDefense().getDefensePassing().setPassingTouchdown(awayPassingTd);
//
//			Integer homePassingTd = 0;
//			for (PlayerStatPassingPojo homePassingStat : game.getTeamHome().getPlayerStat().getOffense()
//					.getPassingStat()) {
//				homePassingTd += homePassingStat.getPassingTouchdown();
//			}
//			game.getTeamHome().getTeamStat().getOffense().getOffensePassing().setPassingTouchdown(homePassingTd);
//			game.getTeamAway().getTeamStat().getDefense().getDefensePassing().setPassingTouchdown(homePassingTd);
//
//			Integer homeRushingTd = 0;
//			for (PlayerStatRushingPojo homeRushingStat : game.getTeamHome().getPlayerStat().getOffense()
//					.getRushingStat()) {
//				homeRushingTd += homeRushingStat.getRushingTouchdown();
//			}
//			game.getTeamHome().getTeamStat().getOffense().getOffenseRushing().setRushingTouchdown(homeRushingTd);
//			game.getTeamAway().getTeamStat().getDefense().getDefenseRushing().setRushingTouchdown(homeRushingTd);
//
//			Integer awayRushingTd = 0;
//			for (PlayerStatRushingPojo awayRushingStat : game.getTeamAway().getPlayerStat().getOffense()
//					.getRushingStat()) {
//				awayRushingTd += awayRushingStat.getRushingTouchdown();
//			}
//			game.getTeamAway().getTeamStat().getOffense().getOffenseRushing().setRushingTouchdown(awayRushingTd);
//			game.getTeamHome().getTeamStat().getDefense().getDefenseRushing().setRushingTouchdown(awayRushingTd);
//
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	private void validateGame(GamePojo game) {
//		try {
//			String homeTeamId = game.getTeamHome().getNcaaTeamId();
//			String awayTeamId = game.getTeamAway().getNcaaTeamId();
//			Map<String, Integer> driveScore = new HashMap<>();
//			driveScore.put(homeTeamId, 0);
//			driveScore.put(awayTeamId, 0);
//
//			Map<String, Integer> playScore = new HashMap<>();
//			playScore.put(homeTeamId, 0);
//			playScore.put(awayTeamId, 0);
//
//			for (DrivePojo drive : game.getPlays().getDrives()) {
//				driveScore.put(drive.getPossessionTeamId(),
//						driveScore.get(drive.getPossessionTeamId()) + drive.getDriveResultPoint());
//				for (PlayPojo play : drive.getDrivePlays()) {
//					playScore.put(play.getPlayResult().getPlayResultPossessionTeamId(),
//							playScore.get(play.getPlayResult().getPlayResultPossessionTeamId())
//									+ play.getPlayResult().getPlayResultPoints());
//				}
//			}
//
//			Integer homeScore = game.getTeamHome().getScore();
//			Integer awayScore = game.getTeamAway().getScore();
//
//			if ((homeScore - awayScore) != (driveScore.get(homeTeamId) - driveScore.get(awayTeamId))) {
//				String logInfo = String.format(
//						"Home Team Score: %s | Away Team Score: %s | Home Sum Drive Score: %s | Away Sum Drive Score: %s",
//						homeScore, awayScore, driveScore.get(homeTeamId), driveScore.get(awayTeamId));
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"(homeScore-awayScore) != (driveScore.get(homeTeamId) - driveScore.get(awayTeamId))");
//			}
//			if (!homeScore.equals(playScore.get(homeTeamId))) {
//				String logInfo = String.format("Home Team Score: %s | Sum Play Score: %s", homeScore,
//						playScore.get(homeTeamId));
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!homeScore.equals(playScore.get(homeTeamId))");
//			}
//			if (!awayScore.equals(playScore.get(awayTeamId))) {
//				String logInfo = String.format("Away Team Score: %s | Sum Play Score: %s", awayScore,
//						playScore.get(awayTeamId));
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!awayScore.equals(playScore.get(awayTeamId))");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}

}
