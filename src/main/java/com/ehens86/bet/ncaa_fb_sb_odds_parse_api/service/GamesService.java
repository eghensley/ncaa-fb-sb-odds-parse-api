package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayerStatIdEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.UrlParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore.BoxScoreDataPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore.BoxScorePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore.BoxScoreTablePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.gameInfo.GameInfoPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.scoringSummary.ScoringSummaryPeriodPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.scoringSummary.ScoringSummaryPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.scoringSummary.ScoringSummarySummaryPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.teamStats.TeamStatsPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.teamStats.TeamStatsStatPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.teamStats.TeamStatsTeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.BoxScoreService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.GameInfoService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.PlayByPlayService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.TeamStatService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.UrlUtils;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

@Service
public class GamesService {
	private static final Logger LOG = Logger.getLogger(GamesService.class.toString());

	private final UrlUtils urlUtils;
	private final MappingUtils mappingUtils;
	private final GameInfoService gameInfoService;
	private final TeamStatService teamStatService;
	private final BoxScoreService boxScoreService;
	private final PlayByPlayService playByPlayService;

	public GamesService(UrlUtils urlUtils, MappingUtils mappingUtils, GameInfoService gameInfoService,
			TeamStatService teamStatService, BoxScoreService boxScoreService, PlayByPlayService playByPlayService) {
		this.urlUtils = urlUtils;
		this.mappingUtils = mappingUtils;
		this.gameInfoService = gameInfoService;
		this.teamStatService = teamStatService;
		this.boxScoreService = boxScoreService;
		this.playByPlayService = playByPlayService;
	}

	public ParseResponse parseWeekFcsGames() {
		String url = String.format("https://www.ncaa.com/scoreboard/football/fcs/2021/02");
		ParseRequest req = new ParseRequest();
		UrlParseRequest urlParseRequest;

		HtmlPage page;
		Integer infoFound = null;
		Integer infoCompleted = 0;
		String errorStr = null;
		List<HtmlElement> dayList;

		List<GamePojo> parsedGames = new ArrayList<GamePojo>();

		try {
			urlParseRequest = urlUtils.parse(url);
			errorStr = urlParseRequest.getErrorStr();
			if (urlParseRequest.getSuccess()) {

				page = urlParseRequest.getPage();
				LOG.info("Completed Day Odds Parse");

				dayList = page.getByXPath("/html/body/div[1]/div/main/div/div/div/div[2]/div/div/div");

				for (HtmlElement day : dayList) {
					HtmlElement rawDate = (HtmlElement) day.getByXPath(".//h6").get(0);
					List<HtmlElement> gameList = day.getByXPath(".//div/div/a");

					for (HtmlElement rawGame : gameList) {

						if ("".equals(((HtmlElement) rawGame.getByXPath(".//ul/li/span").get(5)).asText())) {
							continue;
						}

						parsedGames.add(parseGame(rawGame, rawDate));
						//break;
					}
				}
				infoFound = parsedGames.size();
				LOG.info(String.format("%s items found", infoFound));

				for (GamePojo game : parsedGames) {
					parseCasablancaInfo(game);
					infoCompleted += 1;
					//break;
				}

				ParseResponse resp = new ParseResponse(req, infoFound, infoCompleted, HttpStatus.ACCEPTED, "");
				resp.setPayload(
						parsedGames.stream().filter(parsedGame -> parsedGame.isValid()).collect(Collectors.toList()));
				return resp;
			} else {
				LOG.log(Level.WARNING, errorStr);
				throw new IllegalArgumentException(errorStr);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, errorStr);
			throw new IllegalArgumentException(errorStr);
		}

	}

	private GamePojo parseGame(HtmlElement rawGame, HtmlElement rawDate) {
		GamePojo game = new GamePojo();
		game.setGameDate(rawDate.asText());
		game.setStatsUrl(rawGame.getAttribute("href").toString().replace("/game/", ""));
		game.setTeamAway(new TeamPojo());
		game.setTeamHome(new TeamPojo());

		return game;
	}

	private void parseCasablancaInfo(GamePojo game) {
		String gameInfoUrl = String.format("https://data.ncaa.com/casablanca/game/%s/gameInfo.json",
				game.getStatsUrl());
		String boxScoreUrl = String.format("https://data.ncaa.com/casablanca/game/%s/boxscore.json",
				game.getStatsUrl());
		String teamStatsUrl = String.format("https://data.ncaa.com/casablanca/game/%s/teamStats.json",
				game.getStatsUrl());
		String scoringSummaryUrl = String.format("https://data.ncaa.com/casablanca/game/%s/scoringSummary.json",
				game.getStatsUrl());
		String playByPlayUrl = String.format("https://data.ncaa.com/casablanca/game/%s/pbp.json", game.getStatsUrl());
		UrlParseRequest urlParseRequest;

		HtmlPage page;
		Integer infoFound = null;
		Integer infoCompleted = 0;
		String errorStr = null;
		List<HtmlElement> rawTeamStats;

		try {
			GameInfoPojo gameInfoRaw = (GameInfoPojo) urlUtils.get(gameInfoUrl, GameInfoPojo.class);
			if (!gameInfoRaw.getTabs().isBoxscore()) {
				game.setValid(false);
				return;
			}
			game = gameInfoService.parseGameInfo(gameInfoRaw, game);

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
//						System.out.println(summary.getScoreText());
//						throw new IllegalArgumentException();
//					}
//				}
//			}

			if (gameInfoRaw.getTabs().isPbp()) {
				PlayByPlayPojo playByPlayRaw = (PlayByPlayPojo) urlUtils.get(playByPlayUrl, PlayByPlayPojo.class);
				playByPlayService.parsePbP(playByPlayRaw, game);
			}

			// System.out.println(boxScoreRaw);
			return;
		} catch (Exception e) {
			errorStr = String.format("ERROR: Casablanca parse failed for %s vs %s - game ID %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), game.getStatsUrl());
			LOG.log(Level.SEVERE, errorStr);
			//e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}

	}

	private void applyKickScores(GamePojo game, ScoringSummarySummaryPojo summary) {
		try {
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
			String[] scoreTextRaw = summary.getScoreText().split(" returns kickoff, runs ");
			String player = scoreTextRaw[0];

			// System.out.println(String.format("Player Name: %s", player));

			// Integer yards = Integer.valueOf(scoreTextRaw[1].split(" ")[0]);
			TeamPojo returnTeam = game.pullTeamById(summary.getTeamId());
			TeamPojo kickTeam = game.pullOpponentById(summary.getTeamId());

			// System.out.println(String.format("Return: %s", returnTeam.getTeamName()));
			// System.out.println(String.format("Kickoff: %s", kickTeam.getTeamName()));

			returnTeam.getTeamStat().getSpecialTeam().getKickoffReturn().addKickoffReturnTouchdown(1);

			kickTeam.getTeamStat().getSpecialTeam().getKickoff().addKickoffReturnTouchdown(1);

			if ("".equals(player)) {
				System.out.println(String.format("WARNING: No name provided for punt return touchdown: %s vs %s  -- %s",
						game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), summary.getScoreText()));
			} else {
				returnTeam.getPlayerStat().getSpecialTeam().findKickoffReturnByPlayerName(player)
						.addKickReturnTouchdown(1);
			}

			// System.out.println(
			// String.format("Player Matched: %s",
			// returnTeam.getPlayerStat().getSpecialTeam().findKickoffReturnByPlayerName(player)));

			// System.out.println(summary.getScoreText());
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
		} catch (Exception e) {
			String errorStr = String.format("Kickoff score matching failed for %s vs %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void applyPuntScores(GamePojo game, ScoringSummarySummaryPojo summary) {
		try {
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
			String[] scoreTextRaw = summary.getScoreText().split(" returns punt, runs ");
			String player = scoreTextRaw[0];

			// System.out.println(String.format("Player Name: %s", player));

			// Integer yards = Integer.valueOf(scoreTextRaw[1].split(" ")[0]);
			TeamPojo returnTeam = game.pullTeamById(summary.getTeamId());
			TeamPojo puntTeam = game.pullOpponentById(summary.getTeamId());

			// System.out.println(String.format("Offense: %s", returnTeam.getTeamName()));
			// System.out.println(String.format("Defense: %s", puntTeam.getTeamName()));

			returnTeam.getTeamStat().getSpecialTeam().getPuntReturn().addPuntReturnTouchdown(1);

			puntTeam.getTeamStat().getSpecialTeam().getPunt().addPuntReturnTouchdown(1);

			if ("".equals(player)) {
				System.out.println(String.format("WARNING: No name provided for punt return touchdown: %s vs %s -- %s",
						game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), summary.getScoreText()));
			} else {
				returnTeam.getPlayerStat().getSpecialTeam().findPuntReturnByPlayerName(player)
						.addPuntReturnTouchdown(1);
			}

			// System.out.println(
			// String.format("Player Matched: %s",
			// returnTeam.getPlayerStat().getSpecialTeam().findPuntReturnByPlayerName(player)));

			// System.out.println(summary.getScoreText());
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
		} catch (Exception e) {
			String errorStr = String.format("Punt score matching failed for %s vs %s", game.getTeamHome().getTeamName(),
					game.getTeamAway().getTeamName());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void applyInterceptionScores(GamePojo game, ScoringSummarySummaryPojo summary) {
		try {
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
			String[] scoreTextRaw = summary.getScoreText().split(" returns INT, runs ");
			String player = scoreTextRaw[0];

			// System.out.println(String.format("Player Name: %s", player));
//
			Integer yards = Integer.valueOf(scoreTextRaw[1].split(" ")[0]);
			TeamPojo defense = game.pullTeamById(summary.getTeamId());
			TeamPojo offense = game.pullOpponentById(summary.getTeamId());

			// System.out.println(String.format("Offense: %s", offense.getTeamName()));
			// System.out.println(String.format("Defense: %s", defense.getTeamName()));

			offense.getTeamStat().getOffense().getOffensePassing().addPassingInterceptionTouchdownAndYard(1, yards);

			defense.getTeamStat().getDefense().getDefensePassing().addPassingInterceptionTouchdownAndYard(1, yards);

			defense.getPlayerStat().getDefense().findByPlayerName(player).addInterceptionTouchdownAndYards(1, yards);
			// defense.getPlayerStat().getDefense().findByPlayerName(player).addFumbleYard(yards);

			// System.out.println(
			// String.format("Player Matched: %s",
			// defense.getPlayerStat().getDefense().findByPlayerName(player)));

			// System.out.println(summary.getScoreText());
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
		} catch (Exception e) {
			String errorStr = String.format("Interception score matching failed for %s vs %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void applyFumbleScores(GamePojo game, ScoringSummarySummaryPojo summary) {
		try {
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
			String[] scoreTextRaw = summary.getScoreText().split(" recovers fumble, runs ");
			String player = scoreTextRaw[0];

			// System.out.println(String.format("Player Name: %s", player));

			Integer yards = Integer.valueOf(scoreTextRaw[1].split(" ")[0]);
			TeamPojo defense = game.pullTeamById(summary.getTeamId());
			TeamPojo offense = game.pullOpponentById(summary.getTeamId());

			defense.getPlayerStat().getDefense().findByPlayerName(player).addFumbleTouchdownAndYards(1, yards);

			// System.out.println(String.format("Offense: %s", offense.getTeamName()));
			// System.out.println(String.format("Defense: %s", defense.getTeamName()));

			offense.getTeamStat().getOffense().getOffenseFumble().addFumbleTouchdown(1);
			offense.getTeamStat().getOffense().getOffenseFumble().addFumbleYard(yards);

			defense.getTeamStat().getDefense().getDefenseFumble().addFumbleTouchdown(1);
			defense.getTeamStat().getDefense().getDefenseFumble().addFumbleYard(yards);

			// defense.getPlayerStat().getDefense().findByPlayerName(player).addFumbleYard(yards);

			// System.out.println(
			// String.format("Player Matched: %s",
			// defense.getPlayerStat().getDefense().findByPlayerName(player)));

			// System.out.println(summary.getScoreText());
			// System.out.println("~~~~~~~~~~~~~~~~~~~~~~~");
		} catch (Exception e) {
			String errorStr = String.format("WARN: Fumble score matching failed for %s vs %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName());
			LOG.log(Level.WARNING, errorStr);
			// e.printStackTrace();
			System.out.println(errorStr);
			// throw new IllegalArgumentException(errorStr);
		}
	}

	private void applyTeamStatsFromPlayerStats(GamePojo game) {
		try {
			Integer awayPassingTd = 0;
			for (PlayerStatPassingPojo awayPassingStat : game.getTeamAway().getPlayerStat().getOffense()
					.getPassingStat()) {
				awayPassingTd += awayPassingStat.getPassingTouchdown();
			}
			game.getTeamAway().getTeamStat().getOffense().getOffensePassing().setPassingTouchdown(awayPassingTd);
			game.getTeamHome().getTeamStat().getDefense().getDefensePassing().setPassingTouchdown(awayPassingTd);

			Integer homePassingTd = 0;
			for (PlayerStatPassingPojo homePassingStat : game.getTeamHome().getPlayerStat().getOffense()
					.getPassingStat()) {
				homePassingTd += homePassingStat.getPassingTouchdown();
			}
			game.getTeamHome().getTeamStat().getOffense().getOffensePassing().setPassingTouchdown(homePassingTd);
			game.getTeamAway().getTeamStat().getDefense().getDefensePassing().setPassingTouchdown(homePassingTd);

			Integer homeRushingTd = 0;
			for (PlayerStatRushingPojo homeRushingStat : game.getTeamHome().getPlayerStat().getOffense()
					.getRushingStat()) {
				homeRushingTd += homeRushingStat.getRushingTouchdown();
			}
			game.getTeamHome().getTeamStat().getOffense().getOffenseRushing().setRushingTouchdown(homeRushingTd);
			game.getTeamAway().getTeamStat().getDefense().getDefenseRushing().setRushingTouchdown(homeRushingTd);

			Integer awayRushingTd = 0;
			for (PlayerStatRushingPojo awayRushingStat : game.getTeamAway().getPlayerStat().getOffense()
					.getRushingStat()) {
				awayRushingTd += awayRushingStat.getRushingTouchdown();
			}
			game.getTeamAway().getTeamStat().getOffense().getOffenseRushing().setRushingTouchdown(awayRushingTd);
			game.getTeamHome().getTeamStat().getDefense().getDefenseRushing().setRushingTouchdown(awayRushingTd);

		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e);
		}
	}

}
