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


	private final TeamService teamService;
	private final GameService gameService;

	public ParseService(TeamService teamService,
			GameService gameService) {
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
			urlParseRequest = UrlUtils.parse(url);
			errorStr = urlParseRequest.getErrorStr();
			if (urlParseRequest.getSuccess()) {

				page = urlParseRequest.getPage();
				LoggingUtils.logInfo("Completed Day Odds Parse");

				dayList = page.getByXPath("/html/body/div[1]/div/main/div/div/div/div[2]/div/div/div");

				for (HtmlElement day : dayList) {
					HtmlElement rawDate = (HtmlElement) day.getByXPath(".//h6").get(0);
					List<HtmlElement> gameList = day.getByXPath(".//div/div/a");

					for (HtmlElement rawGame : gameList) {
						addTeamsFromFcsWeekGameHelper(parsedGames, rawGame, rawDate, gameIds);
					}
				}
				infoFound = parsedGames.size();
				String logInfoStr = String.format("%s items found", infoFound);
				LoggingUtils.logInfo(logInfoStr);

				for (GamePojo game : parsedGames) {
					game.setWeek(req.getWeek());
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

	private void addTeamsFromFcsWeekGameHelper(List<GamePojo> parsedGames, HtmlElement rawGame, HtmlElement rawDate,
			Set<String> gameIds) {
		try {
			if ("".equals(((HtmlElement) rawGame.getByXPath(".//ul/li/span").get(5)).asText())) {
				return;
			}
			GamePojo game = parseGame(rawGame, rawDate);
			String gameInfoUrl = String.format("https://data.ncaa.com/casablanca/game/%s/gameInfo.json",
					game.getNcaaGameId());
			GameInfoPojo gameInfoRaw = (GameInfoPojo) UrlUtils.get(gameInfoUrl, GameInfoPojo.class);
			if (!gameInfoRaw.getTabs().isBoxscore()) {
				game.setValid(false);
				return;
			}

			if ("5851674".equals(gameInfoRaw.getId())) {
				gameInfoRaw.getChampionship().setDivision("FCS");
			}
			game = GameInfoService.parseGameInfo(gameInfoRaw, game);
			if (!gameIds.contains(game.getNcaaGameId())) {
				gameIds.add(game.getNcaaGameId());
				parsedGames.add(game);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
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
			GameInfoPojo gameInfoRaw = (GameInfoPojo) UrlUtils.get(gameInfoUrl, GameInfoPojo.class);
			if (!gameInfoRaw.getTabs().isBoxscore()) {
				game.setValid(false);
				return;
			}

			if ("5851674".equals(gameInfoRaw.getId())) {
				gameInfoRaw.getChampionship().setDivision("FCS");
			}
			GameInfoService.parseGameInfo(gameInfoRaw, game);
			TeamStatsPojo teamStatsRaw = (TeamStatsPojo) UrlUtils.get(teamStatsUrl, TeamStatsPojo.class);
			TeamStatService.parseTeamStats(teamStatsRaw, game);
			BoxScorePojo boxScoreRaw = (BoxScorePojo) UrlUtils.get(boxScoreUrl, BoxScorePojo.class);
			BoxScoreService.addTeamNickname(boxScoreRaw, game);

			homeTeam = teamService.addTeam(game.getTeamHome(), extractedTeams);
			awayTeam = teamService.addTeam(game.getTeamAway(), extractedTeams);

			gameService.addGame(game, extractedGames, awayTeam, homeTeam);
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Casablanca parse failed for %s vs %s - game ID %s",
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

}
