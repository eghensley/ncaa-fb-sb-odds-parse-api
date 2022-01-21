package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo.GameInfoPojo;

public final class GameInfoService {
	private static final Logger LOG = Logger.getLogger(GameInfoService.class.toString());
	private static final String ERROR_S_FAILED_WITH_S = "ERROR: [%s] failed with %s";

	// private static static constructor to prevent instantiation
    private GameInfoService() {
        throw new UnsupportedOperationException();
    }
    
	public static GamePojo parseGameInfo(GameInfoPojo gameInfoRaw, GamePojo game) {
		try {
			game.setDivision(DivisionEnum.valueOf(gameInfoRaw.getChampionship().getDivision().toUpperCase()));
			game.setSeason(Integer.valueOf(gameInfoRaw.getChampionship().getYear()));
			game.setGameTime(gameInfoRaw.getStatus().getStartTime());
			game.setWinner(HomeAwayEnum.valueOf(gameInfoRaw.getStatus().getWinner().toUpperCase()));

			game.setVenue(gameInfoRaw.getVenue());

			game.getTeamHome().setScore(gameInfoRaw.getHome().getScore());
			game.getTeamHome().setTeamColor(gameInfoRaw.getHome().getColor());
			game.getTeamHome().setTeamName(gameInfoRaw.getHome().getNames().getFull());
			game.getTeamHome().setTeamNameSeo(gameInfoRaw.getHome().getNames().getSeo());
			game.getTeamHome().setTeamNameShort(gameInfoRaw.getHome().getNames().getNameShort());
			game.getTeamHome().setTeamNameSixChar(gameInfoRaw.getHome().getNames().getSixChar());

			game.getTeamAway().setScore(gameInfoRaw.getAway().getScore());
			game.getTeamAway().setTeamColor(gameInfoRaw.getAway().getColor());
			game.getTeamAway().setTeamName(gameInfoRaw.getAway().getNames().getFull());
			game.getTeamAway().setTeamNameSeo(gameInfoRaw.getAway().getNames().getSeo());
			game.getTeamAway().setTeamNameShort(gameInfoRaw.getAway().getNames().getNameShort());
			game.getTeamAway().setTeamNameSixChar(gameInfoRaw.getAway().getNames().getSixChar());

			return game;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S, ste[1].getMethodName(), e.toString());
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}
}
