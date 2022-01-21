package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore.BoxScorePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore.BoxScoreTeamPojo;

public final class BoxScoreService {
	private static final String ERROR_S_FAILED_WITH_S = "ERROR: [%s] failed with %s";

	private static final Logger LOG = Logger.getLogger(BoxScoreService.class.toString());

	// private static static constructor to prevent instantiation
    private BoxScoreService() {
        throw new UnsupportedOperationException();
    }

	public static GamePojo addTeamNickname(BoxScorePojo teamStatsRaw, GamePojo game) {
		try {
			List<BoxScoreTeamPojo> teamsMeta = teamStatsRaw.getMeta().getTeams();
			game.getTeamHome().setTeamNickname(
					teamsMeta.stream().filter(t -> t.getId().equals(game.getTeamHome().getNcaaTeamId()))
							.collect(Collectors.toList()).get(0).getNickName());
			if (StringUtils.isEmpty(game.getTeamHome().getTeamNickname())) {
				game.getTeamHome().setTeamNickname(game.getTeamHome().getTeamName());
			}
			game.getTeamAway().setTeamNickname(
					teamsMeta.stream().filter(t -> t.getId().equals(game.getTeamAway().getNcaaTeamId()))
							.collect(Collectors.toList()).get(0).getNickName());
			if (StringUtils.isEmpty(game.getTeamAway().getTeamNickname())) {
				game.getTeamAway().setTeamNickname(game.getTeamAway().getTeamName());
			}
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
