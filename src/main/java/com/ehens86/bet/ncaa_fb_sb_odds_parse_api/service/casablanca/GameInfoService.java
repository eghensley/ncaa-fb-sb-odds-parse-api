package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.gameInfo.GameInfoPojo;

@Service
public class GameInfoService {
	private static final Logger LOG = Logger.getLogger(GameInfoService.class.toString());

	public GamePojo parseGameInfo(GameInfoPojo gameInfoRaw, GamePojo game) {
		try {
			game.setDivision(DivisionEnum.valueOf(gameInfoRaw.getChampionship().getDivision().toUpperCase()));
			game.setSeason(Integer.valueOf(gameInfoRaw.getChampionship().getYear()));
			game.setGameTime(gameInfoRaw.getStatus().getStartTime());
			game.setWinner(HomeAwayEnum.valueOf(gameInfoRaw.getStatus().getWinner().toUpperCase()));

			game.setVenue(gameInfoRaw.getVenue());

			game.getTeamHome().setTeamColor(gameInfoRaw.getHome().getColor());
			game.getTeamHome().setTeamName(gameInfoRaw.getHome().getNames().getFull());
			game.getTeamHome().setTeamNameSeo(gameInfoRaw.getHome().getNames().getSeo());
			game.getTeamHome().setTeamNameShort(gameInfoRaw.getHome().getNames().getNameShort());
			game.getTeamHome().setTeamNameSixChar(gameInfoRaw.getHome().getNames().get_6Char());

			game.getTeamAway().setTeamColor(gameInfoRaw.getAway().getColor());
			game.getTeamAway().setTeamName(gameInfoRaw.getAway().getNames().getFull());
			game.getTeamAway().setTeamNameSeo(gameInfoRaw.getAway().getNames().getSeo());
			game.getTeamAway().setTeamNameShort(gameInfoRaw.getAway().getNames().getNameShort());
			game.getTeamAway().setTeamNameSixChar(gameInfoRaw.getAway().getNames().get_6Char());

			return game;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}
}
