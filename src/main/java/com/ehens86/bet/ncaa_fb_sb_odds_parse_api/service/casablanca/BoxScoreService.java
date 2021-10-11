package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayerStatIdEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore.BoxScoreDataPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore.BoxScorePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore.BoxScoreTablePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.ParsingUtils;

@Service
public class BoxScoreService {
	private static final Logger LOG = Logger.getLogger(BoxScoreService.class.toString());

	private final ParsingUtils parsingUtils;

	public BoxScoreService(ParsingUtils parsingUtils) {
		this.parsingUtils = parsingUtils;
	}
	
	public GamePojo parseBoxScore(BoxScorePojo teamStatsRaw, GamePojo game) {
		try {

			for (BoxScoreTablePojo tbl : teamStatsRaw.getTables()) {
				PlayerStatIdEnum statId = PlayerStatIdEnum.valueOf(tbl.getId().toUpperCase());
				// System.out.println(statId);
				switch (statId) {
				case RUSHING_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatRushingPojo rushStat = parsePlayerRushing(data);
						game.getTeamAway().getPlayerStat().getOffense().getRushingStat().add(rushStat);
					}
					break;
				case RUSHING_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatRushingPojo rushStat = parsePlayerRushing(data);
						game.getTeamHome().getPlayerStat().getOffense().getRushingStat().add(rushStat);
					}
					break;
				case RECEIVING_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatReceivingPojo receiveStat = parsePlayerReceiving(data);
						game.getTeamAway().getPlayerStat().getOffense().getReceivingStat().add(receiveStat);
					}
					break;
				case RECEIVING_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatReceivingPojo receiveStat = parsePlayerReceiving(data);
						game.getTeamHome().getPlayerStat().getOffense().getReceivingStat().add(receiveStat);
					}
					break;
				case PUNT_RETURNS_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatPuntReturnPojo puntReturnStat = parsePlayerPuntReturn(data);
						game.getTeamAway().getPlayerStat().getSpecialTeam().getPuntReturn().add(puntReturnStat);
					}
					break;
				case PUNT_RETURNS_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatPuntReturnPojo puntReturnStat = parsePlayerPuntReturn(data);
						game.getTeamHome().getPlayerStat().getSpecialTeam().getPuntReturn().add(puntReturnStat);
					}
					break;
				case PUNTING_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatPuntingPojo puntingStat = parsePlayerPunt(data);
						game.getTeamAway().getPlayerStat().getSpecialTeam().getPunting().add(puntingStat);
					}
					break;
				case PUNTING_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatPuntingPojo puntingStat = parsePlayerPunt(data);
						game.getTeamHome().getPlayerStat().getSpecialTeam().getPunting().add(puntingStat);
					}
					break;
				case PASSING_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatPassingPojo passStat = parsePlayerPassing(data);
						game.getTeamAway().getPlayerStat().getOffense().getPassingStat().add(passStat);
					}
					break;
				case PASSING_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatPassingPojo passStat = parsePlayerPassing(data);
						game.getTeamHome().getPlayerStat().getOffense().getPassingStat().add(passStat);
					}
					break;
				case KICK_RETURNS_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatKickReturnPojo kickReturnStat = parsePlayerKickReturn(data);
						game.getTeamAway().getPlayerStat().getSpecialTeam().getKickReturn().add(kickReturnStat);
					}
					break;
				case KICK_RETURNS_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatKickReturnPojo kickReturnStat = parsePlayerKickReturn(data);
						game.getTeamHome().getPlayerStat().getSpecialTeam().getKickReturn().add(kickReturnStat);
					}
					break;
				case KICKING_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatKickingPojo kickingStat = parsePlayerKicking(data);
						game.getTeamAway().getPlayerStat().getSpecialTeam().getKicking().add(kickingStat);
					}
					break;
				case KICKING_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatKickingPojo kickingStat = parsePlayerKicking(data);
						game.getTeamHome().getPlayerStat().getSpecialTeam().getKicking().add(kickingStat);
					}
					break;
				case DEFENSE_VISITING:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatDefenseProductionPojo defenseStat = parsePlayerDefense(data);
						game.getTeamAway().getPlayerStat().getDefense().getDefenseProduction().add(defenseStat);
					}
					break;
				case DEFENSE_HOME:
					for (BoxScoreDataPojo data : tbl.getData()) {
						if ("Total".equals(data.getRow().get(0).getDisplay())) {
							continue;
						}
						PlayerStatDefenseProductionPojo defenseStat = parsePlayerDefense(data);
						game.getTeamHome().getPlayerStat().getDefense().getDefenseProduction().add(defenseStat);
					}
					break;
				default:
					throw new IllegalArgumentException("Missing enum");
				}
			}

			return game;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}

	}
	
	private PlayerStatDefenseProductionPojo parsePlayerDefense(BoxScoreDataPojo data) {
		try {
			PlayerStatDefenseProductionPojo defenseStat = new PlayerStatDefenseProductionPojo();
			defenseStat.setPlayerName(data.getRow().get(0).getDisplay());
			defenseStat.setTackleTotal(parsingUtils.parseString(data.getRow().get(1).getDisplay()));
			defenseStat.setTackleSolo(parsingUtils.parseStringToDouble(data.getRow().get(2).getDisplay()));
			defenseStat.setSack(parsingUtils.parseStringToDouble(data.getRow().get(3).getDisplay()));
			defenseStat.setTackleForLoss(parsingUtils.parseString(data.getRow().get(4).getDisplay()));
			defenseStat.setInterception(parsingUtils.parseString(data.getRow().get(5).getDisplay()));
			defenseStat.setFumbleForced(parsingUtils.parseString(data.getRow().get(6).getDisplay()));
			defenseStat.setFumbleRecovered(parsingUtils.parseString(data.getRow().get(7).getDisplay()));
			// puntReturnStat.setPuntReturnTouchdown(Integer.valueOf(data.getRow().get(4).getDisplay()));
			return defenseStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}

	private PlayerStatKickingPojo parsePlayerKicking(BoxScoreDataPojo data) {
		try {
			PlayerStatKickingPojo kickingStat = new PlayerStatKickingPojo();
			kickingStat.setPlayerName(data.getRow().get(0).getDisplay());
			kickingStat.setFieldGoal(parsingUtils.parseString(data.getRow().get(1).getDisplay().split("-")[0]));
			kickingStat.setFieldGoalAttempt(parsingUtils.parseString(data.getRow().get(1).getDisplay().split("-")[1]));

			kickingStat.setFieldGoalLong(parsingUtils.parseString(data.getRow().get(2).getDisplay()));
			kickingStat.setExtraPoint(parsingUtils.parseString(data.getRow().get(3).getDisplay()));
			kickingStat.setTotalPoint(parsingUtils.parseString(data.getRow().get(4).getDisplay()));

			// puntReturnStat.setPuntReturnTouchdown(Integer.valueOf(data.getRow().get(4).getDisplay()));
			return kickingStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}

	private PlayerStatKickReturnPojo parsePlayerKickReturn(BoxScoreDataPojo data) {
		try {
			PlayerStatKickReturnPojo kickReturnStat = new PlayerStatKickReturnPojo();
			kickReturnStat.setPlayerName(data.getRow().get(0).getDisplay());
			kickReturnStat.setKickReturn(parsingUtils.parseString(data.getRow().get(1).getDisplay()));
			kickReturnStat.setKickReturnYard(parsingUtils.parseString(data.getRow().get(2).getDisplay()));
			kickReturnStat.setKickReturnLong(parsingUtils.parseString(data.getRow().get(4).getDisplay()));
			// puntReturnStat.setPuntReturnTouchdown(Integer.valueOf(data.getRow().get(4).getDisplay()));
			return kickReturnStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}

	private PlayerStatPassingPojo parsePlayerPassing(BoxScoreDataPojo data) {
		try {
			PlayerStatPassingPojo passStat = new PlayerStatPassingPojo();
			passStat.setPlayerName(data.getRow().get(0).getDisplay());
			passStat.setPassingCompletion(parsingUtils.parseString(data.getRow().get(1).getDisplay().split("-")[0]));
			passStat.setPassingAttempt(parsingUtils.parseString(data.getRow().get(1).getDisplay().split("-")[1]));
			passStat.setPassingInterception(parsingUtils.parseString(data.getRow().get(1).getDisplay().split("-")[2]));
			passStat.setPassingYard(parsingUtils.parseString(data.getRow().get(2).getDisplay()));
			passStat.setPassingTouchdown(parsingUtils.parseString(data.getRow().get(3).getDisplay()));
			passStat.setPassingLong(parsingUtils.parseString(data.getRow().get(4).getDisplay()));
			// puntReturnStat.setPuntReturnTouchdown(Integer.valueOf(data.getRow().get(4).getDisplay()));
			return passStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}

	private PlayerStatPuntingPojo parsePlayerPunt(BoxScoreDataPojo data) {
		try {
			PlayerStatPuntingPojo puntingStat = new PlayerStatPuntingPojo();
			puntingStat.setPlayerName(data.getRow().get(0).getDisplay());
			puntingStat.setPunt(parsingUtils.parseString(data.getRow().get(1).getDisplay()));
			puntingStat.setPuntYard(parsingUtils.parseString(data.getRow().get(2).getDisplay()));
			puntingStat.setPuntLong(parsingUtils.parseString(data.getRow().get(4).getDisplay()));
			// puntReturnStat.setPuntReturnTouchdown(Integer.valueOf(data.getRow().get(4).getDisplay()));
			return puntingStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}

	private PlayerStatPuntReturnPojo parsePlayerPuntReturn(BoxScoreDataPojo data) {
		try {
			PlayerStatPuntReturnPojo puntReturnStat = new PlayerStatPuntReturnPojo();
			puntReturnStat.setPlayerName(data.getRow().get(0).getDisplay());
			puntReturnStat.setPuntReturn(parsingUtils.parseString(data.getRow().get(1).getDisplay()));
			puntReturnStat.setPuntReturnYard(parsingUtils.parseString(data.getRow().get(2).getDisplay()));
			puntReturnStat.setPuntReturnLong(parsingUtils.parseString(data.getRow().get(4).getDisplay()));
			// puntReturnStat.setPuntReturnTouchdown(Integer.valueOf(data.getRow().get(4).getDisplay()));
			return puntReturnStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}

	}

	private PlayerStatReceivingPojo parsePlayerReceiving(BoxScoreDataPojo data) {
		try {
			PlayerStatReceivingPojo receiveStat = new PlayerStatReceivingPojo();
			receiveStat.setPlayerName(data.getRow().get(0).getDisplay());
			receiveStat.setReceivingReception(parsingUtils.parseString(data.getRow().get(1).getDisplay()));
			receiveStat.setReceivingYard(parsingUtils.parseString(data.getRow().get(2).getDisplay()));
			receiveStat.setReceivingTouchdown(parsingUtils.parseString(data.getRow().get(3).getDisplay()));
			receiveStat.setReceivingLong(parsingUtils.parseString(data.getRow().get(4).getDisplay()));
			return receiveStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}

	}

	private PlayerStatRushingPojo parsePlayerRushing(BoxScoreDataPojo data) {
		try {
			PlayerStatRushingPojo rushStat = new PlayerStatRushingPojo();
			rushStat.setPlayerName(data.getRow().get(0).getDisplay());
			rushStat.setRushingAttempt(parsingUtils.parseString(data.getRow().get(1).getDisplay()));
			rushStat.setRushingYard(parsingUtils.parseString(data.getRow().get(2).getDisplay()));
			rushStat.setRushingTouchdown(parsingUtils.parseString(data.getRow().get(3).getDisplay()));
			rushStat.setRushingLong(parsingUtils.parseString(data.getRow().get(4).getDisplay()));
			return rushStat;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}

	}
}
