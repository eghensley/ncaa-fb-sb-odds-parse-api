//package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;
//
//import java.util.ArrayList;
//import java.util.List;
//import java.util.Set;
//import java.util.stream.Collectors;
//
//import org.springframework.stereotype.Service;
//
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PbpPlayerStatPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PlayerStatPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.TeamStatPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatDefensePojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatOffensePojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatSpecialTeamPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player.PlayerStatPassingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player.PlayerStatReceivingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player.PlayerStatRushingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.PlayerStatKickoffPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickoffPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player.PlayerStatPuntingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatDefensePojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatKickoffPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatOffensePojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatPassingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatPuntPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatRushingPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatSpecialTeamPojo;
//import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
//
//@Service
//public class GameValidationService {
//	private static final String STAT_S = "Stat: %s";
//	private static final String NAME_S = "Name: %s";
//	private static final String PLAYER_STAT_BREAKDOWN = "Player stat breakdown------";
//	
//	
//	private final LoggingUtils loggingUtils;
//
//	public GameValidationService(LoggingUtils loggingUtils) {
//		this.loggingUtils = loggingUtils;
//	}
//
//	public void validateGame(GamePojo game) {
//		try {
//			TeamPojo homeTeam = game.getTeamHome();
//			TeamStatPojo homeTeamStat = homeTeam.getTeamStat();
//			TeamStatDefensePojo homeTeamDefenseStat = homeTeamStat.getDefense();
//			TeamStatOffensePojo homeTeamOffenseStat = homeTeamStat.getOffense();
//			TeamStatSpecialTeamPojo homeTeamSpecialTeamsStat = homeTeamStat.getSpecialTeam();
//
//			PlayerStatPojo homePlayerStat = homeTeam.getPlayerStat();
//			PlayerStatDefensePojo homePlayerDefenseStat = homePlayerStat.getDefense(); // TODO validate
//			PlayerStatOffensePojo homePlayerOffenseStat = homePlayerStat.getOffense();
//			PlayerStatSpecialTeamPojo homePlayerSpecialTeamStat = homePlayerStat.getSpecialTeam();
//
//			String homeTeamId = game.getTeamHome().getNcaaTeamId();
//			String awayTeamId = game.getTeamAway().getNcaaTeamId();
//
//			List<PbpPlayerStatRushingPojo> homePbpPlayerRushingStat = new ArrayList<>();
//			List<PbpPlayerStatRushingPojo> awayPbpPlayerRushingStat = new ArrayList<>();
//			List<PbpPlayerStatPassingPojo> homePbpPlayerPassingStat = new ArrayList<>();
//			List<PbpPlayerStatPassingPojo> awayPbpPlayerPassingStat = new ArrayList<>();
//			List<PbpPlayerStatReceivingPojo> homePbpPlayerReceivingStat = new ArrayList<>();
//			List<PbpPlayerStatReceivingPojo> awayPbpPlayerReceivingStat = new ArrayList<>();
//			List<PbpPlayerStatDefenseProductionPojo> homePbpPlayerDefenseStat = new ArrayList<>(); // TODO validate
//			List<PbpPlayerStatDefenseProductionPojo> awayPbpPlayerDefenseStat = new ArrayList<>(); // TODO validate
//			List<PbpPlayerStatKickingPojo> homePbpPlayerKickingStat = new ArrayList<>();
//			List<PbpPlayerStatKickingPojo> awayPbpPlayerKickingStat = new ArrayList<>();
//			List<PbpPlayerStatKickoffPojo> homePbpPlayerKickoffStat = new ArrayList<>();
//			List<PbpPlayerStatKickoffPojo> awayPbpPlayerKickoffStat = new ArrayList<>();
//			List<PbpPlayerStatKickReturnPojo> homePbpPlayerKickReturnStat = new ArrayList<>(); // TODO validate
//			List<PbpPlayerStatKickReturnPojo> awayPbpPlayerKickReturnStat = new ArrayList<>(); // TODO validate
//			List<PbpPlayerStatPuntingPojo> homePbpPlayerPuntingStat = new ArrayList<>();
//			List<PbpPlayerStatPuntingPojo> awayPbpPlayerPuntingStat = new ArrayList<>();
//			List<PbpPlayerStatPuntReturnPojo> homePbpPlayerPuntReturnStat = new ArrayList<>(); // TODO validate
//			List<PbpPlayerStatPuntReturnPojo> awayPbpPlayerPuntReturnStat = new ArrayList<>(); // TODO validate
//
//			for (DrivePojo drive : game.getPlays().getDrives()) {
//				for (PlayPojo play : drive.getDrivePlays()) {
//					PbpPlayerStatPojo awayTeamPbpStats = play.getPlayerStat().get(awayTeamId);
//					awayTeamPbpStats.getOffense().getRushingStat().forEach(offRush -> {
//						awayPbpPlayerRushingStat.add(offRush);
//					});
//					awayTeamPbpStats.getOffense().getPassingStat().forEach(offPass -> {
//						awayPbpPlayerPassingStat.add(offPass);
//					});
//					awayTeamPbpStats.getOffense().getReceivingStat().forEach(offReceive -> {
//						awayPbpPlayerReceivingStat.add(offReceive);
//					});
//					awayTeamPbpStats.getDefense().getDefenseProduction().forEach(def -> {
//						awayPbpPlayerDefenseStat.add(def);
//					});
//					awayTeamPbpStats.getSpecialTeam().getKicking().forEach(kick -> {
//						awayPbpPlayerKickingStat.add(kick);
//					});
//					awayTeamPbpStats.getSpecialTeam().getKickoff().forEach(kickoff -> {
//						awayPbpPlayerKickoffStat.add(kickoff);
//					});
//					awayTeamPbpStats.getSpecialTeam().getKickReturn().forEach(kickReturn -> {
//						awayPbpPlayerKickReturnStat.add(kickReturn);
//					});
//					awayTeamPbpStats.getSpecialTeam().getPunting().forEach(punt -> {
//						awayPbpPlayerPuntingStat.add(punt);
//					});
//					awayTeamPbpStats.getSpecialTeam().getPuntReturn().forEach(puntReturn -> {
//						awayPbpPlayerPuntReturnStat.add(puntReturn);
//					});
//
//					PbpPlayerStatPojo homeTeamPbpStats = play.getPlayerStat().get(homeTeamId);
//					homeTeamPbpStats.getOffense().getRushingStat().forEach(offRush -> {
//						homePbpPlayerRushingStat.add(offRush);
//					});
//					homeTeamPbpStats.getOffense().getPassingStat().forEach(offPass -> {
//						homePbpPlayerPassingStat.add(offPass);
//					});
//					homeTeamPbpStats.getOffense().getReceivingStat().forEach(offReceive -> {
//						homePbpPlayerReceivingStat.add(offReceive);
//					});
//					homeTeamPbpStats.getDefense().getDefenseProduction().forEach(def -> {
//						homePbpPlayerDefenseStat.add(def);
//					});
//					homeTeamPbpStats.getSpecialTeam().getKicking().forEach(kick -> {
//						homePbpPlayerKickingStat.add(kick);
//					});
//					homeTeamPbpStats.getSpecialTeam().getKickoff().forEach(kickoff -> {
//						homePbpPlayerKickoffStat.add(kickoff);
//					});
//					homeTeamPbpStats.getSpecialTeam().getKickReturn().forEach(kickReturn -> {
//						homePbpPlayerKickReturnStat.add(kickReturn);
//					});
//					homeTeamPbpStats.getSpecialTeam().getPunting().forEach(punt -> {
//						homePbpPlayerPuntingStat.add(punt);
//					});
//					homeTeamPbpStats.getSpecialTeam().getPuntReturn().forEach(puntReturn -> {
//						homePbpPlayerPuntReturnStat.add(puntReturn);
//					});
//				}
//			}
//
//			TeamPojo awayTeam = game.getTeamAway();
//			TeamStatPojo awayTeamStat = awayTeam.getTeamStat();
//			TeamStatDefensePojo awayTeamDefenseStat = awayTeamStat.getDefense();
//			TeamStatOffensePojo awayTeamOffenseStat = awayTeamStat.getOffense();
//			TeamStatSpecialTeamPojo awayTeamSpecialTeamsStat = awayTeamStat.getSpecialTeam();
//
//			PlayerStatPojo awayPlayerStat = awayTeam.getPlayerStat();
//			PlayerStatDefensePojo awayPlayerDefenseStat = awayPlayerStat.getDefense(); // TODO validate
//			PlayerStatOffensePojo awayPlayerOffenseStat = awayPlayerStat.getOffense();
//			PlayerStatSpecialTeamPojo awayPlayerSpecialTeamStat = awayPlayerStat.getSpecialTeam();
//
//			// TODO penalty validation
//			// TODO first down validation
//			// TODO score validation
//			validateRushing(homePlayerOffenseStat.getRushingStat(), homeTeamOffenseStat.getOffenseRushing(),
//					awayTeamDefenseStat.getDefenseRushing(), homePbpPlayerRushingStat, homePbpPlayerPassingStat, true);
//			validateRushing(awayPlayerOffenseStat.getRushingStat(), awayTeamOffenseStat.getOffenseRushing(),
//					homeTeamDefenseStat.getDefenseRushing(), awayPbpPlayerRushingStat, awayPbpPlayerPassingStat, false);
//
//			validatePassing(homePlayerOffenseStat.getPassingStat(), homeTeamOffenseStat.getOffensePassing(),
//					awayTeamDefenseStat.getDefensePassing(), homePlayerOffenseStat.getReceivingStat(),
//					homePbpPlayerReceivingStat, homePbpPlayerPassingStat, true);
//			validatePassing(awayPlayerOffenseStat.getPassingStat(), awayTeamOffenseStat.getOffensePassing(),
//					homeTeamDefenseStat.getDefensePassing(), awayPlayerOffenseStat.getReceivingStat(),
//					awayPbpPlayerReceivingStat, awayPbpPlayerPassingStat, false);
//
//			validatePunting(homePlayerSpecialTeamStat.getPunting(), homeTeamSpecialTeamsStat.getPunt(),
//					awayTeamSpecialTeamsStat.getPuntReturn(), homePbpPlayerPuntingStat, true);
//			validatePunting(awayPlayerSpecialTeamStat.getPunting(), awayTeamSpecialTeamsStat.getPunt(),
//					homeTeamSpecialTeamsStat.getPuntReturn(), awayPbpPlayerPuntingStat, false);
//
//			validateTeamKickoff(homePlayerSpecialTeamStat.getKickoff(), homeTeamSpecialTeamsStat.getKickoff(),
//					awayTeamSpecialTeamsStat.getKickoffReturn(), homePbpPlayerKickoffStat, true);
//			validateTeamKickoff(awayPlayerSpecialTeamStat.getKickoff(), awayTeamSpecialTeamsStat.getKickoff(),
//					homeTeamSpecialTeamsStat.getKickoffReturn(), awayPbpPlayerKickoffStat, false);
//			loggingUtils.logInfo("test");
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Validate Kickoff Stats
//	 */
//	private void validateTeamKickoff(List<PlayerStatKickoffPojo> playerKickoffOffense,
//			TeamStatKickoffPojo teamKickoffOffense, TeamStatKickoffPojo teamKickoffDefense,
//			List<PbpPlayerStatKickoffPojo> pbpKickoffOffense, boolean home) {
//		try {
//			String off;
//			String def;
//			if (Boolean.TRUE.equals(home)) {
//				off = "Home";
//				def = "Away";
//			} else {
//				off = "Away";
//				def = "Home";
//			}
//
//			validateKickoffPbpHelper(pbpKickoffOffense, teamKickoffOffense, off, def);
//			validateKickoffPlayerHelper(playerKickoffOffense, teamKickoffOffense, off, def);
//			validateKickoffTeamHelper(teamKickoffOffense, teamKickoffDefense, off, def);
//
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Player Kickoff <> Team Kickoff
//	 */
//	private void validateKickoffPbpHelper(List<PbpPlayerStatKickoffPojo> pbpKickoffOffense,
//			TeamStatKickoffPojo teamKickoffOffense, String off, String def) {
//		try {
//			int kickoffReturnTouchdown = pbpKickoffOffense.stream()
//					.mapToInt(PbpPlayerStatKickoffPojo::getKickoffReturnTouchdown).sum();
//			if (!teamKickoffOffense.getKickoffReturnTouchdown().equals(kickoffReturnTouchdown)) {
//				String logInfo = String.format(
//						"%s Kickoff Return Touchdown: %s | PBP Sum %s Kickoff Return Touchdown: %s", off,
//						teamKickoffOffense.getKickoffReturnTouchdown(), off, kickoffReturnTouchdown);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamKickoffOffense.getKickoffReturnTouchdown().equals(kickoffReturnTouchdown)");
//			}
//
//			int kickoffReturnYards = pbpKickoffOffense.stream().mapToInt(PbpPlayerStatKickoffPojo::getKickoffReturnYard)
//					.sum();
//			if (!teamKickoffOffense.getKickoffReturnYards().equals(kickoffReturnYards)) {
//				String logInfo = String.format("%s Kickoff Return Yards: %s | PBP Sum %s Kickoff Return Yards: %s", off,
//						teamKickoffOffense.getKickoffReturnYards(), off, kickoffReturnYards);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamKickoffOffense.getKickoffReturnYards().equals(kickoffReturnYards)");
//			}
//
//			int kickoffYards = pbpKickoffOffense.stream().mapToInt(PbpPlayerStatKickoffPojo::getKickoffYard).sum();
//			if (!teamKickoffOffense.getKickoffYards().equals(kickoffYards)) {
//				String logInfo = String.format("%s Kickoff Yards: %s | PBP Sum %s Kickoff Yards: %s", off,
//						teamKickoffOffense.getKickoffYards(), off, kickoffYards);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamKickoffOffense.getKickoffYards().equals(kickoffYards)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Player Kickoff <> Team Kickoff
//	 */
//	private void validateKickoffPlayerHelper(List<PlayerStatKickoffPojo> playerKickoffOffense,
//			TeamStatKickoffPojo teamKickoffOffense, String off, String def) {
//		try {
//			int kickoffYards = playerKickoffOffense.stream().mapToInt(PlayerStatKickoffPojo::getKickoffYard).sum();
//			if (!teamKickoffOffense.getKickoffYards().equals(kickoffYards)) {
//				String logInfo = String.format("%s Kickoff Yards: %s | Sum %s Kickoff Yards: %s", off,
//						teamKickoffOffense.getKickoffYards(), off, kickoffYards);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamKickoffOffense.getKickoffYards().equals(kickoffYards)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Team Defense Kickoff <> Team Offense Kickoff
//	 */
//	private void validateKickoffTeamHelper(TeamStatKickoffPojo teamKickoffOffense,
//			TeamStatKickoffPojo teamKickoffDefense, String off, String def) {
//		try {
//			if (!teamKickoffOffense.getKickoff().equals(teamKickoffDefense.getKickoff())) {
//				String logInfo = String.format("%s Offense Kickoffs: %s | %s Defense Kickoffs: %s", off,
//						teamKickoffOffense.getKickoff(), def, teamKickoffDefense.getKickoff());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamKickoffOffense.getKickoff().equals(teamKickoffDefense.getKickoff())");
//			}
//			if (!teamKickoffOffense.getKickoffReturnTouchdown()
//					.equals(teamKickoffDefense.getKickoffReturnTouchdown())) {
//				String logInfo = String.format("%s Offense Kickoff Touchdown: %s | %s Defense Kickoff Touchdown: %s",
//						off, teamKickoffOffense.getKickoffReturnTouchdown(), def,
//						teamKickoffDefense.getKickoffReturnTouchdown());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamKickoffOffense.getPuntReturnTouchdown().equals(teamKickoffDefense.getPuntReturnTouchdown())");
//			}
//			if (!teamKickoffOffense.getKickoffReturnYards().equals(teamKickoffDefense.getKickoffReturnYards())) {
//				String logInfo = String.format(
//						"%s Offense Kickoff Return Yards: %s | %s Defense Kickoff Return Yards: %s", off,
//						teamKickoffOffense.getKickoffReturnYards(), def, teamKickoffDefense.getKickoffReturnYards());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamKickoffOffense.getKickoffReturnYards().equals(teamKickoffDefense.getKickoffReturnYards())");
//			}
//			if (!teamKickoffOffense.getKickoffYards().equals(teamKickoffDefense.getKickoffYards())) {
//				String logInfo = String.format("%s Offense Kickoff Yards: %s | %s Defense Kickoff Yards: %s", off,
//						teamKickoffOffense.getKickoffYards(), def, teamKickoffDefense.getKickoffYards());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamKickoffOffense.getKickoffYards().equals(teamKickoffDefense.getKickoffYards())");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Validate Punting Stats
//	 */
//	private void validatePunting(List<PlayerStatPuntingPojo> playerPuntingOffense, TeamStatPuntPojo teamPuntingOffense,
//			TeamStatPuntPojo teamPuntingDefense, List<PbpPlayerStatPuntingPojo> pbpPuntingOffense, boolean home) {
//		try {
//			String off;
//			String def;
//			if (Boolean.TRUE.equals(home)) {
//				off = "Home";
//				def = "Away";
//			} else {
//				off = "Away";
//				def = "Home";
//			}
//			validatePuntingPbpHelper(teamPuntingOffense, pbpPuntingOffense, off, def);
//			validatePuntingPlayerHelper(teamPuntingOffense, playerPuntingOffense, off, def);
//			validatePuntingTeamHelper(teamPuntingOffense, teamPuntingDefense, off, def);
//
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * PBP Punting <> Team Punting
//	 */
//	private void validatePuntingPbpHelper(TeamStatPuntPojo teamPuntingOffense,
//			List<PbpPlayerStatPuntingPojo> pbpPuntingOffense, String off, String def) {
//		try {
//			int puntingReturnTouchdownPbp = pbpPuntingOffense.stream()
//					.mapToInt(PbpPlayerStatPuntingPojo::getPuntReturnTouchdown).sum();
//			if (!teamPuntingOffense.getPuntReturnTouchdown().equals(puntingReturnTouchdownPbp)) {
//				String logInfo = String.format(
//						"%s Punting Return Touchdowns: %s | PBP Sum %s Punting Return Touchdowns: %s", off,
//						teamPuntingOffense.getPuntReturnTouchdown(), off, puntingReturnTouchdownPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPuntingOffense.getPuntReturnTouchdown().equals(puntingReturnTouchdownPbp)");
//			}
//			int puntingReturnYardsPbp = pbpPuntingOffense.stream().mapToInt(PbpPlayerStatPuntingPojo::getPuntReturnYard)
//					.sum();
//			if (!teamPuntingOffense.getPuntReturnYards().equals(puntingReturnYardsPbp)) {
//				String logInfo = String.format("%s Punting Return Yards: %s | PBP Sum %s Punting Return Yards: %s", off,
//						teamPuntingOffense.getPuntReturnYards(), off, puntingReturnYardsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPuntingOffense.getPuntReturnYards().equals(puntingReturnYardsPbp)");
//			}
//			int puntsPbp = pbpPuntingOffense.stream().mapToInt(PbpPlayerStatPuntingPojo::getPunt).sum();
//			if (!teamPuntingOffense.getPunt().equals(puntsPbp)) {
//				String logInfo = String.format("%s Punts: %s | PBP Sum %s Punts: %s", off, teamPuntingOffense.getPunt(),
//						off, puntsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPuntingOffense.getPunt().equals(puntsPbp)");
//			}
//			int puntingYardsPbp = pbpPuntingOffense.stream().mapToInt(PbpPlayerStatPuntingPojo::getPuntYard).sum();
//			if (!teamPuntingOffense.getPuntYards().equals(puntingYardsPbp)) {
//				String logInfo = String.format("%s Punting Yards: %s | PBP Sum %s Punting Yards: %s", off,
//						teamPuntingOffense.getPuntYards(), off, puntingYardsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPuntingOffense.getPuntYards().equals(puntingYardsPbp)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Player Punting <> Team Punting
//	 */
//	private void validatePuntingPlayerHelper(TeamStatPuntPojo teamPuntingOffense,
//			List<PlayerStatPuntingPojo> playerPuntingOffense, String off, String def) {
//		try {
//			int punts = playerPuntingOffense.stream().mapToInt(PlayerStatPuntingPojo::getPunt).sum();
//			if (!teamPuntingOffense.getPunt().equals(punts)) {
//				String logInfo = String.format("%s Punts: %s | Sum %s Punts: %s", off, teamPuntingOffense.getPunt(),
//						off, punts);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPuntingOffense.getPunt().equals(punts)");
//			}
//			int puntingYards = playerPuntingOffense.stream().mapToInt(PlayerStatPuntingPojo::getPuntYard).sum();
//			if (!teamPuntingOffense.getPuntYards().equals(puntingYards)) {
//				String logInfo = String.format("%s Punting Yards: %s | Sum %s Punting Yards: %s", off,
//						teamPuntingOffense.getPuntYards(), off, puntingYards);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPuntingOffense.getPuntYards().equals(puntingYards)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Team Defense Punting <> Team Offense Punting
//	 */
//	private void validatePuntingTeamHelper(TeamStatPuntPojo teamPuntingOffense, TeamStatPuntPojo teamPuntingDefense,
//			String off, String def) {
//		try {
//			if (!teamPuntingOffense.getPunt().equals(teamPuntingDefense.getPunt())) {
//				String logInfo = String.format("%s Offense Punts: %s | %s Defense Punts: %s", off,
//						teamPuntingOffense.getPunt(), def, teamPuntingDefense.getPunt());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPuntingOffense.getPunt().equals(teamPuntingDefense.getPunt())");
//			}
//			if (!teamPuntingOffense.getPuntReturnTouchdown().equals(teamPuntingDefense.getPuntReturnTouchdown())) {
//				String logInfo = String.format("%s Offense Punt Touchdown: %s | %s Defense Punt Touchdown: %s", off,
//						teamPuntingOffense.getPuntReturnTouchdown(), def, teamPuntingDefense.getPuntReturnTouchdown());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPuntingOffense.getPuntReturnTouchdown().equals(teamPuntingDefense.getPuntReturnTouchdown())");
//			}
//			if (!teamPuntingOffense.getPuntReturnYards().equals(teamPuntingDefense.getPuntReturnYards())) {
//				String logInfo = String.format("%s Offense Punt Return Yards: %s | %s Defense Punt Return Yards: %s",
//						off, teamPuntingOffense.getPuntReturnYards(), def, teamPuntingDefense.getPuntReturnYards());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPuntingOffense.getPuntReturnYards().equals(teamPuntingDefense.getPuntReturnYards())");
//			}
//			if (!teamPuntingOffense.getPuntYards().equals(teamPuntingDefense.getPuntYards())) {
//				String logInfo = String.format("%s Offense Punt Yards: %s | %s Defense Punt Yards: %s", off,
//						teamPuntingOffense.getPuntYards(), def, teamPuntingDefense.getPuntYards());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPuntingOffense.getPuntYards().equals(teamPuntingDefense.getPuntYards())");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Validate Rushing Stats
//	 */
//	private void validateRushing(List<PlayerStatRushingPojo> playerRushingOffense,
//			TeamStatRushingPojo teamRushingOffense, TeamStatRushingPojo teamRushingDefense,
//			List<PbpPlayerStatRushingPojo> pbpPlayerRushingStat, List<PbpPlayerStatPassingPojo> pbpPlayerPassingStat,
//			boolean home) {
//		try {
//			String off;
//			String def;
//			if (Boolean.TRUE.equals(home)) {
//				off = "Home";
//				def = "Away";
//			} else {
//				off = "Away";
//				def = "Home";
//			}
//
//			validateRushingPlayerHelper(teamRushingOffense, playerRushingOffense, off, def);
//			validateRushingPbpHelper(teamRushingOffense, pbpPlayerRushingStat, pbpPlayerPassingStat, off, def);
//			validateRushingTeamHelper(teamRushingOffense, teamRushingDefense, off, def);
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Player Rushing <> Team Offense Rushing
//	 */
//	private void validateRushingPlayerHelper(TeamStatRushingPojo teamRushingOffense,
//			List<PlayerStatRushingPojo> playerRushingOffense, String off, String def) {
//		try {
//
//			int rushingTouchdowns = playerRushingOffense.stream().mapToInt(PlayerStatRushingPojo::getRushingTouchdown)
//					.sum();
//			if (!teamRushingOffense.getRushingTouchdown().equals(rushingTouchdowns)) {
//				String logInfo = String.format("%s Rushing Touchdown: %s | Sum %s Rushing Touchdown: %s", off,
//						teamRushingOffense.getRushingTouchdown(), off, rushingTouchdowns);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!homeTeamRushingOffense.getRushingTouchdown().equals(rushingTouchdowns)");
//			}
//
//			int rushingYards = playerRushingOffense.stream().mapToInt(PlayerStatRushingPojo::getRushingYard).sum();
//			if (!teamRushingOffense.getRushingYard().equals(rushingYards)) {
//				String logInfo = String.format("%s Rushing Yards: %s | Sum %s Rushing Yards: %s", off,
//						teamRushingOffense.getRushingYard(), off, rushingYards);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!homeTeamRushingOffense.getRushingYard().equals(rushingYards)");
//			}
//
//			int rushingAttempts = playerRushingOffense.stream().mapToInt(PlayerStatRushingPojo::getRushingAttempt)
//					.sum();
//			if (!teamRushingOffense.getRushingAttempt().equals(rushingAttempts)) {
//				String logInfo = String.format("%s Rushing Attempts: %s | Sum %s Rushing Attempts: %s", off,
//						teamRushingOffense.getRushingAttempt(), off, rushingAttempts);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!homeTeamRushingOffense.getRushingAttempt().equals(rushingAttempts)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * PBP Rushing <> Team Offense Rushing
//	 */
//	private void validateRushingPbpHelper(TeamStatRushingPojo teamRushingOffense,
//			List<PbpPlayerStatRushingPojo> pbpPlayerRushingStat, List<PbpPlayerStatPassingPojo> pbpPlayerPassingStat,
//			String off, String def) {
//		try {
//			Set<String> uniqueNames;
//			int playerPbpStatVal;
//			int playerPbpStatValExt;
//
//			int rushingFirstDownPbp = pbpPlayerRushingStat.stream()
//					.mapToInt(PbpPlayerStatRushingPojo::getRushingFirstDown).sum();
//			if (!teamRushingOffense.getRushingFirstDown().equals(rushingFirstDownPbp)) {
//				loggingUtils.logInfo(String.format("%s Rushing First Downs: %s | PBP Sum %s Rushing First Downs: %s",
//						off, teamRushingOffense.getRushingFirstDown(), off, rushingFirstDownPbp));
//				uniqueNames = pbpPlayerRushingStat.stream().map(PbpPlayerStatRushingPojo::getPlayerName)
//						.collect(Collectors.toSet());
//				loggingUtils.logInfo(PLAYER_STAT_BREAKDOWN);
//				for (String uName : uniqueNames) {
//					playerPbpStatVal = pbpPlayerRushingStat.stream().filter(p -> p.getPlayerName().equals(uName))
//							.mapToInt(PbpPlayerStatRushingPojo::getRushingFirstDown).sum();
//					loggingUtils.logInfo(String.format(NAME_S, uName));
//					loggingUtils.logInfo(String.format(STAT_S, playerPbpStatVal));
//				}
//				throw new IllegalArgumentException(
//						"!homeTeamRushingOffense.getRushingFirstDown().equals(rushingFirstDownPbp)");
//			}
//			int rushingTouchdownsPbp = pbpPlayerRushingStat.stream()
//					.mapToInt(PbpPlayerStatRushingPojo::getRushingTouchdown).sum();
//			if (!teamRushingOffense.getRushingTouchdown().equals(rushingTouchdownsPbp)) {
//				loggingUtils.logInfo(String.format("%s Rushing Touchdown: %s | PBP Sum %s Rushing Touchdown: %s", off,
//						teamRushingOffense.getRushingTouchdown(), off, rushingTouchdownsPbp));
//				uniqueNames = pbpPlayerRushingStat.stream().map(PbpPlayerStatRushingPojo::getPlayerName)
//						.collect(Collectors.toSet());
//				loggingUtils.logInfo(PLAYER_STAT_BREAKDOWN);
//				for (String uName : uniqueNames) {
//					playerPbpStatVal = pbpPlayerRushingStat.stream().filter(p -> p.getPlayerName().equals(uName))
//							.mapToInt(PbpPlayerStatRushingPojo::getRushingTouchdown).sum();
//					loggingUtils.logInfo(String.format(NAME_S, uName));
//					loggingUtils.logInfo(String.format(STAT_S, playerPbpStatVal));
//				}
//				throw new IllegalArgumentException(
//						"!homeTeamRushingOffense.getRushingTouchdown().equals(rushingTouchdownsPbp)");
//			}
//
//			int rushingYardsPbp = pbpPlayerRushingStat.stream().mapToInt(PbpPlayerStatRushingPojo::getRushingYard).sum()
//					+ pbpPlayerPassingStat.stream().mapToInt(PbpPlayerStatPassingPojo::getPassingSackYard).sum();
//			if (!teamRushingOffense.getRushingYard().equals(rushingYardsPbp)) {
//				loggingUtils.logInfo(String.format("%s Rushing Yards: %s | PBP Sum %s Rushing Yards: %s", off,
//						teamRushingOffense.getRushingYard(), off, rushingYardsPbp));
//				uniqueNames = pbpPlayerRushingStat.stream().map(PbpPlayerStatRushingPojo::getPlayerName)
//						.collect(Collectors.toSet());
//				loggingUtils.logInfo(PLAYER_STAT_BREAKDOWN);
//				for (String uName : uniqueNames) {
//					playerPbpStatVal = pbpPlayerRushingStat.stream().filter(p -> p.getPlayerName().equals(uName))
//							.mapToInt(PbpPlayerStatRushingPojo::getRushingYard).sum();
//					playerPbpStatValExt = pbpPlayerPassingStat.stream().filter(p -> p.getPlayerName().equals(uName))
//							.mapToInt(PbpPlayerStatPassingPojo::getPassingSackYard).sum();
//					loggingUtils.logInfo(String.format(NAME_S, uName));
//					loggingUtils.logInfo(String.format(STAT_S, playerPbpStatVal));
//					loggingUtils.logInfo(String.format("   Sacks: %s", playerPbpStatValExt));
//				}
//				throw new IllegalArgumentException("!homeTeamRushingOffense.getRushingYard().equals(rushingYardsPbp)");
//			}
//
//			int rushingAttemptsPbp = pbpPlayerRushingStat.stream().mapToInt(PbpPlayerStatRushingPojo::getRushingAttempt)
//					.sum() + pbpPlayerPassingStat.stream().mapToInt(PbpPlayerStatPassingPojo::getPassingSack).sum();
//			if (!teamRushingOffense.getRushingAttempt().equals(rushingAttemptsPbp)) {
//				loggingUtils.logInfo(String.format("%s Rushing Attempts: %s | PBP Sum %s Rushing Attempts: %s", off,
//						teamRushingOffense.getRushingAttempt(), off, rushingAttemptsPbp));
//				uniqueNames = pbpPlayerRushingStat.stream().map(PbpPlayerStatRushingPojo::getPlayerName)
//						.collect(Collectors.toSet());
//				loggingUtils.logInfo(PLAYER_STAT_BREAKDOWN);
//				for (String uName : uniqueNames) {
//					playerPbpStatVal = pbpPlayerRushingStat.stream().filter(p -> p.getPlayerName().equals(uName))
//							.mapToInt(PbpPlayerStatRushingPojo::getRushingAttempt).sum();
//					playerPbpStatValExt = pbpPlayerPassingStat.stream().filter(p -> p.getPlayerName().equals(uName))
//							.mapToInt(PbpPlayerStatPassingPojo::getPassingSack).sum();
//					loggingUtils.logInfo(String.format(NAME_S, uName));
//					loggingUtils.logInfo(String.format(STAT_S, playerPbpStatVal));
//					loggingUtils.logInfo(String.format("   Sacks: %s", playerPbpStatValExt));
//
//				}
//				throw new IllegalArgumentException(
//						"!homeTeamRushingOffense.getRushingAttempt().equals(rushingAttemptsPbp)");
//			}
//
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Team Defense Rushing <> Team Offense Rushing
//	 */
//	private void validateRushingTeamHelper(TeamStatRushingPojo teamRushingOffense,
//			TeamStatRushingPojo teamRushingDefense, String off, String def) {
//		try {
//
//			if (!teamRushingOffense.getRushingAttempt().equals(teamRushingDefense.getRushingAttempt())) {
//				String logInfo = String.format("%s Offense Rushing Attempts: %s | %s Defense Rushing Attempts: %s", off,
//						teamRushingOffense.getRushingAttempt(), def, teamRushingDefense.getRushingAttempt());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamRushingOffense.getRushingAttempt().equals(teamRushingDefense.getRushingAttempt())");
//			}
//			if (!teamRushingOffense.getRushingYard().equals(teamRushingDefense.getRushingYard())) {
//				String logInfo = String.format("%s Offense Rushing Yards: %s | %s Defense Rushing Yards: %s", off,
//						teamRushingOffense.getRushingYard(), def, teamRushingDefense.getRushingYard());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamRushingOffense.getRushingYard().equals(teamRushingDefense.getRushingYard())");
//			}
//			if (!teamRushingOffense.getRushingTouchdown().equals(teamRushingDefense.getRushingTouchdown())) {
//				String logInfo = String.format("%s Offense Rushing Touchdown: %s | %s Defense Rushing Touchdown: %s",
//						off, teamRushingOffense.getRushingTouchdown(), def, teamRushingDefense.getRushingTouchdown());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamRushingOffense.getRushingTouchdown().equals(teamRushingDefense.getRushingTouchdown())");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Validate Passing/Receiving Stats
//	 */
//	private void validatePassing(List<PlayerStatPassingPojo> playerPassingOffense,
//			TeamStatPassingPojo teamPassingOffense, TeamStatPassingPojo teamPassingDefense,
//			List<PlayerStatReceivingPojo> playerReceivingOffense,
//			List<PbpPlayerStatReceivingPojo> pbpPlayerReceivingStat,
//			List<PbpPlayerStatPassingPojo> pbpPlayerPassingStat, boolean home) {
//		try {
//			String off;
//			String def;
//			if (Boolean.TRUE.equals(home)) {
//				off = "Home";
//				def = "Away";
//			} else {
//				off = "Away";
//				def = "Home";
//			}
//
//			validateReceivingPbpHelper(teamPassingOffense, pbpPlayerReceivingStat, off, def);
//			validateReceivingPlayerHelper(teamPassingOffense, playerReceivingOffense, off, def);
//			validatePassingPbpHelper(teamPassingOffense, pbpPlayerPassingStat, off, def);
//			validatePassingPlayerHelper(teamPassingOffense, playerPassingOffense, off, def);
//			validatePassingTeamHelper(teamPassingOffense, teamPassingDefense, off, def);
//
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * PBP Receiving <> Team Offense Receiving
//	 */
//	private void validateReceivingPbpHelper(TeamStatPassingPojo teamPassingOffense,
//			List<PbpPlayerStatReceivingPojo> pbpPlayerReceivingStat, String off, String def) {
//		try {
//
//			int receivingTouchdownPbp = pbpPlayerReceivingStat.stream()
//					.mapToInt(PbpPlayerStatReceivingPojo::getReceivingTouchdown).sum();
//			if (!teamPassingOffense.getPassingTouchdown().equals(receivingTouchdownPbp)) {
//				String logInfo = String.format("%s Passing Touchdown: %s | PBP Sum %s Receiving Touchdown: %s", off,
//						teamPassingOffense.getPassingTouchdown(), off, receivingTouchdownPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingTouchdown().equals(receivingTouchdownPbp)");
//			}
//			int receivingReceptionsPbp = pbpPlayerReceivingStat.stream()
//					.mapToInt(PbpPlayerStatReceivingPojo::getReceivingReception).sum();
//			if (!teamPassingOffense.getPassingCompletion().equals(receivingReceptionsPbp)) {
//				String logInfo = String.format("%s Passing Completions: %s | PBP Sum %s Receiving Receptions: %s", off,
//						teamPassingOffense.getPassingCompletion(), off, receivingReceptionsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingCompletion().equals(receivingReceptionsPbp)");
//			}
//			int receivingYardPbp = pbpPlayerReceivingStat.stream()
//					.mapToInt(PbpPlayerStatReceivingPojo::getReceivingYard).sum();
//			if (!teamPassingOffense.getPassingYard().equals(receivingYardPbp)) {
//				String logInfo = String.format("%s Passing Yard: %s | PBP Sum %s Receiving Yard: %s", off,
//						teamPassingOffense.getPassingYard(), off, receivingYardPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPassingOffense.getPassingYard().equals(receivingYardPbp)");
//			}
//			int receivingFirstDownPbp = pbpPlayerReceivingStat.stream()
//					.mapToInt(PbpPlayerStatReceivingPojo::getRecievingFirstDown).sum();
//			if (!teamPassingOffense.getPassingFirstDown().equals(receivingFirstDownPbp)) {
//				String logInfo = String.format("%s Receiving First Downs: %s | PBP Sum %s Receiving First Downs: %s",
//						off, teamPassingOffense.getPassingFirstDown(), off, receivingFirstDownPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingFirstDown().equals(receivingFirstDownPbp)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * PBP Passing <> Team Offense Passing
//	 */
//	private void validatePassingPbpHelper(TeamStatPassingPojo teamPassingOffense,
//			List<PbpPlayerStatPassingPojo> pbpPlayerPassingStat, String off, String def) {
//		try {
//
//			int passingFirstDownPbp = pbpPlayerPassingStat.stream()
//					.mapToInt(PbpPlayerStatPassingPojo::getPassingFirstDown).sum();
//			if (!teamPassingOffense.getPassingFirstDown().equals(passingFirstDownPbp)) {
//				String logInfo = String.format("%s Passing First Down: %s | PBP Sum %s Passing First Down: %s", off,
//						teamPassingOffense.getPassingFirstDown(), off, passingFirstDownPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingFirstDown().equals(passingFirstDownPbp)");
//			}
//
//			int passingCompletionsPbp = pbpPlayerPassingStat.stream()
//					.mapToInt(PbpPlayerStatPassingPojo::getPassingCompletion).sum();
//			if (!teamPassingOffense.getPassingCompletion().equals(passingCompletionsPbp)) {
//				String logInfo = String.format("%s Passing Completions: %s | PBP Sum %s Passing Completions: %s", off,
//						teamPassingOffense.getPassingCompletion(), off, passingCompletionsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingCompletion().equals(passingCompletionsPbp)");
//			}
//
//			int passingInterceptionsPbp = pbpPlayerPassingStat.stream()
//					.mapToInt(PbpPlayerStatPassingPojo::getPassingInterception).sum();
//			if (!teamPassingOffense.getPassingInterception().equals(passingInterceptionsPbp)) {
//				String logInfo = String.format("%s Passing Interception: %s | PBP Sum %s Passing Interception: %s", off,
//						teamPassingOffense.getPassingInterception(), off, passingInterceptionsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingInterception().equals(passingInterceptionsPbp)");
//			}
//
//			int passingTouchdownsPbp = pbpPlayerPassingStat.stream()
//					.mapToInt(PbpPlayerStatPassingPojo::getPassingTouchdown).sum();
//			if (!teamPassingOffense.getPassingTouchdown().equals(passingTouchdownsPbp)) {
//				String logInfo = String.format("%s Passing Touchdown: %s | PBP Sum %s Passing Touchdown: %s", off,
//						teamPassingOffense.getPassingTouchdown(), off, passingTouchdownsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingTouchdown().equals(passingTouchdownsPbp)");
//			}
//
//			int passingYardsPbp = pbpPlayerPassingStat.stream().mapToInt(PbpPlayerStatPassingPojo::getPassingYard)
//					.sum();
//			if (!teamPassingOffense.getPassingYard().equals(passingYardsPbp)) {
//				String logInfo = String.format("%s Passing Yards: %s | PBP Sum %s Passing Yards: %s", off,
//						teamPassingOffense.getPassingYard(), off, passingYardsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPassingOffense.getPassingYard().equals(passingYardsPbp)");
//			}
//
//			int passingAttemptsPbp = pbpPlayerPassingStat.stream().mapToInt(PbpPlayerStatPassingPojo::getPassingAttempt)
//					.sum();
//			if (!teamPassingOffense.getPassingAttempt().equals(passingAttemptsPbp)) {
//				String logInfo = String.format("%s Passing Attempts: %s | PBP Sum %s Passing Attempts: %s", off,
//						teamPassingOffense.getPassingAttempt(), off, passingAttemptsPbp);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingAttempt().equals(passingAttemptsPbp)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Player Receiving <> Team Offense Receiving
//	 */
//	private void validateReceivingPlayerHelper(TeamStatPassingPojo teamPassingOffense,
//			List<PlayerStatReceivingPojo> playerReceivingOffense, String off, String def) {
//		try {
//
//			int receivingTouchdown = playerReceivingOffense.stream()
//					.mapToInt(PlayerStatReceivingPojo::getReceivingTouchdown).sum();
//			if (!teamPassingOffense.getPassingTouchdown().equals(receivingTouchdown)) {
//				String logInfo = String.format("%s Passing Touchdown: %s | Sum %s Receiving Touchdown: %s", off,
//						teamPassingOffense.getPassingTouchdown(), off, receivingTouchdown);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingTouchdown().equals(receivingTouchdown)");
//			}
//			int receivingReceptions = playerReceivingOffense.stream()
//					.mapToInt(PlayerStatReceivingPojo::getReceivingReception).sum();
//			if (!teamPassingOffense.getPassingCompletion().equals(receivingReceptions)) {
//				String logInfo = String.format("%s Passing Completions: %s | Sum %s Receiving Receptions: %s", off,
//						teamPassingOffense.getPassingCompletion(), off, receivingReceptions);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingCompletion().equals(receivingReceptions)");
//			}
//			int receivingYard = playerReceivingOffense.stream().mapToInt(PlayerStatReceivingPojo::getReceivingYard)
//					.sum();
//			if (!teamPassingOffense.getPassingYard().equals(receivingYard)) {
//				String logInfo = String.format("%s Passing Yard: %s | Sum %s Receiving Yard: %s", off,
//						teamPassingOffense.getPassingYard(), off, receivingYard);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPassingOffense.getPassingYard().equals(receivingYard)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Player Passing <> Team Offense Passing
//	 */
//	private void validatePassingPlayerHelper(TeamStatPassingPojo teamPassingOffense,
//			List<PlayerStatPassingPojo> playerPassingOffense, String off, String def) {
//		try {
//			int passingCompletions = playerPassingOffense.stream().mapToInt(PlayerStatPassingPojo::getPassingCompletion)
//					.sum();
//			if (!teamPassingOffense.getPassingCompletion().equals(passingCompletions)) {
//				String logInfo = String.format("%s Passing Completions: %s | Sum %s Passing Completions: %s", off,
//						teamPassingOffense.getPassingCompletion(), off, passingCompletions);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingCompletion().equals(passingInterceptions)");
//			}
//
//			int passingInterceptions = playerPassingOffense.stream()
//					.mapToInt(PlayerStatPassingPojo::getPassingInterception).sum();
//			if (!teamPassingOffense.getPassingInterception().equals(passingInterceptions)) {
//				String logInfo = String.format("%s Passing Interception: %s | Sum %s Passing Interception: %s", off,
//						teamPassingOffense.getPassingInterception(), off, passingInterceptions);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingInterception().equals(passingInterceptions)");
//			}
//
//			int passingTouchdowns = playerPassingOffense.stream().mapToInt(PlayerStatPassingPojo::getPassingTouchdown)
//					.sum();
//			if (!teamPassingOffense.getPassingTouchdown().equals(passingTouchdowns)) {
//				String logInfo = String.format("%s Passing Touchdown: %s | Sum %s Passing Touchdown: %s", off,
//						teamPassingOffense.getPassingTouchdown(), off, passingTouchdowns);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingTouchdown().equals(passingTouchdowns)");
//			}
//
//			int passingYards = playerPassingOffense.stream().mapToInt(PlayerStatPassingPojo::getPassingYard).sum();
//			if (!teamPassingOffense.getPassingYard().equals(passingYards)) {
//				String logInfo = String.format("%s Passing Yards: %s | Sum %s Passing Yards: %s", off,
//						teamPassingOffense.getPassingYard(), off, passingYards);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPassingOffense.getPassingYard().equals(passingYards)");
//			}
//
//			int passingAttempts = playerPassingOffense.stream().mapToInt(PlayerStatPassingPojo::getPassingAttempt)
//					.sum();
//			if (!teamPassingOffense.getPassingAttempt().equals(passingAttempts)) {
//				String logInfo = String.format("%s Passing Attempts: %s | Sum %s Passing Attempts: %s", off,
//						teamPassingOffense.getPassingAttempt(), off, passingAttempts);
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException("!teamPassingOffense.getPassingAttempt().equals(passingAttempts)");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//
//	/**
//	 * Team Offense Receiving <> Team Offense Defense
//	 */
//	private void validatePassingTeamHelper(TeamStatPassingPojo teamPassingOffense,
//			TeamStatPassingPojo teamPassingDefense, String off, String def) {
//		try {
//
//			if (!teamPassingOffense.getPassingCompletion().equals(teamPassingDefense.getPassingCompletion())) {
//				String logInfo = String.format(
//						"%s Offense Passing Completions: %s | %s Defense Passing Completions: %s", off,
//						teamPassingOffense.getPassingCompletion(), def, teamPassingDefense.getPassingCompletion());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingCompletion().equals(teamPassingDefense.getPassingCompletion())");
//			}
//			if (!teamPassingOffense.getPassingInterception().equals(teamPassingDefense.getPassingInterception())) {
//				String logInfo = String.format(
//						"%s Offense Passing Interception: %s | %s Defense Passing Interception: %s", off,
//						teamPassingOffense.getPassingInterception(), def, teamPassingDefense.getPassingInterception());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingInterception().equals(teamPassingDefense.getPassingInterception())");
//			}
//			if (!teamPassingOffense.getPassingAttempt().equals(teamPassingDefense.getPassingAttempt())) {
//				String logInfo = String.format("%s Offense Passing Attempts: %s | %s Defense Passing Attempts: %s", off,
//						teamPassingOffense.getPassingAttempt(), def, teamPassingDefense.getPassingAttempt());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingAttempt().equals(teamPassingDefense.getPassingAttempt())");
//			}
//			if (!teamPassingOffense.getPassingYard().equals(teamPassingDefense.getPassingYard())) {
//				String logInfo = String.format("%s Offense Passing Yards: %s | %s Defense Passing Yards: %s", off,
//						teamPassingOffense.getPassingYard(), def, teamPassingDefense.getPassingYard());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingYard().equals(teamPassingDefense.getPassingYard())");
//			}
//			if (!teamPassingOffense.getPassingTouchdown().equals(teamPassingDefense.getPassingTouchdown())) {
//				String logInfo = String.format("%s Offense Passing Touchdown: %s | %s Defense Passing Touchdown: %s",
//						off, teamPassingOffense.getPassingTouchdown(), def, teamPassingDefense.getPassingTouchdown());
//				loggingUtils.logInfo(logInfo);
//				throw new IllegalArgumentException(
//						"!teamPassingOffense.getPassingTouchdown().equals(teamPassingDefense.getPassingTouchdown())");
//			}
//		} catch (Exception e) {
//			loggingUtils.logException(e, e.toString());
//		}
//	}
//}
