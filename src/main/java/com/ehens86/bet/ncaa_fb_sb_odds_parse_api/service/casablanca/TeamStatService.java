package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.teamstats.TeamStatsPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.teamstats.TeamStatsStatPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.teamstats.TeamStatsTeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.ParsingUtils;

@Service
public class TeamStatService {
	private static final String RUSHING = "Rushing";

	private static final String RUSHING2 = RUSHING;

	private static final String PASSING = "Passing";

	private static final String ATTEMPTS = "Attempts";

	private static final Logger LOG = Logger.getLogger(TeamStatService.class.toString());

	private final ParsingUtils parsingUtils;

	public TeamStatService(ParsingUtils parsingUtils) {
		this.parsingUtils = parsingUtils;
	}

	public GamePojo parseTeamStats(TeamStatsPojo teamStatsRaw, GamePojo game) {

		game.getTeamAway().setNcaaTeamId(teamStatsRaw.pullAwayTeam().getId());
		game.getTeamHome().setNcaaTeamId(teamStatsRaw.pullHomeTeam().getId());

		try {
			for (TeamStatsTeamPojo teamRaw : teamStatsRaw.getTeams()) {
				parseTeamStatHelper(game, teamRaw);
			}

			return game;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s", ste[1].getMethodName(), e.toString());
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTeamStatHelper(GamePojo game, TeamStatsTeamPojo teamRaw) {
		try {
			TeamPojo offense = game.pullTeamById(teamRaw.getTeamId());
			TeamPojo defense = game.pullOpponentById(teamRaw.getTeamId());

			for (TeamStatsStatPojo stat : teamRaw.getStats()) {
				if (RUSHING2.equals(stat.getStat())) {
					offense.getTeamStat().getOffense().getOffenseRushing()
							.setRushingYard(Integer.valueOf(stat.getData()));
					offense.getTeamStat().getOffense().getOffenseRushing()
							.setRushingAttempt(Integer.valueOf(stat.pullValueByKey(ATTEMPTS)));

					defense.getTeamStat().getDefense().getDefenseRushing()
							.setRushingYard(Integer.valueOf(stat.getData()));
					defense.getTeamStat().getDefense().getDefenseRushing()
							.setRushingAttempt(Integer.valueOf(stat.pullValueByKey(ATTEMPTS)));
				} else if ("1st Downs".equals(stat.getStat())) {
					offense.getTeamStat().getOffense().getOffenseRushing()
							.setRushingFirstDown(Integer.valueOf(stat.pullValueByKey(RUSHING2)));
					offense.getTeamStat().getOffense().getOffensePassing()
							.setPassingFirstDown(Integer.valueOf(stat.pullValueByKey(PASSING)));
					offense.getTeamStat().getOffense()
							.setOffenseFirstDownPenalty(Integer.valueOf(stat.pullValueByKey("Penalty")));

					defense.getTeamStat().getDefense().getDefenseRushing()
							.setRushingFirstDown(Integer.valueOf(stat.pullValueByKey(RUSHING2)));
					defense.getTeamStat().getDefense().getDefensePassing()
							.setPassingFirstDown(Integer.valueOf(stat.pullValueByKey(PASSING)));
					defense.getTeamStat().getDefense()
							.setDefenseFirstDownPenalty(Integer.valueOf(stat.pullValueByKey("Penalty")));
				} else if (PASSING.equals(stat.getStat())) {
					offense.getTeamStat().getOffense().getOffensePassing()
							.setPassingYard(Integer.valueOf(stat.getData()));
					offense.getTeamStat().getOffense().getOffensePassing()
							.setPassingAttempt(Integer.valueOf(stat.pullValueByKey(ATTEMPTS)));
					offense.getTeamStat().getOffense().getOffensePassing()
							.setPassingCompletion(Integer.valueOf(stat.pullValueByKey("Completions")));
					offense.getTeamStat().getOffense().getOffensePassing()
							.setPassingInterception(Integer.valueOf(stat.pullValueByKey("Interceptions")));

					defense.getTeamStat().getDefense().getDefensePassing()
							.setPassingYard(Integer.valueOf(stat.getData()));
					defense.getTeamStat().getDefense().getDefensePassing()
							.setPassingAttempt(Integer.valueOf(stat.pullValueByKey(ATTEMPTS)));
					defense.getTeamStat().getDefense().getDefensePassing()
							.setPassingCompletion(Integer.valueOf(stat.pullValueByKey("Completions")));
					defense.getTeamStat().getDefense().getDefensePassing()
							.setPassingInterception(Integer.valueOf(stat.pullValueByKey("Interceptions")));
				} else if ("Fumbles: Number-Lost".equals(stat.getStat())) {
					offense.getTeamStat().getOffense().getOffenseFumble()
							.setFumble(parsingUtils.splitValue(stat.getData(), "-", 0));
					offense.getTeamStat().getOffense().getOffenseFumble()
							.setFumbleLost(parsingUtils.splitValue(stat.getData(), "-", 1));

					defense.getTeamStat().getDefense().getDefenseFumble()
							.setFumble(parsingUtils.splitValue(stat.getData(), "-", 0));
					defense.getTeamStat().getDefense().getDefenseFumble()
							.setFumbleLost(parsingUtils.splitValue(stat.getData(), "-", 1));
				} else if ("Interception Returns: Number-Yards".equals(stat.getStat())) {
					offense.getTeamStat().getOffense().getOffensePassing()
							.setPassingInterceptionYard(parsingUtils.splitValue(stat.getData(), "-", 1));

					defense.getTeamStat().getDefense().getDefensePassing()
							.setPassingInterceptionYard(parsingUtils.splitValue(stat.getData(), "-", 1));
				} else if ("Third-Down Conversions".equals(stat.getStat())) {
					offense.getTeamStat().getOffense().getOffenseConversion()
							.setThirdDownAttempt(parsingUtils.splitValue(stat.getData(), "-", 1));
					offense.getTeamStat().getOffense().getOffenseConversion()
							.setThirdDownConversion(parsingUtils.splitValue(stat.getData(), "-", 0));

					defense.getTeamStat().getDefense().getDefenseConversion()
							.setThirdDownAttempt(parsingUtils.splitValue(stat.getData(), "-", 1));
					defense.getTeamStat().getDefense().getDefenseConversion()
							.setThirdDownConversion(parsingUtils.splitValue(stat.getData(), "-", 0));
				} else if ("Fourth-Down Conversions".equals(stat.getStat())) {
					offense.getTeamStat().getOffense().getOffenseConversion()
							.setFourthDownAttempt(parsingUtils.splitValue(stat.getData(), "-", 1));
					offense.getTeamStat().getOffense().getOffenseConversion()
							.setFourthDownConversion(parsingUtils.splitValue(stat.getData(), "-", 0));

					defense.getTeamStat().getDefense().getDefenseConversion()
							.setFourthDownAttempt(parsingUtils.splitValue(stat.getData(), "-", 1));
					defense.getTeamStat().getDefense().getDefenseConversion()
							.setFourthDownConversion(parsingUtils.splitValue(stat.getData(), "-", 0));
				} else if ("Penalties: Number-Yards".equals(stat.getStat())) {
					offense.getTeamStat().getPenalty().setPenalty(parsingUtils.splitValue(stat.getData(), "-", 0));
					offense.getTeamStat().getPenalty().setPenaltyYards(parsingUtils.splitValue(stat.getData(), "-", 1));
				} else if ("Punting: Number-Yards".equals(stat.getStat())) {
					offense.getTeamStat().getSpecialTeam().getPunt()
							.setPunt(parsingUtils.splitValue(stat.getData(), "-", 0));
					offense.getTeamStat().getSpecialTeam().getPunt()
							.setPuntYards(parsingUtils.splitValue(stat.getData(), "-", 1));

					defense.getTeamStat().getSpecialTeam().getPuntReturn()
							.setPunt(parsingUtils.splitValue(stat.getData(), "-", 0));
					defense.getTeamStat().getSpecialTeam().getPuntReturn()
							.setPuntYards(parsingUtils.splitValue(stat.getData(), "-", 1));
				} else if ("Punt Returns: Number-Yards".equals(stat.getStat())) {
					offense.getTeamStat().getSpecialTeam().getPuntReturn()
							.setPuntReturnYards(parsingUtils.splitValue(stat.getData(), "-", 1));

					defense.getTeamStat().getSpecialTeam().getPunt()
							.setPuntReturnYards(parsingUtils.splitValue(stat.getData(), "-", 1));
				} else if ("Kickoff Returns: Number-Yards".equals(stat.getStat())) {
					offense.getTeamStat().getSpecialTeam().getKickoffReturn()
							.setKickoffReturnYards(parsingUtils.splitValue(stat.getData(), "-", 1));
					offense.getTeamStat().getSpecialTeam().getKickoffReturn()
							.setKickoff(parsingUtils.splitValue(stat.getData(), "-", 0));

					offense.getTeamStat().getSpecialTeam().getKickoff()
							.setKickoffReturnYards(parsingUtils.splitValue(stat.getData(), "-", 1));
					offense.getTeamStat().getSpecialTeam().getKickoff()
							.setKickoff(parsingUtils.splitValue(stat.getData(), "-", 0));
				} else if ("Total Offense".equals(stat.getStat())) {
					// Not currently needed
				} else {
					String logInfoStr = String.format("Stat not found: %s", stat.getStat());
					LOG.log(Level.INFO, logInfoStr);
					throw new IllegalArgumentException(logInfoStr);
				}

			}

			game.addTeamById(defense);
			game.addTeamById(offense);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s", ste[1].getMethodName(), e.toString());
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}
}
