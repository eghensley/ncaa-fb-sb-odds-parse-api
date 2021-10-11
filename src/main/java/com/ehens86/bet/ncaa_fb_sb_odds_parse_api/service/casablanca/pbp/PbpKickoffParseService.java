package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpKickoffParseService {
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0";
	private static final String GET_KICKOFF_FAIR_CATCH_0 = "						.getKickoffFairCatch() != 0";
	private static final String GET_KICKOFF_RETURN_YARD_25 = "						.getKickoffReturnYard() != 25";
	private static final String GET_KICKOFF_OUT_OF_BOUNDS_0 = "						.getKickoffOutOfBounds() != 0";
	private static final String GET_KICKOFF_ONSIDE_SUCCESS_NULL = "						.getKickoffOnsideSuccess() != null";
	private static final String GET_KICKOFF_RETURN_TOUCHDOWN_0 = "						.getKickoffReturnTouchdown() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 0";
	private static final String GET_KICKOFF_ONSIDE_ATTEMPT_0 = "						.getKickoffOnsideAttempt() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)\n";
	private static final String ERROR_S_FAILED_WITH_S_INPUT_S = "ERROR: [%s] failed with %s.  Input = %s";

	private static final Logger LOG = Logger.getLogger(PbpKickoffParseService.class.toString());

	private final PbpParsingUtils pbpParsingUtils;

	public PbpKickoffParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public void parseKickoff(PbpServiceRequestPojo params) {

		try {
			params.getDrive().setKickoff(true);
			PlayerStatKickoffPojo kickingStat = new PlayerStatKickoffPojo();
			params.getPlay().setPlayType(PlayTypeEnum.KICKOFF);
			String[] playPlayerNames = pbpParsingUtils.extractNames(params.getPlayRawText());
			String[] playYardLines = pbpParsingUtils.extractYard(params.getPlayRawText());

			kickingStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[0]));
			kickingStat.setKickoff(1);
			params.getPlay().getPlayResult().setPlayResultPoints(0);
			params.getPlay().getPlayResult().setPlayResultTurnover(false);

			/**
			 * ONSIDE KICK
			 */
			if (params.getPlayRawText().toUpperCase().contains("ONSIDE")) {
				parseOnsideKick(params, playYardLines, kickingStat, playPlayerNames);
				validate(params);
				return;
			}

			String kickoffString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"%s kickoff (\\d+) yards to the [A-Z]*?-?([aA-zZ]{2,3}\\s?\\d{1,2})", playPlayerNames[0]), 2);
			String[] kickoffStringArray = kickoffString.split("\\|")[0].split("\\~");
			kickingStat.setKickoffYard(Integer.valueOf(kickoffStringArray[0]));
			Integer formattedYard = pbpParsingUtils.formatYardLine(kickoffStringArray[1], params.getPossessionTeam(),
					params.getDefenseTeam(), params.getTeamAbbrevDict());
			Integer startingYard = formattedYard - kickingStat.getKickoffYard();
			params.getPlay().setPlayStartYard(startingYard);

			/**
			 * OUT OF BOUNDS
			 */
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format(
					"%s kickoff (\\d+) yards to the ([aA-zZ]{2,3}\\d{1,2}),? out.of.bounds", playPlayerNames[0]))) {
				parseOutOfBoundsKick(params, playYardLines, kickingStat, playPlayerNames);
				validate(params);
				return;
			}

			kickingStat.setKickoffOnsideAttempt(0);
			kickingStat.setKickoffOutOfBounds(0);

			if (params.getPlayRawText().toUpperCase().contains("TOUCHBACK")) {
				/**
				 * TOUCHBACK
				 */
				parseTouchback(params, playYardLines, kickingStat, playPlayerNames);
			} else if (params.getPlayRawText().toUpperCase().contains("FAIR CATCH")) {
				/**
				 * FAIR CATCH
				 */
				parseKickoffFairCatch(params, playYardLines, kickingStat, playPlayerNames);
			} else if (params.getPlayRawText().toUpperCase().contains("TOUCHDOWN")) {
				/**
				 * TOUCHDOWN
				 */
				parseTouchdown(params, playYardLines, kickingStat, playPlayerNames);
			} else if (params.getPlayRawText().toUpperCase().contains("MUFFED")) {
				/**
				 * MUFFED
				 */
				throw new IllegalArgumentException("Handle case of MUFFED for kickoff");
			} else if (params.getPlayRawText().toUpperCase().contains("RECOVER")) {
				/**
				 * RECOVERED
				 */
				parseRecover(params, playYardLines, kickingStat, playPlayerNames);
			} else if (params.getPlayRawText().toUpperCase().contains("FUMBLE")) {
				/**
				 * FUMBLE
				 */
				throw new IllegalArgumentException("Handle case of FUMBLE for kickoff");
			} else if (params.getPlayRawText().toUpperCase().contains("RETURN")) {
				/**
				 * STANDARD RETURN
				 */
				parseKickoffReturn(params, playYardLines, kickingStat, playPlayerNames);
			} else {
				/**
				 * OTHER?
				 */
				throw new IllegalArgumentException("Handle case of OTHER for kickoff");
			}

			//validate(params);

		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void validate(PbpServiceRequestPojo params) {
		LOG.log(Level.INFO, "-- Kickoff validation");
		if (!params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
		}

		if (params.getPlay().getPlayStartDown() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartDown() != null");
		}
		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
		}
		if (params.getPlay().getPlayYardToGain() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() != null");
		}
		if (params.getPlay().getPlayCallType() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != null");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() != null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown() != null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().size() != 1");
		}
		if (StringUtils.isBlank(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
				.getKickoff().get(0).getPlayerName())) {
			throw new IllegalArgumentException(
					"StringUtils.isBlank(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0).getPlayerName())");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffYard() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffTouchback() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffTouchback() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOnsideAttempt() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOnsideAttempt() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffReturnYard() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffReturnYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOutOfBounds() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOutOfBounds() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffReturnTouchdown() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffReturnTouchdown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffFairCatch() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffFairCatch() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoff() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoff() == null");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffTouchback() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffTouchback() > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOnsideAttempt() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOnsideAttempt() > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOutOfBounds() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOutOfBounds() > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffReturnTouchdown()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffFairCatch() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffFairCatch()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoff() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoff() > 1");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffTouchback() == 1) {
			/**
			 * TOUCHBACK
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnYard() != 25) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		}

		else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffFairCatch() == 1) {
			/**
			 * FAIR CATCH
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnYard() != 25 - (100 - (params.getPlay().getPlayStartYard()
							+ params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.getKickoff().get(0).getKickoffYard()))) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffTouchback() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ "						.getKickoffTouchback() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.size() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFairCatch() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0).getKickReturnFairCatch() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnStartYard()
					+ params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).getKickReturnYard() != 25) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "						.getKickReturnStartYard()\n"
								+ "						+ params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "								.getKickReturnYard() != 25");
			}
		}

		else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOutOfBounds() == 1) {
			/**
			 * OUT OF BOUNDS
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnYard() != 35) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ "						.getKickoffReturnYard() != 35");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffTouchback() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ "						.getKickoffTouchback() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		}

		else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOnsideAttempt() == 1) {
			/**
			 * ONSIDE
			 */

			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != null) {
				LOG.log(Level.WARNING,
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
							.get(0).getKickoffReturnYard() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0
					&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		}

	}

	private void parseRecover(PbpServiceRequestPojo params, String[] playYardLines, PlayerStatKickoffPojo kickingStat,
			String[] playPlayerNames) {
		try {
			boolean loss = false;
			boolean turnover;
			String kickoffReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"%s return (\\d+) yards to the [A-Z]*?-?([aA-zZ]{2,3}\\d{1,2}), fumble by (((Van )?[A-Z][aA-zZ']+ ?[A-Z]{0,2}\\.?,.?[aA-zZ][aA-zZ'\\.]+)|([A-Z]([A-Z]|[a-z]+) [A-Z][a-z]+)) recovered by (([A-Z]*?)( TEAM)?) at [A-Z]*?-?([aA-zZ]{2,3}\\d{1,2}).",
					playPlayerNames[1]), 11);
			String[] kickReturnStringSplit = kickoffReturnString.split("\\~");

			PlayerStatKickReturnPojo returnStat = new PlayerStatKickReturnPojo();
			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
			returnStat.setKickReturn(1);
			returnStat.setKickReturnStartYard(100 - pbpParsingUtils.formatYardLine(playYardLines[0],
					params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			returnStat.setKickReturnYard(Integer.valueOf(kickReturnStringSplit[0]));
			returnStat.setKickReturnFairCatch(0);
			returnStat.setKickReturnFumble(1);

			if (pbpParsingUtils.resolvePossesionTeam(kickReturnStringSplit[8], params.getPossessionTeam(),
					params.getDefenseTeam(), params.getTeamAbbrevDict())) {
				turnover = false;
				returnStat.setKickReturnFumbleLost(0);
			} else {
				turnover = true;
				returnStat.setKickReturnFumbleLost(1);
			}

			kickingStat.setKickoffReturnYard(returnStat.getKickReturnYard());
			kickingStat.setKickoffTouchback(0);
			kickingStat.setKickoffFairCatch(0);
			kickingStat.setKickoffReturnTouchdown(0);

			if (returnStat.getKickReturnYard() < 0) {
				loss = true;
			}

			if (params.getPlayTackles().length == 1 && "".equals(params.getPlayTackles()[0])) {
				throw new IllegalArgumentException(
						"params.getPlayTackles().length == 1 && \"\".equals(params.getPlayTackles()[0])");

			} else if (params.getPlayTackles().length == 1 && " TEAM".equals(kickReturnStringSplit[9])) {
				PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
				kickCoverage.setPlayerName(pbpParsingUtils.formatName(params.getPlayTackles()[0]));
				kickCoverage.setTackleSolo(1.0);
				kickCoverage.setTackleAssist(0.0);
				kickCoverage.setTackleTotal(1);
				kickCoverage.setFumbleYard(0);
				kickCoverage.setFumbleRecovered(1);
				if (loss) {
					kickCoverage.setTackleForLoss(1);
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
						.add(kickCoverage);
			} else if (params.getPlayTackles().length == 1) {
				throw new IllegalArgumentException("Handle tackle on fumble case");

//				PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
//				kickCoverage.setPlayerName(pbpParsingUtils.formatName(params.getPlayTackles()[0]));
//				kickCoverage.setTackleSolo(1.0);
//				kickCoverage.setTackleAssist(0.0);
//				kickCoverage.setTackleTotal(1);
//				if (loss) {
//					kickCoverage.setTackleForLoss(1);
//				}
//				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().add(kickCoverage);

			} else {
				throw new IllegalArgumentException("Handle tackle on fumble case");

//				for (String coverageTackle : params.getPlayTackles()) {
//					PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
//					kickCoverage.setPlayerName(pbpParsingUtils.formatName(coverageTackle));
//					kickCoverage.setTackleTotal(1);
//					kickCoverage.setTackleAssist(1.0);
//					if (loss) {
//						kickCoverage.setTackleForLoss(1);
//					}
//					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().add(kickCoverage);
//				}
			}
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.add(returnStat);
			params.getPlay().getPlayResult().setPlayResultTurnover(turnover);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTouchdown(PbpServiceRequestPojo params, String[] playYardLines, PlayerStatKickoffPojo kickingStat,
			String[] playPlayerNames) {
		try {
			kickingStat.setKickoffTouchback(0);
			kickingStat.setKickoffFairCatch(0);
			kickingStat.setKickoffReturnTouchdown(1);

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
			params.getPlay().getPlayResult().setPlayResultPoints(6);
			throw new IllegalArgumentException("HANDLE TOUCHDOWN");

		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTouchback(PbpServiceRequestPojo params, String[] playYardLines, PlayerStatKickoffPojo kickingStat,
			String[] playPlayerNames) {
		try {
			kickingStat.setKickoffTouchback(1);
			kickingStat.setKickoffFairCatch(0);
			kickingStat.setKickoffReturnYard(25);

			kickingStat.setKickoffReturnTouchdown(0);
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseOutOfBoundsKick(PbpServiceRequestPojo params, String[] playYardLines,
			PlayerStatKickoffPojo kickingStat, String[] playPlayerNames) {
		try {
			kickingStat.setKickoffOutOfBounds(1);
			kickingStat.setKickoffOnsideAttempt(0);
			kickingStat.setKickoffFairCatch(0);
			kickingStat.setKickoffTouchback(0);
			kickingStat.setKickoffReturnTouchdown(0);
			kickingStat.setKickoffReturnYard(35);

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseOnsideKick(PbpServiceRequestPojo params, String[] playYardLines,
			PlayerStatKickoffPojo kickingStat, String[] playPlayerNames) {
		try {
			String onsideString;
			boolean kickingRecovery = false;
			String[] onsideStringArray;

			kickingStat.setKickoffOnsideAttempt(1);
			kickingStat.setKickoffOutOfBounds(0);
			kickingStat.setKickoffTouchback(0);
			kickingStat.setKickoffFairCatch(0);
			kickingStat.setKickoffReturnTouchdown(0);

			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s onside kickoff (\\d+) yards to the ([aA-zZ]{3}\\d{2})", playPlayerNames[0]))) {
				onsideString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("%s onside kickoff (\\d+) yards to the ([aA-zZ]{3}\\d{2})", playPlayerNames[0]),
						2);
				onsideStringArray = onsideString.split("\\|")[0].split("\\~");
				kickingRecovery = true;
				kickingStat.setKickoffReturnYard(0);
				kickingStat.setKickoffOnsideSuccess(1);
			} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format(
					"%s onside kickoff (\\d+) yards, recovered by ([a-zA-Z]{2,3}\\s?)  at the ([a-zA-Z]{2,3}\\s?\\d{1,3}),  return (\\d) yards to the ([a-zA-Z]{2,3}\\s?\\d{1,3}) , out of bounds",
					playPlayerNames[0]))) {
				onsideString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
						"%s onside kickoff (\\d+) yards, recovered by ([a-zA-Z]{2,3}\\s?)  at the ([a-zA-Z]{2,3}\\s?\\d{1,3}),  return (\\d) yards to the ([a-zA-Z]{2,3}\\s?\\d{1,3}) , out of bounds",
						playPlayerNames[0]), 4);
				onsideStringArray = onsideString.split("\\|")[0].split("\\~");
				kickingStat.setKickoffReturnYard(Integer.valueOf(onsideStringArray[3]));
				kickingStat.setKickoffOnsideSuccess(0);
			} else {
				onsideString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
						"%s onside kickoff (\\d+) yards, recovered by ([a-zA-Z]{2,3}\\s?) ([aA-zZ][aA-zZ]+,.?[aA-zZ][aA-zZ]+) at the ([a-zA-Z]{2,3}\\s?\\d{1,3}), ([aA-zZ][aA-zZ]+,.?[aA-zZ][aA-zZ]+) return (\\d) yards to the ([a-zA-Z]{2,3}\\s?\\d{1,3})",
						playPlayerNames[0]), 7);
				onsideStringArray = onsideString.split("\\|")[0].split("\\~");
				kickingStat.setKickoffReturnYard(Integer.valueOf(onsideStringArray[5]));
				kickingStat.setKickoffOnsideSuccess(0);

				PlayerStatKickReturnPojo returnStat = new PlayerStatKickReturnPojo();
				returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
				returnStat.setKickReturn(1);
				returnStat.setKickReturnFumble(0);

				returnStat.setKickReturnStartYard(100 - pbpParsingUtils.formatYardLine(playYardLines[0],
						params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
				returnStat.setKickReturnYard(kickingStat.getKickoffReturnYard());

				returnStat.setKickReturnFairCatch(0);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.add(returnStat);

			}

			kickingStat.setKickoffYard(Integer.valueOf(onsideStringArray[0]));

			params.getPlay()
					.setPlayStartYard(pbpParsingUtils.formatYardLine(playYardLines[0], params.getPossessionTeam(),
							params.getDefenseTeam(), params.getTeamAbbrevDict()) - kickingStat.getKickoffYard());
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
			params.getPlay().getPlayResult().setPlayResultTurnover(kickingRecovery);
			String warningStr = String.format("WARNING: NEED TO HANDLE EVALUATING ONSIDE KICK SUCCESS/FAILURE");
			LOG.log(Level.WARNING, warningStr);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickoffFairCatch(PbpServiceRequestPojo params, String[] playYardLines,
			PlayerStatKickoffPojo kickingStat, String[] playPlayerNames) {
		try {
			PlayerStatKickReturnPojo returnStat = new PlayerStatKickReturnPojo();
			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
			returnStat.setKickReturn(1);
			returnStat.setKickReturnStartYard(100 - pbpParsingUtils.formatYardLine(playYardLines[0],
					params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			returnStat.setKickReturnYard(25 - returnStat.getKickReturnStartYard());
			returnStat.setKickReturnFumble(0);

			returnStat.setKickReturnFairCatch(1);
			kickingStat.setKickoffTouchback(0);

			kickingStat.setKickoffFairCatch(1);
			kickingStat.setKickoffReturnYard(returnStat.getKickReturnYard());
			kickingStat.setKickoffReturnTouchdown(0);

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.add(returnStat);

			// throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickoffReturn(PbpServiceRequestPojo params, String[] playYardLines,
			PlayerStatKickoffPojo kickingStat, String[] playPlayerNames) {
		try {
			boolean loss = false;
			String kickoffReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("%s return (\\d+) yards to the [A-Z]*?-?([aA-zZ]{2,3}\\d{1,2})", playPlayerNames[1]),
					2);
			PlayerStatKickReturnPojo returnStat = new PlayerStatKickReturnPojo();
			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
			returnStat.setKickReturn(1);
			returnStat.setKickReturnStartYard(100 - pbpParsingUtils.formatYardLine(playYardLines[0],
					params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			returnStat.setKickReturnYard(Integer.valueOf(kickoffReturnString.split("\\~")[0]));
			returnStat.setKickReturnFairCatch(0);
			returnStat.setKickReturnFumble(0);

			kickingStat.setKickoffReturnYard(returnStat.getKickReturnYard());
			kickingStat.setKickoffTouchback(0);
			kickingStat.setKickoffFairCatch(0);
			kickingStat.setKickoffReturnTouchdown(0);

			if (returnStat.getKickReturnYard() < 0) {
				loss = true;
			}

			if (params.getPlayTackles().length == 1 && "".equals(params.getPlayTackles()[0])) {

			} else if (params.getPlayTackles().length == 1) {
				PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
				kickCoverage.setPlayerName(pbpParsingUtils.formatName(params.getPlayTackles()[0]));
				kickCoverage.setTackleSolo(1.0);
				kickCoverage.setTackleAssist(0.0);
				kickCoverage.setTackleTotal(1);
				if (loss) {
					kickCoverage.setTackleForLoss(1);
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
						.add(kickCoverage);

			} else {
				for (String coverageTackle : params.getPlayTackles()) {
					PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
					kickCoverage.setPlayerName(pbpParsingUtils.formatName(coverageTackle));
					kickCoverage.setTackleTotal(1);
					kickCoverage.setTackleAssist(1.0);
					if (loss) {
						kickCoverage.setTackleForLoss(1);
					}
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
							.add(kickCoverage);
				}
			}
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.add(returnStat);

		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
}
