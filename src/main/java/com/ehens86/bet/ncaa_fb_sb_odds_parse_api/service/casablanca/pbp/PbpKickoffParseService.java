package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;
import com.google.gson.Gson;

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
			//TODO kick coverage base class
			//TODO remove tackle format name
			//TODO check turnover logic

			if (params.getPlayRawText().startsWith(" kickoff")) {
				params.setPlayRawText(
						String.format("%s, TEAM%s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
								.getSeoName().toUpperCase().replace(" ", ""), params.getPlayRawText()));
				params.getPlay().setPlayText(params.getPlayRawText());
				System.out.println(params.getPlay().getPlayText());
			}
			if (params.getPlayRawText().startsWith("kickoff")) {
				params.setPlayRawText(
						String.format("%s, TEAM %s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
								.getSeoName().toUpperCase().replace(" ", ""), params.getPlayRawText()));
				params.getPlay().setPlayText(params.getPlayRawText());
				System.out.println(params.getPlay().getPlayText());
			}

			if (params.getPlayRawText().equals("TAGER, Pablo kickoff 70 yards to the CP-5, touchback.")) {
				params.setPlayRawText("TAGER, Pablo kickoff 65 yards to the CP0, touchback.");
			}

			params.getDrive().setKickoff(true);
			params.getPlay().setPlayType(PlayTypeEnum.KICKOFF);

			// PlayerStatKickoffPojo kickingStat = new PlayerStatKickoffPojo();
//			String[] playPlayerNames = pbpParsingUtils.extractNames(params.getPlayRawText());
//			String[] playYardLines = pbpParsingUtils.extractYard(params.getPlayRawText());

			// kickingStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[0]));
			// kickingStat.setKickoff(1);
			// params.getPlay().getPlayResult().setPlayResultPoints(0);
			// params.getPlay().getPlayResult().setPlayResultTurnover(false);

			// kickingStat.setKickoffOnsideAttempt(0);
			// kickingStat.setKickoffOutOfBounds(0);
			parseBasic(params);
			parseKickoffReturn(params);
			parseKickReturnStartYard(params);
			parseTouchback(params);
			parseOnsideKick(params);
			parseOutOfBoundsKick(params);
			parseKickoffTackles(params);
			parseKickoffFairCatch(params);
			parseTouchdown(params);
			parseRecover(params);
			parseFumbleForced(params);
			parseMuff(params);
			parseFumble(params);
			reconcileFumbleLost(params);

//			} else if (params.getPlayRawText().toUpperCase().contains("MUFFED")) {
//				/**
//				 * MUFFED
//				 */
//				throw new IllegalArgumentException("Handle case of MUFFED for kickoff");
//			} else if (params.getPlayRawText().toUpperCase().contains("FUMBLE")) {
//				/**
//				 * FUMBLE
//				 */
//				throw new IllegalArgumentException("Handle case of FUMBLE for kickoff");

			validate(params);

		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			System.out.println(params.getDefenseTeam());
			System.out.println(params.getPossessionTeam());
			System.out.println(params.getTeamAbbrevDict());

			// e.printStackTrace();
			// throw new IllegalArgumentException(errorStr);
		}
	}

	private void validate(PbpServiceRequestPojo params) {
		// LOG.log(Level.INFO, "-- Kickoff validation");
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
				.getKickoffLandYard() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffLandYard() == null");
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

		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().isEmpty()) {
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getPlayerName() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getPlayerName() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturn() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturn() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnYard() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnYard() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnTouchdown() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnTouchdown() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFairCatch() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnFairCatch() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnStartYard() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnStartYard() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumble() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnFumble() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumbleLost() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnFumbleLost() == null");
			}

			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturn() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturn() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnTouchdown() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnTouchdown() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFairCatch() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnFairCatch() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnStartYard() > 100 - params.getPlay().getPlayStartYard()) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnStartYard() > 100 - params.getPlay().getPlayStartYard()");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumble() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnFumble() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumbleLost() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "					.getKickReturnFumbleLost() > 1");
			}

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

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
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

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
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

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
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

	private void parseBasic(PbpServiceRequestPojo params) {
		try {
			String kickoffString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("(%s|[aA-zZ]+?)( onside)? kickoff ((-?\\d+) yards.+?to the%s|blocked|BLOCKED)",
							NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
					12);
			String[] kickoffStringArray = kickoffString.split("\\|")[0].split("\\~");
			PlayerStatKickoffPojo kickingStat = new PlayerStatKickoffPojo(
					pbpParsingUtils.formatName(kickoffStringArray[0]));
			kickingStat.setKickoffYard(Integer.valueOf(kickoffStringArray[10]));
			Integer formattedYard = pbpParsingUtils.formatYardLine(kickoffStringArray[11], params.getPossessionTeam(),
					params.getDefenseTeam(), params.getTeamAbbrevDict());
			Integer startingYard = formattedYard - kickingStat.getKickoffYard();
			params.getPlay().setPlayStartYard(startingYard);
			kickingStat.setKickoffLandYard(formattedYard);
			// TODO find by player name here
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickoffReturn(PbpServiceRequestPojo params) {
		try {
//			if (params.getPlayRawText().equals("Jack Culbreath kickoff 62 yards to the DAV3, Mark McCurdy return 21 yards to the DAV24 (Hunter Rice).")) {
//				System.out.println("catch");
//			}
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s (return|for) (-?\\d+) yards? to the%s", NcaaConstants.playerNameRegex,
							NcaaConstants.teamYardRegex))) {
				String kickReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("%s (return|for) (-?\\d+) yards? to the%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex),
						9);
				String playerName = pbpParsingUtils.formatName(kickReturnString.split("\\~")[0]);
				Integer returnYards = Integer.valueOf(kickReturnString.split("\\~")[8]);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickReturnByName(playerName).setKickReturnYard(returnYards);
				// TODO find by player name here
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnYard(returnYards);

			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnYard(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickReturnStartYard(PbpServiceRequestPojo params) {
		try {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.isEmpty()) {
				/**
				 * Add kickoff return start yard
				 */
				String startYard = pbpParsingUtils
						.extractCustom(params.getPlayRawText(),
								String.format("kickoff -?\\d{1,2} yards.+?to the%s", NcaaConstants.teamYardRegex), 1)
						.split("\\~")[0].toUpperCase();
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
						.setKickReturnStartYard(pbpParsingUtils.formatYardLine(startYard, params.getDefenseTeam(),
								params.getPossessionTeam(), params.getTeamAbbrevDict()));
			} else {

			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTouchback(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("kickoff \\d{1,2} yards to the%s, touchback", NcaaConstants.teamYardRegex))) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.applyTouchback();
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffTouchback(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseOnsideKick(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("ONSIDE")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("onside kickoff \\d{1,2} yards, recovered by ([A-Z]*?-?[aA-zZ]{2,3})"))) {
					String onsideString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("onside kickoff \\d{1,2} yards, recovered by ([A-Z]*?-?[aA-zZ]{2,3})"), 1);
					String[] onsideStringArray = onsideString.split("\\|")[0].split("\\~");
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
							.get(0).setKickoffOnsideAttempt(1);
					if (pbpParsingUtils.resolvePossesionTeam(onsideStringArray[0], params.getPossessionTeam(),
							params.getDefenseTeam(), params.getTeamAbbrevDict())) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
								.get(0).setKickoffOnsideSuccess(0);
					} else {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
								.get(0).setKickoffOnsideSuccess(1);
					}
				} else {
					throw new IllegalArgumentException("handle onside kick");
				}

			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOnsideAttempt(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseOutOfBoundsKick(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s kickoff (\\d+) yards to the%s,? out.of.bounds", NcaaConstants.playerNameRegex,
							NcaaConstants.teamYardRegex))) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.applyOutOfBounds();
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOutOfBounds(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickoffTackles(PbpServiceRequestPojo params) {
		try {
			boolean loss = false;
			boolean solo = false;
			if (params.getPlayTackles().length == 1 && "".equals(params.getPlayTackles()[0])) {
				return;
			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()
						&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.getKickReturn().get(0).getKickReturnYard() < 0) {
					loss = true;
				}
				if (params.getPlayTackles().length == 1) {
					solo = true;
				}
				for (String coverageTackleName : params.getPlayTackles()) {
					if (solo) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findKickCoverageByName(pbpParsingUtils.formatName(coverageTackleName))
								.applyTackleSolo(loss);
					} else {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findKickCoverageByName(pbpParsingUtils.formatName(coverageTackleName))
								.applyTackle(loss);
					}
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickoffFairCatch(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FAIR CATCH")) {
				// throw new IllegalArgumentException("handle fair catch");
				/**
				 * Add kickoff return return fair catch stats
				 */
				String returnerName = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format(" fair catch by %s", NcaaConstants.playerNameRegex), 1).split("\\~")[0];

				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickReturnByName(pbpParsingUtils.formatName(returnerName))
						.applyReturnFairCatch(params.getPlay().getPlayerStat().get(params.getPossessionTeam())
								.getSpecialTeam().getKickoff().get(0).getKickoffLandYard());

				/**
				 * Add kickoff fair catch stats
				 */
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.applyFairCatch();
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffFairCatch(0);
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFairCatch(0);
				}

			}
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

	private void parseTouchdown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("TOUCHDOWN")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("%s kickoff (\\d+) yards to the%s,? %s return \\d+ yards to the%s,? ?TOUCHDOWN",
								NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex,
								NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex))) {

					params.getPlay().getPlayResult().setPlayResultPoints(6);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
							.get(0).setKickoffReturnTouchdown(1);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnTouchdown(1);
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("fumble by %s recovered by ([aA-zZ]{2,3}) %s at%s, TOUCHDOWN",
								NcaaConstants.playerNameRegex, NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex))) {
					String fumbleTouchdownString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("fumble by %s recovered by ([aA-zZ]{2,3}) %s at%s, TOUCHDOWN",
									NcaaConstants.playerNameRegex, NcaaConstants.playerNameRegex,
									NcaaConstants.teamYardRegex),
							16);
					String[] fumbleTouchdownStringArray = fumbleTouchdownString.split("\\|")[0].split("\\~");
					if (pbpParsingUtils.resolvePossesionTeam(fumbleTouchdownStringArray[7], params.getPossessionTeam(),
							params.getDefenseTeam(), params.getTeamAbbrevDict())) {
						params.getPlay().getPlayResult().setPlayResultPoints(6);
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
								.get(0).setKickoffReturnTouchdown(0);
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findKickCoverageByName(fumbleTouchdownStringArray[8]).setFumbleTouchdown(1);
					} else {
						throw new IllegalArgumentException("HANDLE RETURN FUMBLE, RECOVER, TOUCHDOWN");
					}
				} else {
					throw new IllegalArgumentException("HANDLE TOUCHDOWN");
				}
			} else {
				params.getPlay().getPlayResult().setPlayResultPoints(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnTouchdown(0);
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnTouchdown(0);
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseRecover(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("RECOVER")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("recovered by ([aA-zZ]{2,3}) %s at( the)?%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex))) {
					String fumbleReturnString = "";
					Integer fumbleReturnYards;
					String returnerFumbleRecoverString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("recovered by ([aA-zZ]{2,3}) %s at( the)?%s", NcaaConstants.playerNameRegex,
									NcaaConstants.teamYardRegex),
							10);
					String abbrev = returnerFumbleRecoverString.split("\\~")[0];
					String recoverName = pbpParsingUtils.formatName(returnerFumbleRecoverString.split("\\~")[1]);

					boolean turnover = pbpParsingUtils.resolvePossesionTeam(abbrev, params.getPossessionTeam(),
							params.getDefenseTeam(), params.getTeamAbbrevDict());

					if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
							String.format("at the%s, returned (-?\\d+) yards? to the%s", NcaaConstants.teamYardRegex,
									NcaaConstants.teamYardRegex))) {
						fumbleReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
								String.format("at the%s, returned (-?\\d+) yards? to the%s",
										NcaaConstants.teamYardRegex, NcaaConstants.teamYardRegex),
								3);
						fumbleReturnYards = Integer.valueOf(fumbleReturnString.split("\\~")[1]);
					} else {
						fumbleReturnYards = 0;
					}
					if (turnover) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findKickCoverageByName(recoverName).applyFumbleRecovery(fumbleReturnYards);
					} else {
						if (fumbleReturnYards > 0) {
							throw new IllegalArgumentException("Handle kickoff return team fumble recover");
						} else {

						}
					}
					params.getPlay().getPlayResult().setPlayResultTurnover(turnover);

				} else {
					throw new IllegalArgumentException("HANDLE NEW RECOVERY");

				}
			} else {
				params.getPlay().getPlayResult().setPlayResultTurnover(false);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseFumbleForced(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FORCED")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format(" at%s forced by %s",
						NcaaConstants.teamYardRegex, NcaaConstants.playerNameRegex))) {
					String fumbleRecoveryString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
							" at%s forced by %s", NcaaConstants.teamYardRegex, NcaaConstants.playerNameRegex), 8);
					String[] fumbleRecoveryStringArray = fumbleRecoveryString.split("\\|")[0].split("\\~");
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
							.findKickCoverageByName(pbpParsingUtils.formatName(fumbleRecoveryStringArray[1]))
							.setFumbleForced(1);
				} else {
					throw new IllegalArgumentException("HANDLE FORCED FUMBLE");
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseFumble(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FUMBLE")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format("((%s fumbled?)|fumbled? by %s)",
						NcaaConstants.playerNameRegex, NcaaConstants.playerNameRegex))) {
					String fumbleString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("((%s fumbled?)|fumbled? by %s)", NcaaConstants.playerNameRegex,
									NcaaConstants.playerNameRegex),
							16);
					String[] fumbleStringArray = fumbleString.split("\\|")[0].split("\\~");
					String fumbleMatch;
					if (!fumbleStringArray[7].equals("null")) {
						fumbleMatch = fumbleStringArray[7];
					} else {
						fumbleMatch = fumbleStringArray[9];
					}
					if (pbpParsingUtils.formatName(fumbleMatch).equals(params.getPlay().getPlayerStat()
							.get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0).getPlayerName())) {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
								.get(0).setKickReturnFumble(1);
					} else {
						throw new IllegalArgumentException("HANDLE FUMBLE -- name does not match");
					}
				} else {
					throw new IllegalArgumentException("HANDLE FUMBLE -- new condition");
				}
			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumble(0);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumbleLost(0);
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseMuff(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("MUFF")) {
				String returnerMuffString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("(return)? muffed by %s at( the)?%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex),
						10);
				String returnerName = returnerMuffString.split("\\~")[1];
				LOG.log(Level.INFO, String.format("Handle return yards after muff-- %s", params.getPlayRawText()));
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickReturnByName(pbpParsingUtils.formatName(returnerName)).applyReturnMuff();
				parseKickReturnStartYard(params);
				parseKickoffFairCatch(params);
			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumble(0);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumbleLost(0);
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void reconcileFumbleLost(PbpServiceRequestPojo params) {
		try {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
					.findKickReturnWithFumble().isEmpty()) {
				if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findKickReturnWithFumble().get(0).setKickReturnFumbleLost(1);
				} else {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findKickReturnWithFumble().get(0).setKickReturnFumbleLost(0);
				}

			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumbleLost(0);
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

}
