package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;

@Service
public class PbpValidateService {
	private static final Logger LOG = Logger.getLogger(PbpValidateService.class.toString());
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0";
	private static final String GET_KICKOFF_FAIR_CATCH_0 = "						.getKickoffFairCatch() != 0";
	private static final String GET_KICKOFF_RETURN_YARD_25 = "						.getKickoffReturnYard() != 25";
	private static final String GET_KICKOFF_OUT_OF_BOUNDS_0 = "						.getKickoffOutOfBounds() != 0";
	private static final String GET_KICKOFF_ONSIDE_SUCCESS_NULL = "						.getKickoffOnsideSuccess() != null";
	private static final String GET_KICKOFF_RETURN_TOUCHDOWN_0 = "						.getKickoffReturnTouchdown() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 0";
	private static final String GET_KICKOFF_ONSIDE_ATTEMPT_0 = "						.getKickoffOnsideAttempt() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)\n";

	public void validate(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayStartDown() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayStartDown() == null");
			}
			if (params.getPlay().getPlayStartYard() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
			}
			if (params.getPlay().getPlayTempo() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayTempo() == null");
			}
			if (params.getPlay().getNoPlayPenalty() == null) {
				throw new IllegalArgumentException("params.getPlay().getNoPlayPenalty() == null");
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
					&& params.getPlay().getPlayType() != PlayTypeEnum.PENALTY
					&& params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT
					&& params.getPlay().getPlayYardToGain() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() == null");
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF && params.getPlay().getPlayCallType() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayCallType() == null");
			}
			// TODO how to do punt/fg play result??
			if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT
					&& params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
					&& params.getPlay().getPlayCallType() != PlayCallTypeEnum.FG
					&& params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT
					&& params.getPlay().getPlayResult().getPlayResultYard() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() == null");
			}
			if (params.getPlay().getPlayResult().isPlayResultFirstDown() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown() == null");
			}
			if (params.getPlay().getPlayResult().getPlayResultYardLine() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() == null");
			}
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				validateRush(params);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				validatePunt(params);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				validateReceiver(params);
				validatePass(params);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG) {
				validateFieldGoal(params);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				validatePat(params);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				validateKickoff(params);
			}
			validateOffense(params);
			validatePenalty(params);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void validatePenalty(PbpServiceRequestPojo params) {
		List<TeamStatPenaltyPojo> offensePenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
		List<TeamStatPenaltyPojo> defensePenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();

		for (TeamStatPenaltyPojo offPen : offensePenalty) {
			if (offPen.getPenaltyFirstDown() == 1) {
				throw new IllegalArgumentException("offPen.getPenaltyFirstDown() == 1"); // && offPen.getPenaltyName()
																							// !=
																							// PenaltyEnum.ROUGHING_THE_KICKER
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() > 0) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() > 0");
			}
		}
		for (TeamStatPenaltyPojo defPen : defensePenalty) {
			if (defPen.getPenaltyFirstDown() == 1 && !params.getPlay().getPlayResult().isPlayResultFirstDown()) {
				throw new IllegalArgumentException(
						"defPen.getPenaltyFirstDown() == 1 && !params.getPlay().getPlayResult().isPlayResultFirstDown()");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() < 0) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() < 0");
			}
		}
	}

	private void validateKickoff(PbpServiceRequestPojo params) {

		List<PlayerStatDefenseProductionPojo> coverage = params.getPlay().getPlayerStat()
				.get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage();

		for (PlayerStatDefenseProductionPojo cov : coverage) {
			validateDef(params, cov);
		}

		if (!params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}

		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
		}
		if (params.getPlay().getPlayYardToGain() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() != null");
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.NA) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.NA");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() != null");
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
				.getKickoffOnsideSuccess() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOnsideSuccess() == null");
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
					.getKickoffOnsideSuccess() != 0) {
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
					.getKickoffOnsideSuccess() != 0) {
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
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0) {
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
//			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
//					.getKickoffOnsideSuccess() != null) {
//				LOG.log(Level.WARNING,
//						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
//								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
//			}
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

	private void validateReceiver(PbpServiceRequestPojo params) {
		List<TeamStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();
		List<TeamStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
		PlayerStatReceivingPojo receiver;
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
				.isEmpty()) {
			receiver = null;
		} else {
			receiver = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.get(0);
		}
		if (Objects.nonNull(receiver)) {
			if (receiver.getPlayerName().isEmpty()) {
				throw new IllegalArgumentException("receiver.getPlayerName().isEmpty()");
			}
			if (receiver.getReceivingReception() == null) {
				throw new IllegalArgumentException("receiver.getReceivingReception() == null");
			}
			if (receiver.getReceivingSafety() == null) {
				throw new IllegalArgumentException("receiver.getReceivingSafety() == null");
			}
			if (receiver.getReceivingTarget() == null) {
				throw new IllegalArgumentException("receiver.getReceivingTarget() == null");
			}
			if (receiver.getReceivingYard() == null) {
				throw new IllegalArgumentException("receiver.getReceivingYard() == null");
			}
			if (receiver.getReceivingTouchdown() == null) {
				throw new IllegalArgumentException("receiver.getReceivingTouchdown() == null");
			}
			if (receiver.getReceivingDrop() == null) {
				throw new IllegalArgumentException("receiver.getReceivingDrop() == null");
			}
//			if (receiver.getReceivingYardAfterCatch() == null) {
//				throw new IllegalArgumentException("receiver.getReceivingYardAfterCatch() == null");
//			}	
			if (receiver.getRecievingFirstDown() == null) {
				throw new IllegalArgumentException("receiver.getRecievingFirstDown() == null");
			}
			if (receiver.getReceivingFumble() == null) {
				throw new IllegalArgumentException("receiver.getReceivingFumble() == null");
			}
			if (receiver.getReceivingFumbleLost() == null) {
				throw new IllegalArgumentException("receiver.getReceivingFumbleLost() == null");
			}
			if (receiver.getReceivingTwoPointConversion() == null) {
				throw new IllegalArgumentException("receiver.getReceivingTwoPointConversion() == null");
			}
			if (receiver.getReceivingTwoPointConversion() == 1) {
				if (params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
					throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != 2");
				}
				if (params.getPlay().getPlayType() != PlayTypeEnum.PAT) {
					throw new IllegalArgumentException("params.getPlay().getPlayType() != PlayTypeEnum.PAT");
				}
				if (receiver.getReceivingTwoPointConversion() != 1) {
					throw new IllegalArgumentException("receiver.getReceivingTwoPointConversion() != 1");
				}
				if (receiver.getReceivingYard() < 3) {
					throw new IllegalArgumentException("receiver.getReceivingYard() < 3");
				}
			}
			if (!defPenalty.isEmpty()) {
				if (defPenalty.get(0).getPenaltyYards() + receiver.getReceivingYard() != params.getPlay()
						.getPlayResult().getPlayResultYard()
						&& !params.getPlay().getPlayResult().isPlayResultTurnover()) {
					throw new IllegalArgumentException(
							"receiver.getReceivingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
				}
			} else if (!offPenalty.isEmpty()) {
				if (receiver.getReceivingYard() - offPenalty.get(0).getPenaltyYards() != params.getPlay()
						.getPlayResult().getPlayResultYard()) {
					throw new IllegalArgumentException(
							"receiver.getReceivingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
				}
			} else {
				if (receiver.getReceivingYard() != params.getPlay().getPlayResult().getPlayResultYard()) {
					throw new IllegalArgumentException(
							"receiver.getReceivingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
				}
			}
		}
	}

	private void validatePass(PbpServiceRequestPojo params) {
		PlayerStatPassingPojo passer = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
				.getPassingStat().get(0);
		List<TeamStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
		List<TeamStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();

		PlayerStatReceivingPojo receiver;
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
				.isEmpty()) {
			receiver = null;
		} else {
			receiver = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.get(0);
		}

		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}

		List<PlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		for (PlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

		if (params.getPlay().getPlayStartDown() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartDown() == null");
		}
		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
		}
		if (params.getPlay().getPlayYardToGain() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() == null");
		}
		if (params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() == null");
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PASS) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PASS");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 1");
		}
		if (passer.getPlayerName() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPlayerName() == null");
		}
		if (passer.getPassingSafety() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSafety() == null");
		}
		if (passer.getPassingSpike() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSpike() == null");
		}
		if (passer.getPassingSpike() == 1) {
			if (passer.getPassingCompletion() == 1) {
				throw new IllegalArgumentException("passer.getPassingCompletion() == 1");
			}
		}
		if (passer.getPassingInterception() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterception() == null");
		}
		if (passer.getPassingTwoPointConversion() == null) {
			throw new IllegalArgumentException("passer.getPassingTwoPointConversion() == null");
		}
		if (passer.getPassingTwoPointConversion() == 1) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != 2");
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.PAT) {
				throw new IllegalArgumentException("params.getPlay().getPlayType() != PlayTypeEnum.PAT");
			}
			if (receiver.getReceivingTwoPointConversion() != 1) {
				throw new IllegalArgumentException("receiver.getReceivingTwoPointConversion() != 1");
			}
			if (passer.getPassingYard() < 3) {
				throw new IllegalArgumentException("passer.getPassingYard() < 3");
			}
		}
		if (passer.getPassingInterception() == 1) {
			if (passer.getPassingCompletion() != 0) {
				throw new IllegalArgumentException("passer.getPassingCompletion() != 0");
			}
			if (passer.getPassingYard() != 0) {
				throw new IllegalArgumentException("passer.getPassingYard() != 0");
			}
			if (passer.getPassingBreakup() != 0) {
				throw new IllegalArgumentException("passer.getPassingBreakup() != 0");
			}
			if (passer.getPassingFirstDown() != 0) {
				throw new IllegalArgumentException("passer.getPassingFirstDown() != 0");
			}
			if (passer.getPassingTouchdown() != 0) {
				throw new IllegalArgumentException("passer.getPassingTouchdown() != 0");
			}
			if (passer.getPassingYardThrownTo() == null) {
				throw new IllegalArgumentException("passer.getPassingYardThrownTo() == null");
			}
			if (defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).size() == 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).size() == 0");
			}
		} else {
			if (defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).size() != 0");
			}
			if (defense.stream().filter(d -> d.getInterceptionYard() > 1).collect(Collectors.toList()).size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterceptionYard() > 0).collect(Collectors.toList()).size() != 0");
			}
		}

		if (passer.getPassingInterceptionTouchdown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterceptionTouchdown() == null");
		}
		if (passer.getPassingInterceptionTouchdown() == 1) {
			if (passer.getPassingInterception() == 0) {
				throw new IllegalArgumentException("passer.getPassingInterception() == 0");
			}
			if (passer.getPassingInterceptionYard() == 0) {
				throw new IllegalArgumentException("passer.getPassingInterceptionYard() == 0");
			}
			if (defense.stream().filter(d -> d.getInterceptionTouchdown() == 1).collect(Collectors.toList())
					.size() == 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterceptionTouchdown() == 1).collect(Collectors.toList()).size() == 0");
			}
		} else {
			if (defense.stream().filter(d -> d.getInterceptionTouchdown() == 1).collect(Collectors.toList())
					.size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterceptionTouchdown() == 1).collect(Collectors.toList()).size() != 0");
			}
		}

		if (passer.getPassingInterceptionYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterceptionYard() == null");
		}
		if (passer.getPassingInterceptionYard() > 0) {
			if (defense.stream().filter(d -> d.getInterceptionTouchdown() > 1).collect(Collectors.toList())
					.size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterceptionYard() > 0).collect(Collectors.toList()).size() != 0");
			}
		} else {
			if (defense.stream().filter(d -> d.getInterceptionYard() > 1).collect(Collectors.toList()).size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterceptionYard() > 0).collect(Collectors.toList()).size() != 0");
			}
		}

		if (passer.getPassingBreakup() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingBreakup() == null");
		}
		if (passer.getPassingBreakup() == 1) {
			if (passer.getPassingCompletion() == 1) {
				throw new IllegalArgumentException("passer.getPassingCompletion() == 1");
			}
			if (defense.stream().filter(d -> d.getPassBreakUp() == 1).collect(Collectors.toList()).size() == 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getPassBreakUp() > 0).collect(Collectors.toList()).size() == 0");
			}
			if (defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).size() > 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).size() > 0");
			}
		} else {
			if (defense.stream().filter(d -> d.getPassBreakUp() == 1).collect(Collectors.toList()).size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getPassBreakUp() > 0).collect(Collectors.toList()).size() != 0");
			}
		}

		if (passer.getPassingCompletion() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingCompletion() == null");
		}
		if (passer.getPassingCompletion() == 0) {
			if (Objects.nonNull(receiver) && receiver.getReceivingReception() != 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingReception() != 0");
			}
			if (passer.getPassingYard() != 0) {
				throw new IllegalArgumentException("passer.getPassingYard() != 0");
			}
			if (passer.getPassingInterception() != 1 && passer.getPassingSack() != 1
					&& !defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
			if (passer.getPassingSack() == 0
					&& defense.stream().filter(d -> d.getTackleTotal() > 0 && d.getFumbleForced() == 0)
							.collect(Collectors.toList()).size() > 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).size() > 0");
			}
		} else {
			if (Objects.isNull(receiver) || receiver.getReceivingReception() == 0) {
				throw new IllegalArgumentException("Objects.isNull(receiver) || receiver.getReceivingReception() == 0");
			}

		}

		if (passer.getPassingAttempt() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingAttempt() == null");
		}
		if (passer.getPassingAttempt() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingAttempt() != 1");
		}

		if (passer.getPassingFirstDown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFirstDown() == null");
		}
		if (passer.getPassingFirstDown() == 1) {
			if (!params.getPlay().getPlayResult().isPlayResultFirstDown()) {
				throw new IllegalArgumentException("!params.getPlay().getPlayResult().isPlayResultFirstDown()");
			}
			if (passer.getPassingCompletion() == 0) {
				throw new IllegalArgumentException("passer.getPassingCompletion() == 0");
			}
			if (Objects.isNull(receiver) || receiver.getRecievingFirstDown() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getRecievingFirstDown() == 0");
			}
			if (Objects.isNull(receiver) || receiver.getReceivingReception() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingReception() == 0");
			}
		} else {
			if (params.getPlay().getPlayResult().isPlayResultFirstDown()
					&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown()");
			}
			if (Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0");
			}
		}

		if (passer.getPassingTouchdown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingTouchdown() == null");
		}
		if (passer.getPassingTouchdown() == 1) {
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 6) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != 6");
			}
			if (passer.getPassingCompletion() == 0) {
				throw new IllegalArgumentException("passer.getPassingCompletion() == 0");
			}
			if (Objects.isNull(receiver) || receiver.getReceivingTouchdown() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingTouchdown() == 0");
			}
			if (Objects.isNull(receiver) || receiver.getReceivingReception() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingReception() == 0");
			}
		} else {
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 0
					&& params.getPlay().getPlayResult().getPlayResultPoints() != -6
					&& params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayResult().getPlayResultPoints() != 0 && params.getPlay().getPlayResult().getPlayResultPoints() != -6");
			}
			if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0");
			}
		}

		if (passer.getPassingYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingYard() == null");
		}
		if (passer.getPassingYard() == 0) {
			if (Objects.nonNull(receiver) && receiver.getReceivingYard() != 0) {
				throw new IllegalArgumentException("Objects.nonNull(receiver) && receiver.getReceivingYard() != 0");
			}
		} else {
			if (Objects.isNull(receiver) || receiver.getReceivingYard() == 0) {
				throw new IllegalArgumentException("Objects.isNull(receiver) || receiver.getReceivingYard() == 0");
			}
			if (passer.getPassingCompletion() == 0) {
				throw new IllegalArgumentException("passer.getPassingCompletion() != 0");
			}

		}

		if (passer.getPassingDirection() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingDirection() == null");
		}
		if (passer.getPassingYardThrownTo() == null) {
			if (passer.getPassingYardAfterCatch() != null) {
				throw new IllegalArgumentException("passer.getPassingYardAfterCatch() != null");
			}
		} else {
			if (passer.getPassingInterception() == 0 && passer.getPassingYardAfterCatch() == null) {
				throw new IllegalArgumentException("passer.getPassingYardAfterCatch() == null");
			}
		}
		if (passer.getPassingYardAfterCatch() == null) {
			if (passer.getPassingInterception() == 0 && passer.getPassingYardThrownTo() != null) {
				throw new IllegalArgumentException("passer.getPassingYardThrownTo() != null");
			}
		} else {
			if (passer.getPassingYardThrownTo() == null) {
				throw new IllegalArgumentException("passer.getPassingYardThrownTo() == null");
			}
		}

		if (passer.getPassingDrop() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingDrop() == null");
		}
		if (passer.getPassingDrop() == 0) {
			if (Objects.nonNull(receiver) && receiver.getReceivingDrop() != 0) {
				throw new IllegalArgumentException("Objects.nonNull(receiver) && receiver.getReceivingDrop() != 0");
			}
		} else {
			if (Objects.isNull(receiver) || receiver.getReceivingDrop() == 0) {
				throw new IllegalArgumentException("Objects.isNull(receiver) || receiver.getReceivingDrop() == 0");
			}
			if (passer.getPassingCompletion() != 0) {
				throw new IllegalArgumentException("passer.getPassingCompletion() != 0");
			}

		}

		if (passer.getPassingYardAfterCatch() != null) {

			if (passer.getPassingYardAfterCatch() == 0) {
				if (Objects.nonNull(receiver) && receiver.getReceivingYardAfterCatch() != 0) {
					throw new IllegalArgumentException(
							"Objects.nonNull(receiver) && receiver.getReceivingYardAfterCatch() != 0");
				}
				if (passer.getPassingCompletion() != 0) {
					throw new IllegalArgumentException("passer.getPassingCompletion() != 0");
				}
			} else {
				if (Objects.isNull(receiver) || receiver.getReceivingYardAfterCatch() == 0) {
					throw new IllegalArgumentException(
							"Objects.isNull(receiver) || receiver.getReceivingYardAfterCatch() == 0");
				}
			}
		}

		if (passer.getPassingHurry() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingHurry() == null");
		}
		if (passer.getPassingHurry() == 0) {
			if (defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).size() != 0");
			}
		} else {
			if (defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).size() == 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).size() == 0");
			}
		}

		if (passer.getPassingSack() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSack() == null");
		}
		if (passer.getPassingSack() == 0) {
			if (defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).size() != 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).size() != 0");
			}
		} else {
			if (defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).size() == 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).size() == 0");
			}
			if (defense.stream().filter(d -> d.getTackleForLoss() == 1).collect(Collectors.toList()).size() == 0) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getTackleForLoss() == 1).collect(Collectors.toList()).size() == 0");
			}
		}

		if (passer.getPassingSackYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSackYard() == null");
		}
		if (passer.getPassingSackYard() != 0) {
			if (passer.getPassingSack() == 0) {
				throw new IllegalArgumentException("passer.getPassingSack() == 0");
			}
		}

		if (passer.getPassingFumble() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFumble() == null");
		}
		if (passer.getPassingFumbleLost() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFumbleLost() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == true && passer.getPassingFirstDown() == 0
				&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ "				.getPassingFirstDown() == 0");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == false && passer.getPassingFirstDown() == 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == false && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ "				.getPassingFirstDown() == 1");
		}
		if (passer.getPassingTouchdown() == 0) {
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
//				throw new IllegalArgumentException(
//						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			}
		} else {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
				throw new IllegalArgumentException(
						"!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			}
		}

		if (!defPenalty.isEmpty()) {
			if (passer.getPassingSack() != 1 && defPenalty.get(0).getPenaltyYards() + passer.getPassingYard() != params
					.getPlay().getPlayResult().getPlayResultYard()
					&& !params.getPlay().getPlayResult().isPlayResultTurnover()) {
				throw new IllegalArgumentException(
						"passer.getPassingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
			}
		} else if (!offPenalty.isEmpty()) {
			if (passer.getPassingSack() != 1 && passer.getPassingYard() - offPenalty.get(0).getPenaltyYards() != params
					.getPlay().getPlayResult().getPlayResultYard()) {
				throw new IllegalArgumentException(
						"passer.getPassingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
			}
		} else {
			if (passer.getPassingSack() != 1
					&& passer.getPassingYard() != params.getPlay().getPlayResult().getPlayResultYard()) {
				throw new IllegalArgumentException(
						"passer.getPassingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
			}
		}

	}

	private void validatePat(PbpServiceRequestPojo params) {

		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().size() == 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0");
		}

		PlayerStatKickingPojo kicking = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking().get(0);
		List<PlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		if (kicking.getPlayerName().isEmpty()) {
			throw new IllegalArgumentException("kicking.getPlayerName().isEmpty()");
		}
		if (kicking.getExtraPointAttempt() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() == null");
		}
		if (kicking.getExtraPointAttempt() != 1) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() != 1");
		}
		if (kicking.getFieldGoal() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoal() == null");
		}
		if (kicking.getFieldGoal() == 1) {
			throw new IllegalArgumentException("kicking.getFieldGoal() == 1");
		}
		if (kicking.getTotalPoint() == null) {
			throw new IllegalArgumentException("kicking.getTotalPoint() == null");
		}
		if (kicking.getExtraPointYard() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() == null");
		}
		if (kicking.getExtraPointYard() > 70) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() > 70");
		}
		if (kicking.getExtraPoint() == null) {
			throw new IllegalArgumentException("kicking.getExtraPoint() == null");
		}

		if (kicking.getFieldGoalAttempt() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() == null");
		}
		if (kicking.getFieldGoalAttempt() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() != 0");
		}
		if (kicking.getFieldGoalMiss() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalMiss() == null");
		}
		if (kicking.getFieldGoalMiss() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalMiss() != 0");
		}
		if (kicking.getFieldGoalYard() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() == null");
		}
		if (kicking.getFieldGoalYard() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() != 0");
		}
		if (kicking.getFieldGoalBlock() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalBlock() == null");
		}
		if (kicking.getFieldGoalBlock() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalBlock() != 0");
		}

		if (kicking.getExtraPoint() == 1) {
			if (kicking.getExtraPointMiss() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPointMiss() == 1");
			}
			if (kicking.getExtraPointBlock() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPointBlock() == 1");
			}
			if (kicking.getTotalPoint() != 1) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 1");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 1) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != 1");
			}
		} else {
			if (params.getPlay().getPlayResult().getPlayResultPoints() == 1) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() == 1");
			}
		}

		if (kicking.getExtraPointBlock() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointBlock() == null");
		}
		if (kicking.getExtraPointBlock() == 1) {
			if (kicking.getExtraPointMiss() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPointMiss() == 1");
			}
			if (kicking.getExtraPoint() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPoint() == 1");
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 0");
			}
			if (defense.isEmpty()) {
				throw new IllegalArgumentException("defense.isEmpty()");
			}
		} else {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
		}

		if (kicking.getExtraPointMiss() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointMiss() == null");
		}
		if (kicking.getExtraPointMiss() == 1) {
			if (kicking.getExtraPointBlock() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPointBlock() == 1");
			}
			if (kicking.getExtraPoint() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPoint() == 1");
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 0");
			}
		}

		if (kicking.getKickMissReason() != null) {
			if (kicking.getExtraPointBlock() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPointBlock() == 1");
			}
			if (kicking.getExtraPoint() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPoint() == 1");
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 0");
			}
			if (kicking.getExtraPointMiss() != 1) {
				throw new IllegalArgumentException("kicking.getExtraPointMiss() != 1");
			}
		}

		for (PlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

	}

	private void validateFieldGoal(PbpServiceRequestPojo params) {

		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().size() == 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0");
		}

		PlayerStatKickingPojo kicking = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking().get(0);
		List<PlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		if (kicking.getPlayerName().isEmpty()) {
			throw new IllegalArgumentException("kicking.getPlayerName().isEmpty()");
		}
		if (kicking.getFieldGoalAttempt() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() == null");
		}
		if (kicking.getFieldGoalAttempt() != 1) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() != 1");
		}
		if (kicking.getExtraPoint() == null) {
			throw new IllegalArgumentException("kicking.getExtraPoint() == null");
		}
		if (kicking.getExtraPoint() == 1) {
			throw new IllegalArgumentException("kicking.getExtraPoint() == 1");
		}
		if (kicking.getTotalPoint() == null) {
			throw new IllegalArgumentException("kicking.getTotalPoint() == null");
		}
		if (kicking.getFieldGoalYard() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() == null");
		}
		if (kicking.getFieldGoalYard() > 70) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() > 70");
		}
		if (kicking.getFieldGoal() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoal() == null");
		}

		if (kicking.getExtraPointAttempt() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() == null");
		}
		if (kicking.getExtraPointAttempt() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() != 0");
		}
		if (kicking.getExtraPointMiss() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointMiss() == null");
		}
		if (kicking.getExtraPointMiss() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointMiss() != 0");
		}
		if (kicking.getExtraPointYard() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() == null");
		}
		if (kicking.getExtraPointYard() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() != 0");
		}
		if (kicking.getExtraPointBlock() == null) {
			throw new IllegalArgumentException("kicking.getExtraPointBlock() == null");
		}
		if (kicking.getExtraPointBlock() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointBlock() != 0");
		}

		if (kicking.getFieldGoal() == 1) {
			if (kicking.getFieldGoalMiss() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalMiss() == 1");
			}
			if (kicking.getFieldGoalBlock() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalBlock() == 1");
			}
			if (kicking.getTotalPoint() != 3) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 3");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 3) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != 3");
			}
		} else {
			if (params.getPlay().getPlayResult().getPlayResultPoints() == 3) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() == 3");
			}
		}

		if (kicking.getFieldGoalBlock() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalBlock() == null");
		}
		if (kicking.getFieldGoalBlock() == 1) {
			if (kicking.getFieldGoalMiss() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalMiss() == 1");
			}
			if (kicking.getFieldGoal() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoal() == 1");
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 0");
			}
			if (defense.isEmpty()) {
				throw new IllegalArgumentException("defense.isEmpty()");
			}
		} else {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
		}

		if (kicking.getFieldGoalMiss() == null) {
			throw new IllegalArgumentException("kicking.getFieldGoalMiss() == null");
		}
		if (kicking.getFieldGoalMiss() == 1) {
			if (kicking.getFieldGoalBlock() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalBlock() == 1");
			}
			if (kicking.getFieldGoal() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoal() == 1");
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 0");
			}
		}

		if (kicking.getKickMissReason() != null) {
			if (kicking.getFieldGoalBlock() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalBlock() == 1");
			}
			if (kicking.getFieldGoal() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoal() == 1");
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException("kicking.getTotalPoint() != 0");
			}
			if (kicking.getFieldGoalMiss() != 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalMiss() != 1");
			}
		}

		for (PlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

	}

	private void validateRush(PbpServiceRequestPojo params) {

		List<TeamStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();
		List<TeamStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();

		// TODO add conditional tests
		// LOG.log(Level.INFO, "-- Rushing validation");

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}

		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()\n"
							+ "					.getRushingStat().size() != 1");
		}

		List<PlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		for (PlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

		PlayerStatRushingPojo run = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
				.getRushingStat().get(0);
		if (run.getPlayerName() == null) {
			throw new IllegalArgumentException("run.getPlayerName() == null");
		}
		if (run.getRushingSafety() == null) {
			throw new IllegalArgumentException("run.getRushingSafety() == null");
		}
		if (run.getRushingKneel() == null) {
			throw new IllegalArgumentException("run.getRushingKneel() == null");
		}
		if (run.getRushingKneel() == 1) {
			if (run.getRushingYard() > 0) {
				throw new IllegalArgumentException("run.getRushingYard() > 0");
			}
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
		}
		if (run.getRushingAttempt() == null) {
			throw new IllegalArgumentException("run.getRushingAttempt() == null");
		}
		if (run.getRushingTwoPointConversion() == null) {
			throw new IllegalArgumentException("run.getRushingTwoPointConversion() == null");
		}
		if (run.getRushingTwoPointConversion() == 1) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != 2");
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.PAT) {
				throw new IllegalArgumentException("params.getPlay().getPlayType() != PlayTypeEnum.PAT");
			}
			if (run.getRushingYard() < 3) {
				throw new IllegalArgumentException("run.getRushingYard() < 3");
			}
		}
		if (run.getRushingAttempt() != 1) {
			throw new IllegalArgumentException("run.getRushingAttempt() != 1");
		}
		if (run.getRushingYard() == null) {
			throw new IllegalArgumentException("run.getRushingYard() == null");
		}
		if (!defPenalty.isEmpty()) {
			if (defPenalty.get(0).getPenaltyYards() + run.getRushingYard() != params.getPlay().getPlayResult()
					.getPlayResultYard()) {
				throw new IllegalArgumentException(
						"run.getRushingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
			}
		} else if (!offPenalty.isEmpty()) {
			if (run.getRushingYard() - offPenalty.get(0).getPenaltyYards() != params.getPlay().getPlayResult()
					.getPlayResultYard()) {
				throw new IllegalArgumentException(
						"run.getRushingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
			}
		} else {
			if (run.getRushingFumble() == 0
					&& run.getRushingYard() != params.getPlay().getPlayResult().getPlayResultYard()) {
				throw new IllegalArgumentException(
						"run.getRushingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
			}
		}

		if (run.getRushingFumble() == null) {
			throw new IllegalArgumentException("run.getRushingFumble() == null");
		}
		if (run.getRushingFumbleLost() == null) {
			throw new IllegalArgumentException("run.getRushingFumbleLost() == null");
		}
		if (run.getRushingFirstDown() == null) {
			throw new IllegalArgumentException("run.getRushingFirstDown() == null");
		}
		if (run.getRushingTouchdown() == null) {
			throw new IllegalArgumentException("run.getRushingTouchdown() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == true && run.getRushingFirstDown() == 0
				&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && run.getRushingFirstDown() == 0");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == false && run.getRushingFirstDown() == 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == false && run.getRushingFirstDown() == 1");
		}

	}

	private void validatePunt(PbpServiceRequestPojo params) {
		// LOG.log(Level.INFO, "-- Punt validation");

		PlayerStatPuntingPojo punter = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
				.getPunting().get(0);
//		List<PlayerStatPuntReturnPojo> returner = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
//				.getSpecialTeam().getPuntReturn();
		PlayerStatPuntReturnPojo puntReturner;
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().isEmpty()) {
			puntReturner = null;
		} else {
			puntReturner = params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
					.findPuntReturner();
		}

		List<PlayerStatPuntReturnPojo> allReturnStats = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getSpecialTeam().getPuntReturn();
		List<PlayerStatDefenseProductionPojo> coverage = params.getPlay().getPlayerStat()
				.get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage();

		for (PlayerStatDefenseProductionPojo cov : coverage) {
			validateDef(params, cov);
		}
		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("Drive.isKickoff()");
		}

		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 0");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()\n"
							+ "					.getRushingStat().size() != 0");
		}

		if (params.getPlay().getPlayStartDown() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartDown() == null");
		}
		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
		}
		if (params.getPlay().getPlayYardToGain() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() == null");
		}
		if (params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() == null");
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() != null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() != Boolean.FALSE) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() != Boolean.FALSE");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 1");
		}

		/**
		 * NULL CHECK
		 */
		if (StringUtils.isBlank(punter.getPlayerName())) {
			throw new IllegalArgumentException("StringUtils.isBlank(punter.getPlayerName())");
		}
		if (punter.getPuntYard() == null) {
			throw new IllegalArgumentException("punter.getPuntYard() == null");
		}
		if (punter.getPuntTouchback() == null) {
			throw new IllegalArgumentException("punter.getPuntTouchback() == null");
		}
		if (punter.getPuntReturnYard() == null) {
			throw new IllegalArgumentException("punter.getPuntReturnYard() == null");
		}
		if (punter.getPuntReturnTouchdown() == null) {
			throw new IllegalArgumentException("punter.getPuntReturnTouchdown() == null");
		}
		if (punter.getPuntFairCatch() == null) {
			throw new IllegalArgumentException("punter.getPuntFairCatch() == null");
		}
		if (punter.getPunt() == null) {
			throw new IllegalArgumentException("punter.getPunt() == null");
		}
		if (punter.getPuntLandYard() == null) {
			throw new IllegalArgumentException("punter.getPuntLandYard() == null");
		}
		if (punter.getPuntFairCatch() == 1 && allReturnStats.isEmpty()) {
			throw new IllegalArgumentException("punter.getPuntFairCatch() == 1 && allReturnStats.isEmpty()");
		}
		if ((punter.getPuntReturnYard() != 0 && punter.getPuntTouchback() != 1) && allReturnStats.isEmpty()) {
			throw new IllegalArgumentException(
					"(punter.getPuntReturnYard() != 0 && punter.getPuntTouchback() != 1) && returner.isEmpty()");
		}
		if ((punter.getPuntTouchback() == 1 || punter.getPuntFairCatch() == 1) && !coverage.isEmpty()) {
			throw new IllegalArgumentException(
					"(punter.getPuntTouchback() == 1 || punter.getPuntFairCatch() == 1) && !coverage.isEmpty()");
		}

		/**
		 * VALUE CHECK
		 */
		if (punter.getPuntTouchback() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntTouchback() > 1");
		}

		if (punter.getPuntReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntReturnTouchdown()  > 1");
		}
		if (punter.getPuntFairCatch() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntFairCatch()  > 1");
		}
		if (punter.getPunt() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPunt() > 1");
		}

		if (allReturnStats.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1) {
			throw new IllegalArgumentException(
					"returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1");

		}
		for (PlayerStatPuntReturnPojo allReturn : allReturnStats) {
			if (allReturn.getPlayerName() == null) {
				throw new IllegalArgumentException("allReturn.getPlayerName() == null");
			}
			if (allReturn.getPuntReturn() == null) {
				throw new IllegalArgumentException("allReturn.getpuntReturn() == null");
			}
			if (allReturn.getPuntReturnYard() == null) {
				throw new IllegalArgumentException("allReturn.getpuntReturnYard() == null");
			}
			if (allReturn.getPuntReturnTouchdown() == null) {
				throw new IllegalArgumentException("allReturn.getpuntReturnTouchdown() == null");
			}
			if (allReturn.getPuntReturnFairCatch() == null) {
				throw new IllegalArgumentException("allReturn.getpuntReturnFairCatch() == null");
			}
			if (allReturn.getPuntReturnStartYard() == null) {
				throw new IllegalArgumentException("allReturn.getpuntReturnStartYard() == null");
			}
			if (allReturn.getPuntReturnFumble() == null) {
				throw new IllegalArgumentException("allReturn.getpuntReturnFumble() == null");
			}
			if (allReturn.getPuntReturnFumbleLost() == null) {
				throw new IllegalArgumentException("allReturn.getpuntReturnFumbleLost() == null");
			}
			if (allReturn.getPuntReturnBlock() == null) {
				throw new IllegalArgumentException("allReturn.getPuntReturnBlock() == null");
			}

			if (allReturn.getPuntReturn() > 1) {
				throw new IllegalArgumentException("allReturn.getPuntReturn() > 1");
			}
			if (allReturn.getPuntReturnTouchdown() > 1) {
				throw new IllegalArgumentException("allReturn.getPuntReturnTouchdown() > 1");
			}
			if (allReturn.getPuntReturnFairCatch() > 1) {
				throw new IllegalArgumentException("allReturn.getPuntReturnFairCatch() > 1");
			}
			if (allReturn.getPuntReturnFumble() > 1) {
				throw new IllegalArgumentException("allReturn.getPuntReturnFumble() > 1");
			}
			if (allReturn.getPuntReturnFumbleLost() > 1) {
				throw new IllegalArgumentException("allReturn.getPuntReturnFumbleLost() > 1");
			}

			if (allReturn.getPuntReturn() == 0) {
				if (allReturn.getPuntReturnTouchdown() != 0) {
					throw new IllegalArgumentException("allReturn.getPuntReturnTouchdown() != 0");
				}
				if (allReturn.getPuntReturnFairCatch() != 0) {
					throw new IllegalArgumentException("allReturn.getPuntReturnFairCatch() != 0");
				}
				if (allReturn.getPuntReturnStartYard() != 0) {
					throw new IllegalArgumentException("allReturn.getPuntReturnStartYard() != 0");
				}
				if (allReturn.getPuntReturnFumble() != 0) {
					throw new IllegalArgumentException("allReturn.getPuntReturnFumble() != 0");
				}
				if (allReturn.getPuntReturnFumbleLost() != 0) {
					throw new IllegalArgumentException("allReturn.getPuntReturnFumbleLost() != 0");
				}
				if (allReturn.getPuntReturnBlock() != 1) {
					throw new IllegalArgumentException("allReturn.getPuntReturnBlock() != 1");
				}
			}
		}

		/**
		 * CONDITIONAL CHECK
		 */
		if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnTouchdown() == 1) {
			/**
			 * RETURN TOUCHDOWN
			 */
			if (punter.getPuntReturnTouchdown() != 1) {
				throw new IllegalArgumentException("punter.getPuntReturnTouchdown() != 1");
			}
			if (punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0) {
				throw new IllegalArgumentException(
						"punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0");
			}
			if (punter.getPuntBlocked() == 0
					&& puntReturner.getPuntReturnStartYard() + puntReturner.getPuntReturnYard() != 100) {
				throw new IllegalArgumentException("punter.getPuntBlocked() == 0\n"
						+ "					&& puntReturner.getPuntReturnStartYard() + puntReturner.getPuntReturnYard() != 100");
			}

		}
		if (punter.getPuntReturnTouchdown() == 1) {
			/**
			 * RETURN TOUCHDOWN
			 */
			if (Objects.isNull(puntReturner) || puntReturner.getPuntReturnTouchdown() != 1) {
				throw new IllegalArgumentException(
						"returner.isEmpty() || returner.get(0).getPuntReturnTouchdown() != 1");
			}
			if (punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0) {
				throw new IllegalArgumentException(
						"punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0");
			}
			if (punter.getPuntBlocked() == 0
					&& puntReturner.getPuntReturnStartYard() + puntReturner.getPuntReturnYard() != 100) {
				throw new IllegalArgumentException(
						"puntReturner.getPuntReturnStartYard() + puntReturner.getPuntReturnYard() != 100");
			}
		}
		if (punter.getPuntTouchback() == 1) {
			/**
			 * TOUCHBACK
			 */
			if (punter.getPuntReturnYard() != 25) {
				throw new IllegalArgumentException("punter.getPuntReturnYard() != 25");
			}
			if (punter.getPuntBlocked() != 0) {
				throw new IllegalArgumentException("punter.getPuntBlocked() != 0");
			}
			if (punter.getPuntReturnTouchdown() != 0) {
				throw new IllegalArgumentException("punter.getPuntReturnTouchdown() != 0");
			}
			if (punter.getPuntFairCatch() != 0) {
				throw new IllegalArgumentException("punter.getPuntFairCatch() != 0");
			}
			if (coverage.size() != 0) {
				throw new IllegalArgumentException("coverage.size() != 0");
			}
			if (allReturnStats.size() != 0) {
				throw new IllegalArgumentException("returner.size() != 0");
			}
		}

		if (punter.getPuntFairCatch() == 1) {
			/**
			 * FAIR CATCH
			 */
			if (punter.getPuntReturnYard() != 0) {
				throw new IllegalArgumentException("punter.getPuntReturnYard() != 0");
			}
			if (punter.getPuntBlocked() != 0) {
				throw new IllegalArgumentException("punter.getPuntBlocked() != 0");
			}
			if (punter.getPuntTouchback() != 0) {
				throw new IllegalArgumentException("punter.getPuntTouchback() != 0");
			}
			if (punter.getPuntReturnTouchdown() != 0) {
				throw new IllegalArgumentException("punter.getPuntReturnTouchdown() != 0");
			}
			if (coverage.size() != 0) {
				throw new IllegalArgumentException("coverage.size() != 0");
			}
			if (allReturnStats.size() != 1) {
				throw new IllegalArgumentException("returner.size() != 1");
			}
			if (puntReturner.getPuntReturnFairCatch() != 1) {
				throw new IllegalArgumentException("returner.get(0).getPuntReturnFairCatch() != 1");
			}
			if (puntReturner.getPuntReturnStartYard() != 100
					- (params.getPlay().getPlayStartYard() + punter.getPuntYard())) {

				System.out.println(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.getPuntReturn().get(0).getPuntReturnStartYard());
				System.out.println(params.getPlay().getPlayStartYard());
				System.out.println(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.getPunting().get(0).getPuntYard());
				System.out.println(params.getPlay().getDriveText());
				System.out.println(params.getTeamAbbrevDict());

				throw new IllegalArgumentException("returner.get(0).getPuntReturnStartYard() != 100\n"
						+ "					- (params.getPlay().getPlayStartYard() + punter.getPuntYard())");
			}
			if (puntReturner.getPuntReturnFumble() != 0) {
				throw new IllegalArgumentException("returner.get(0).getPuntReturnFumble() != 0");
			}
			if (puntReturner.getPuntReturnFumbleLost() != 0) {
				throw new IllegalArgumentException("returner.get(0).getPuntReturnFumbleLost() != 0");
			}
			if (puntReturner.getPuntReturnBlock() != 0) {
				throw new IllegalArgumentException("returner.get(0).getPuntReturnBlock() != 0");
			}
		}

		if (punter.getPuntBlocked() == 1) {
			/**
			 * BLOCKED
			 */
			if (punter.getPuntTouchback() != 0) {
				throw new IllegalArgumentException("punter.getPuntTouchback() != 0");
			}
			if (punter.getPuntFairCatch() != 0) {
				throw new IllegalArgumentException("punter.getPuntFairCatch() != 0");
			}
			if (coverage.size() != 0) {
				throw new IllegalArgumentException("coverage.size() != 0");
			}
			if (allReturnStats.size() == 0) {
				throw new IllegalArgumentException("returner.size() == 0");
			}
			if (allReturnStats.stream().filter(ret -> ret.getPuntReturnBlock() == 1).collect(Collectors.toList())
					.size() == 0) {
				throw new IllegalArgumentException(
						"returner.stream().filter(ret -> ret.getPuntReturnBlock() == 1).collect(Collectors.toList())\n"
								+ "					.size() == 0");
			}
		}

	}

	private void validateDef(PbpServiceRequestPojo params, PlayerStatDefenseProductionPojo def) {

		List<PlayerStatPassingPojo> pass = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
				.getPassingStat();
		List<PlayerStatRushingPojo> rush = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
				.getRushingStat();
		List<PlayerStatReceivingPojo> receive = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getOffense().getReceivingStat();
		List<PlayerStatPuntReturnPojo> puntRet = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getSpecialTeam().getPuntReturn();
		List<PlayerStatKickReturnPojo> kickRet = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getSpecialTeam().getKickReturn();
		List<PlayerStatKickingPojo> kick = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking();

		if (def.getPlayerName().isBlank()) {
			throw new IllegalArgumentException("def.getPlayerName().isBlank()");
		}
		if (def.getTackleTotal() == null) {
			throw new IllegalArgumentException("def.getTackleTotal() == null");
		}
		if (def.getTackleSolo() == null) {
			throw new IllegalArgumentException("def.getTackleSolo() == null");
		}
		if (def.getTackleAssist() == null) {
			throw new IllegalArgumentException("def.getTackleAssist() == null");
		}
		if (def.getSack() == null) {
			throw new IllegalArgumentException("def.getSack() == null");
		}
		if (def.getPassBreakUp() == null) {
			throw new IllegalArgumentException("def.getPassBreakUp() == null");
		}
		if (def.getTackleForLoss() == null) {
			throw new IllegalArgumentException("def.getTackleForLoss() == null");
		}
		if (def.getInterception() == null) {
			throw new IllegalArgumentException("def.getInterception() == null");
		}
		if (def.getFumbleForced() == null) {
			throw new IllegalArgumentException("def.getFumbleForced() == null");
		}
		if (def.getFumbleRecovered() == null) {
			throw new IllegalArgumentException("def.getFumbleRecovered() == null");
		}
		if (def.getFumbleYard() == null) {
			throw new IllegalArgumentException("def.getFumbleYard() == null");
		}
		if (def.getFumbleTouchdown() == null) {
			throw new IllegalArgumentException("def.getFumbleTouchdown() == null");
		}
		if (def.getSafety() == null) {
			throw new IllegalArgumentException("def.getSafety() == null");
		}
		if (def.getInterceptionTouchdown() == null) {
			throw new IllegalArgumentException("def.getInterceptionTouchdown() == null");
		}
		if (def.getInterceptionYard() == null) {
			throw new IllegalArgumentException("def.getInterceptionYard() == null");
		}
		if (def.getQuarterbackHurry() == null) {
			throw new IllegalArgumentException("def.getQuarterbackHurry() == null");
		}
		if (def.getKickBlock() == null) {
			throw new IllegalArgumentException("def.getKickBlock() == null");
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT) {
			if (def.getTackleYard() == null) {
				throw new IllegalArgumentException("def.getTackleYard() == null");
			}
		}

		if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
				|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
			if (def.getKickBlock() == 1 && kick.get(0).getFieldGoalBlock() == 0
					&& kick.get(0).getExtraPointBlock() == 0) {
				throw new IllegalArgumentException("def.getKickBlock() == 1 && kick.get(0).getFieldGoalBlock() == 0");
			}

		} else {
			if (def.getKickBlock() == 1) {
				throw new IllegalArgumentException("def.getKickBlock() == 1");
			}
		}

		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PASS) {
			if (def.getSack() == 1) {
				throw new IllegalArgumentException("def.getSack() == 1");
			}
			if (def.getPassBreakUp() == 1) {
				throw new IllegalArgumentException("def.getPassBreakUp() == 1");
			}
			if (def.getInterception() == 1) {
				throw new IllegalArgumentException("def.getInterception() == 1");
			}
			if (def.getInterceptionTouchdown() == 1) {
				throw new IllegalArgumentException("def.getInterceptionTouchdown() == 1");
			}
			if (def.getInterceptionYard() == 1) {
				throw new IllegalArgumentException("def.getInterceptionYard() == 1");
			}
			if (def.getQuarterbackHurry() == 1) {
				throw new IllegalArgumentException("def.getQuarterbackHurry() == 1");
			}
		}

		if (def.getInterceptionTouchdown() == 1) {
			if (def.getInterception() == 0) {
				throw new IllegalArgumentException("def.getInterception() == 0");
			}
			if (def.getInterceptionYard() == 0) {
				throw new IllegalArgumentException("def.getInterceptionYard() == 0");
			}
		}

		if (def.getFumbleTouchdown() == 1) {
			if (def.getFumbleRecovered() == 0) {
				throw new IllegalArgumentException("def.getFumbleRecovered() == 0");
			}
			if (!params.getDrive().isKickoff() && def.getFumbleYard() == 0) {
				throw new IllegalArgumentException("def.getFumbleYard() == 0");
			}
		}

		if (def.getFumbleRecovered() == 1) {
			if (params.getDrive().isKickoff()) {
				if (kickRet.stream().filter(kr -> kr.getKickReturnFumbleLost() == 1).collect(Collectors.toList())
						.size() == 0) {
					throw new IllegalArgumentException(
							"kickRet.stream().filter(kr -> kr.getKickReturnFumbleLost() == 1).collect(Collectors.toList())\n"
									+ "								.size() == 0");
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				if (puntRet.stream().filter(pr -> pr.getPuntReturnFumbleLost() == 1).collect(Collectors.toList())
						.size() == 0) {
					throw new IllegalArgumentException(
							"puntRet.stream().filter(pr -> pr.getPuntReturnFumbleLost() == 1).collect(Collectors.toList())\n"
									+ "						.size() == 0");
				}
			} else {
				if ((pass.stream().filter(p -> p.getPassingFumbleLost() == 1).collect(Collectors.toList()).size() == 0)
						&& (rush.stream().filter(r -> r.getRushingFumbleLost() == 1).collect(Collectors.toList())
								.size() == 0)
						&& (receive.stream().filter(rc -> rc.getReceivingFumbleLost() == 1).collect(Collectors.toList())
								.size() == 0)
						&& (kick.stream().filter(k -> k.getFieldGoalBlock() == 1).collect(Collectors.toList())
								.size() == 0)) {
					throw new IllegalArgumentException(
							"(pass.stream().filter(p -> p.getPassingFumbleLost() == 1).collect(Collectors.toList()).size() == 0)\n"
									+ "						&& (rush.stream().filter(r -> r.getRushingFumbleLost() == 1).collect(Collectors.toList())\n"
									+ "								.size() == 0)\n"
									+ "						&& (receive.stream().filter(rc -> rc.getReceivingFumbleLost() == 1).collect(Collectors.toList())\n"
									+ "								.size() == 0)");
				}
			}

		}

		if (def.getSack() == 1) {
			if (def.getTackleForLoss() == 0) {
				throw new IllegalArgumentException("def.getTackleForLoss() == 0");
			}
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException("def.getTackleTotal() == 0");
			}
		}

		if (def.getSafety() == 1) {
			if (def.getTackleForLoss() == 0) {
				throw new IllegalArgumentException("def.getTackleForLoss() == 0");
			}
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException("def.getTackleTotal() == 0");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != -2) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != -2");
			}
		}

		if (def.getTackleForLoss() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException("def.getTackleTotal() == 0");
			}
			if (def.getTackleYard() > 0) {
				throw new IllegalArgumentException("def.getTackleYard() > 0");
			}
		}

		if (def.getTackleYard() < 0) {
			if (def.getTackleForLoss() == 0) {
				throw new IllegalArgumentException("def.getTackleForLoss() == 0");
			}
		}

		if (def.getTackleAssist() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException("def.getTackleTotal() == 0");
			}
			if (def.getFumbleForced() != 1 && def.getTackleSolo() != 0) {
				throw new IllegalArgumentException("def.getTackleSolo() != 0");
			}
		}

		if (def.getTackleSolo() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException("def.getTackleTotal() == 0");
			}
			if (def.getFumbleForced() != 1 && def.getTackleAssist() != 0) {
				throw new IllegalArgumentException("def.getTackleAssist() != 0");
			}
		}

		if (def.getSack() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException("def.getTackleTotal() == 0");
			}
			if (def.getTackleForLoss() == 0) {
				throw new IllegalArgumentException("def.getTackleForLoss() == 0");
			}
			if (pass.get(0).getPassingSack() == 0) {
				throw new IllegalArgumentException("pass.getPassingSack() == 0");
			}
		}

		if (def.getFumbleForced() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException("def.getTackleTotal() == 0");
			}
			if (def.getTackleSolo() == 0.0) {
				throw new IllegalArgumentException("def.getTackleSolo() == 0");
			}
			if ((pass.stream().filter(p -> p.getPassingFumble() == 1).collect(Collectors.toList()).size() == 0)
					&& (rush.stream().filter(r -> r.getRushingFumble() == 1).collect(Collectors.toList()).size() == 0)
					&& (receive.stream().filter(rc -> rc.getReceivingFumble() == 1).collect(Collectors.toList())
							.size() == 0)
					&& (kickRet.stream().filter(kr -> kr.getKickReturnFumble() == 1).collect(Collectors.toList())
							.size() == 0)
					&& (puntRet.stream().filter(pr -> pr.getPuntReturnFumble() == 1).collect(Collectors.toList())
							.size() == 0)) {
				throw new IllegalArgumentException(
						"(pass.stream().filter(p -> p.getPassingFumble() == 1).collect(Collectors.toList()).size() == 0)\n"
								+ "					&& (rush.stream().filter(r -> r.getRushingFumble() == 1).collect(Collectors.toList()).size() == 0)\n"
								+ "					&& (receive.stream().filter(rc -> rc.getReceivingFumble() == 1).collect(Collectors.toList())\n"
								+ "							.size() == 0)\n"
								+ "					&& (kickRet.stream().filter(kr -> kr.getKickReturnFumble() == 1).collect(Collectors.toList())\n"
								+ "							.size() == 0)\n"
								+ "					&& (puntRet.stream().filter(pr -> pr.getPuntReturnFumble() == 1).collect(Collectors.toList())\n"
								+ "							.size() == 0)");
			}
		}
	}

	private void validateOffense(PbpServiceRequestPojo params) {

		List<TeamStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();
		List<TeamStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();

		/**
		 * Pull test cases
		 */

		if (params.getPlayRawText().equals(
				"Brin,Davis pass complete to Stokes,Keylon for 11 yards to the UCD01 fumbled by Stokes,Keylon at UCD08 forced by Venable,Chris, Touchback")) {
			return;
		}
		// TODO add safety checks
		PlayerStatRushingPojo rush;
		PlayerStatPassingPojo pass;
		PlayerStatReceivingPojo receiver;
		List<PlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();
		Integer playPoints = params.getPlay().getPlayResult().getPlayResultPoints();
		boolean firstDown = params.getPlay().getPlayResult().isPlayResultFirstDown();
		boolean turnover = params.getPlay().getPlayResult().isPlayResultTurnover();
		Integer playResultYardLine = params.getPlay().getPlayResult().getPlayResultYardLine();
		Integer playResultYard = params.getPlay().getPlayResult().getPlayResultYard();
		Integer playStartYard = params.getPlay().getPlayStartYard();
		Integer yardToGain = params.getPlay().getPlayYardToGain();
		Integer turnoverReturnYard;
		Integer offensePenalty;
		Integer defensePenalty;
		Integer passYardThrown;
		if (offPenalty.isEmpty()) {
			offensePenalty = 0;
		} else {
			offensePenalty = offPenalty.get(0).getPenaltyYards();
		}
		if (defPenalty.isEmpty()) {
			defensePenalty = 0;
		} else {
			defensePenalty = defPenalty.get(0).getPenaltyYards();
		}
		if (turnover && params.getPlay().getPlayType() == PlayTypeEnum.OFFENSE) {
			PlayerStatDefenseProductionPojo turnoverDef = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
					.getDefense().findDefenseWithTurnover();
			if (Objects.isNull(turnoverDef)) {
				turnoverReturnYard = 0;
			} else {
				turnoverReturnYard = turnoverDef.getInterceptionYard() + turnoverDef.getFumbleYard();
			}
		} else {
			turnoverReturnYard = 0;
		}

		if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
					.getPassingInterception() == 1) {
				passYardThrown = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
						.getPassingStat().get(0).getPassingYardThrownTo();
			} else {
				passYardThrown = 0;
			}
		} else {
			passYardThrown = 0;
		}

		/**
		 * Null Tests
		 */
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultTurnover() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultTurnover() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown()
				&& params.getPlay().getPlayResult().isPlayResultTurnover()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayResult().isPlayResultTurnover()");
		}
		if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
			if (defense.stream().filter(d -> d.getTackleTotal() > 0 && d.getFumbleForced() == 0)
					.collect(Collectors.toList()).size() > 0) {
				if (!(params.getPlay().getPlayStartDown() == PlayDownEnum.FOURTH && yardToGain > playResultYard)) {
					throw new IllegalArgumentException(
							"params.getPlay().getPlayResult().isPlayResultTurnover() && defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).size() > 0");
				}
			}
		} else {
			if (params.getPlay().getPlayType() == PlayTypeEnum.OFFENSE && params.getPlay().getPlayCallType() != PlayCallTypeEnum.FG
					&& params.getPlay().getPlayStartDown() == PlayDownEnum.FOURTH && yardToGain > playResultYard) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayStartDown() == PlayDownEnum.FOURTH && yardToGain > playResultYard");
			}
		}

		if (params.getPlay().getPlayResult().getPlayResultPoints() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() == null");
		}
		if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF && params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() == null");
		}

		if (!params.getDrive().isKickoff() && params.getPlay().getPlayType() != PlayTypeEnum.PUNT) {
			if (playStartYard + playResultYard + passYardThrown - turnoverReturnYard != playResultYardLine && !(params
					.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
							.isEmpty())) {
				if (!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN
						&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
								.getRushingStat().get(0).getRushingFumble() == 1
						&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
								.getRushingStat().get(0).getRushingFumbleLost() == 0)
						&& !params.getPlayRawText().toUpperCase().contains("TOUCHBACK")) {
					System.out.println(String.format("Play Start Yardline: %s", playStartYard));
					System.out.println(String.format("Play Result Yard: %s", playResultYard));
					System.out.println(String.format("Pass Yard Thrown: %s", passYardThrown));
					System.out.println(String.format("Offense Penalty Yard: %s", offensePenalty));
					System.out.println(String.format("Defense Penalty Yard: %s", defensePenalty));
					System.out.println(String.format("Turnover Return Yard: %s", turnoverReturnYard));
					System.out.println(String.format("Play Result Yardline: %s", playResultYardLine));
					throw new IllegalArgumentException(
							"playStartYard + playResultYard + passYardThrown != playResultYardLine");
				}
			}

			if (yardToGain < playResultYard && !firstDown && !turnover && playPoints != 6 && playPoints != 2) {
				System.out.println(String.format("Play Yard to Gain: %s", yardToGain));
				System.out.println(String.format("Play Result Yard: %s", playResultYard));
				System.out.println(String.format("Play First Down: %s", firstDown));
				System.out.println(String.format("Play Turnover: %s", turnover));
				System.out.println(String.format("Play Points: %s", playPoints));

				throw new IllegalArgumentException(
						"yardToGain <= playResultYard && !firstDown && !turnover && playPoints != 6");
			}
			if (offPenalty.isEmpty() && yardToGain > playResultYard && firstDown && defPenalty.stream()
					.filter(dp -> dp.getPenaltyFirstDown() == 1).collect(Collectors.toList()).isEmpty()) {
				System.out.println(String.format("Play Yard to Gain: %s", yardToGain));
				System.out.println(String.format("Play Result Yard: %s", playResultYard));
				System.out.println(String.format("Play First Down: %s", firstDown));
				System.out.println(String.format("Play Turnover: %s", turnover));
				throw new IllegalArgumentException(
						"params.getPlay().getPlayYardToGain() > params.getPlay().getPlayResult().getPlayResultYard() && params.getPlay().getPlayResult().isPlayResultFirstDown()");
			}
		}
		/**
		 * Play result either -6, 0, -2, 3, or 6
		 */
		if (params.getPlay().getPlayResult().getPlayResultPoints() != 6
				&& params.getPlay().getPlayResult().getPlayResultPoints() != -6
				&& params.getPlay().getPlayResult().getPlayResultPoints() != 0
				&& params.getPlay().getPlayResult().getPlayResultPoints() != 1
				&& params.getPlay().getPlayResult().getPlayResultPoints() != 3
				&& params.getPlay().getPlayResult().getPlayResultPoints() != 2
				&& params.getPlay().getPlayResult().getPlayResultPoints() != -2) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().getPlayResultPoints() != 6 && params.getPlay().getPlayResult().getPlayResultPoints() != -6 && params.getPlay().getPlayResult().getPlayResultPoints() != 0");
		}

		/**
		 * RUN Tests
		 */
		if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
			rush = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
					.get(0);

			if (rush.getRushingFirstDown() == null) {
				throw new IllegalArgumentException("rush.getRushingFirstDown() == null");
			}
			if (rush.getRushingSafety() == null) {
				throw new IllegalArgumentException("rush.getRushingSafety() == null");
			}
			if (rush.getRushingTouchdown() == null) {
				throw new IllegalArgumentException("rush.getRushingTouchdown() == null");
			}
			if (rush.getRushingFumble() == null) {
				throw new IllegalArgumentException("rush.getRushingFumble() == null");
			}
			if (rush.getRushingFumbleLost() == null) {
				throw new IllegalArgumentException("rush.getRushingFumbleLost() == null");
			}
			if (rush.getRushingFumbleLost() == 1 && rush.getRushingFumble() == 0) {
				throw new IllegalArgumentException("rush.getRushingFumbleLost() == 1 && rush.getRushingFumble() == 0");
			}

			if (firstDown) {
				if (rush.getRushingFirstDown() != 1
						&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
					throw new IllegalArgumentException("rush.getRushingFirstDown() != 1");
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException("rush.getRushingSafety() != 0");
				}
			} else {
				if (rush.getRushingFirstDown() != 0) {
					throw new IllegalArgumentException("rush.getRushingFirstDown() != 0");
				}
			}

			if (playPoints == 6) {
				if (rush.getRushingTouchdown() != 1) {
					throw new IllegalArgumentException("rush.getRushingTouchdown() != 1");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException("rush.getRushingSafety() != 0");
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("rush.getRushingTwoPointConversion() != 0");
				}
			} else if (playPoints == 0) {
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException("rush.getRushingTouchdown() != 0");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException("rush.getRushingSafety() != 0");
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("rush.getRushingTwoPointConversion() != 0");
				}
			} else if (playPoints == -2) {
				if (rush.getRushingSafety() != 1) {
					throw new IllegalArgumentException("rush.getRushingSafety() != 1");
				}
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException("rush.getRushingTouchdown() != 0");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("rush.getRushingTwoPointConversion() != 0");
				}
			} else if (playPoints == 2) {
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException("rush.getRushingTouchdown() != 0");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException("rush.getRushingSafety() != 0");
				}
				if (rush.getRushingTwoPointConversion() == 0) {
					throw new IllegalArgumentException("rush.getRushingTwoPointConversion() == 0");
				}
			} else {
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException("rush.getRushingTouchdown() != 0");
				}
				if (defense.stream().filter(d -> d.getFumbleTouchdown() == 1).collect(Collectors.toList())
						.size() != 1) {
					throw new IllegalArgumentException(
							"defense.stream().filter(d -> d.getFumbleTouchdown() == 1).collect(Collectors.toList()).size() != 1");
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException("rush.getRushingSafety() != 0");
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("rush.getRushingTwoPointConversion() != 0");
				}
			}

			/**
			 * PASS Tests
			 */
		} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
			pass = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
					.get(0);
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.isEmpty()) {
				receiver = null;
			} else {
				receiver = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
						.getReceivingStat().get(0);
			}

			if (pass.getPassingFirstDown() == null) {
				throw new IllegalArgumentException("pass.getPassingFirstDown() == null");
			}
			if (pass.getPassingSafety() == null) {
				throw new IllegalArgumentException("pass.getPassingSafety() == null");
			}
			if (pass.getPassingTouchdown() == null) {
				throw new IllegalArgumentException("pass.getPassingTouchdown() == null");
			}
			if (pass.getPassingFumble() == null) {
				throw new IllegalArgumentException("pass.getPassingFumble() == null");
			}
			if (pass.getPassingFumbleLost() == null) {
				throw new IllegalArgumentException("pass.getPassingFumbleLost() == null");
			}
			if (pass.getPassingFumbleLost() == 1 && pass.getPassingFumble() == 0) {
				throw new IllegalArgumentException("pass.getPassingFumbleLost() == 1 && pass.getPassingFumble() == 0");
			}
			if (pass.getPassingFumble() == 1 && pass.getPassingSack() == 1
					&& defense.stream().filter(d -> d.getFumbleForced() != 0).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"pass.getPassingFumble() == 1 && pass.getPassingSack() == 1 && defense.stream().filter(d -> d.getFumbleForced() != 0).collect(Collectors.toList()).isEmpty()");
			}
			if (Objects.nonNull(receiver)) {
				if (receiver.getRecievingFirstDown() == null) {
					throw new IllegalArgumentException("receiver.getRecievingFirstDown() == null");
				}
				if (receiver.getReceivingSafety() == null) {
					throw new IllegalArgumentException("receiver.getReceivingSafety() == null");
				}
				if (receiver.getReceivingTouchdown() == null) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() == null");
				}
				if (receiver.getReceivingFumble() == null) {
					throw new IllegalArgumentException("receiver.getReceivingFumble() == null");
				}
				if (receiver.getReceivingFumbleLost() == null) {
					throw new IllegalArgumentException("receiver.getReceivingFumbleLost() == null");
				}
				if (receiver.getReceivingFumbleLost() == 1 && receiver.getReceivingFumble() == 0) {
					throw new IllegalArgumentException(
							"receiver.getReceivingFumbleLost() == 1 && receiver.getReceivingFumble() == 0");
				}
			}

			if (firstDown) {
				if (pass.getPassingFirstDown() != 1
						&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
					throw new IllegalArgumentException("pass.getPassingFirstDown() != 1");
				}
				if (Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 1
						&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
					throw new IllegalArgumentException(
							"Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 1");
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException("pass.getPassingSafety() != 0");
				}
			} else {
				if (pass.getPassingFirstDown() != 0) {
					throw new IllegalArgumentException("pass.getPassingFirstDown() != 0");
				}
				if (Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0) {
					throw new IllegalArgumentException(
							"Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0");
				}
			}

			if (playPoints == 6) {
				if (pass.getPassingTouchdown() != 1) {
					throw new IllegalArgumentException("pass.getPassingTouchdown() != 1");
				}
				if (receiver.getReceivingTouchdown() != 1) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 1");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException("pass.getPassingSafety() != 0");
				}
				if (receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 0");
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("pass.getPassingTwoPointConversion() != 0");
				}
			} else if (playPoints == 2) {
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException("pass.getPassingTouchdown() != 0");
				}
				if (pass.getPassingTwoPointConversion() != 1) {
					throw new IllegalArgumentException("pass.getPassingTwoPointConversion() != 1");
				}
				if (receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 0");
				}
				if (receiver.getReceivingTwoPointConversion() != 1) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 1");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException("pass.getPassingSafety() != 0");
				}
			} else if (playPoints == 0) {
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException("pass.getPassingTouchdown() != 0");
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException(
							"Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException("pass.getPassingSafety() != 0");
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 0");
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("pass.getPassingTwoPointConversion() != 0");
				}
			} else if (playPoints == -2) {
				if (pass.getPassingSafety() != 1) {
					throw new IllegalArgumentException("pass.getPassingSafety() != 1");
				}
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException("pass.getPassingTouchdown() != 0");
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException(
							"Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							"!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()");
				}
				if (receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 0");
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("pass.getPassingTwoPointConversion() != 0");
				}
			} else if (playPoints == -6) {
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException("pass.getPassingTouchdown() != 0");
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 0");
				}
				if (defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).size() != 1) {
					throw new IllegalArgumentException(
							"defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).size() == 1");
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException("pass.getPassingSafety() != 0");
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 0");
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException("pass.getPassingTwoPointConversion() != 0");
				}
			} else {
				throw new IllegalArgumentException("playPoints");
			}
			/**
			 * PUNT Tests
			 */
		} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
			PlayerStatPuntingPojo punter = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
					.getSpecialTeam().getPunting().get(0);
			List<PlayerStatPuntReturnPojo> returner = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
					.getSpecialTeam().getPuntReturn();
			if (punter.getPuntReturnTouchdown() == null) {
				throw new IllegalArgumentException("punter.getPuntReturnTouchdown() == null");
			}
			if (returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1) {
				throw new IllegalArgumentException(
						"puntReturn.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1");
			}
			if (punter.getPuntReturnTouchdown() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntReturnTouchdown()  > 1");
			}
			if (punter.getPuntReturnTouchdown() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntReturnTouchdown()  > 1");
			}
			for (PlayerStatPuntReturnPojo puntReturn : returner) {
				if (puntReturn.getPuntReturnSafety() == null) {
					throw new IllegalArgumentException("puntReturn.getPuntReturnSafety() == null");
				}
				if (puntReturn.getPuntReturnTouchdown() == 1) {
					/**
					 * RETURN TOUCHDOWN
					 */
					if (punter.getPuntReturnTouchdown() != 1) {
						throw new IllegalArgumentException("punter.getPuntReturnTouchdown() != 1");
					}
					if (punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0) {
						throw new IllegalArgumentException(
								"punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0");
					}
					if (punter.getPuntBlocked() == 0
							&& puntReturn.getPuntReturnStartYard() + puntReturn.getPuntReturnYard() != 100) {
						throw new IllegalArgumentException("punter.getPuntBlocked() == 0\n"
								+ "					&& returner.get(0).getPuntReturnStartYard() + puntReturn.getPuntReturnYard() != 100");
					}
				}
			}

			if (punter.getPuntReturnTouchdown() == 1) {
				/**
				 * RETURN TOUCHDOWN
				 */
				if (returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).get(0)
						.getPuntReturnTouchdown() != 1) {
					throw new IllegalArgumentException(
							"returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).get(0).getPuntReturnTouchdown() != 1");
				}
				if (punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0) {
					throw new IllegalArgumentException(
							"punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0");
				}

				if (punter.getPuntBlocked() == 0 && returner.stream().filter(ret -> ret.getPuntReturn() == 1)
						.collect(Collectors.toList()).get(0).getPuntReturnStartYard()
						+ returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).get(0)
								.getPuntReturnYard() != 100) {
					throw new IllegalArgumentException(
							"returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).get(0).getPuntReturnStartYard() + returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).get(0).getPuntReturnYard() != 100");
				}
				if (punter.getPuntTouchback() == 1) {
					throw new IllegalArgumentException("punter.getPuntTouchback() == 1");
				}
				if (punter.getPuntFairCatch() == 1) {
					throw new IllegalArgumentException("punter.getPuntFairCatch() == 1");
				}
			}
			if (punter.getPuntTouchback() == 1 || punter.getPuntFairCatch() == 1) {
				if (punter.getPuntReturnTouchdown() != 0) {
					throw new IllegalArgumentException("punter.getPuntReturnTouchdown() != 0");
				}
			}
		}
		if (params.getPlay().getPlayResult().getPlayResultYardLine() > 100) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() > 100");
		}
		if (params.getPlay().getPlayResult().getPlayResultYardLine() < 0) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() < 0");
		}
		/**
		 * Score tests
		 */
		if (playPoints == 6) {
			if (!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()");
			}
			if (params.getPlay().getPlayResult().getPlayResultYardLine() != 100) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() != 100");
			}
		} else if (playPoints == 0) {
			if (!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()");
			}
			if (params.getPlay().getPlayResult().getPlayResultYardLine() == 0
					|| params.getPlay().getPlayResult().getPlayResultYardLine() == 100) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayResult().getPlayResultYardLine() == 0 || params.getPlay().getPlayResult().getPlayResultYardLine() == 100");
			}
		} else if (playPoints == -6) {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				if (defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).size() != 1) {
					throw new IllegalArgumentException(
							"defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).size() != 1");
				}
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.PAT) {
				throw new IllegalArgumentException("params.getPlay().getPlayType() == PlayTypeEnum.PAT");
			} else {
				throw new IllegalArgumentException("HANDLE THIS");
			}
			if (params.getPlay().getPlayResult().getPlayResultYardLine() != 0) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() != 0");
			}
		} else if (playPoints == -2) {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				if (defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).size() != 1) {
					throw new IllegalArgumentException(
							"defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).size() != 1");
				} else {
					throw new IllegalArgumentException("CATCH THIS");
				}
			}
			if (params.getPlay().getPlayResult().getPlayResultYardLine() != 0) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() != 0");
			}
		} else if (playPoints == 3) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
		} else if (playPoints == 2) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
			if (!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN)) {
				throw new IllegalArgumentException("!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS\n"
						+ "					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN)");
			}
		} else if (playPoints == 1) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException("!defense.isEmpty()");
			}
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				throw new IllegalArgumentException("params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT");
			}
		} else {
			throw new IllegalArgumentException("CATCH THIS");
		}

	}

}
