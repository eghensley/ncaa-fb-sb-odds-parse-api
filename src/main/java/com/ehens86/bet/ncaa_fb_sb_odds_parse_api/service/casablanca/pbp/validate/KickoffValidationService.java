package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.Objects;

import org.apache.commons.lang.StringUtils;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;

public final class KickoffValidationService {
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_DEFENSE_GET_DEFENSE_PRODUCTION = "!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()\n";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE = "!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()\n";
	private static final String PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD_NULL = "params.getPlay().getPlayResult().getPlayResultYard() != null";
	private static final String IS_EMPTY = ".isEmpty())\n";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL = "params.getPlay().getPlayStartYard() == null";
	private static final String PARAMS_GET_DRIVE_GET_KICKOFF = "params.getDrive().getKickoff()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0 = "kickReturn\n";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0 = PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02;
	private static final String GET_KICKOFF_FAIR_CATCH_0 = ".getKickoffFairCatch() != 0";
	private static final String GET_KICKOFF_RETURN_YARD_25 = ".getKickoffReturnYard() != 25";
	private static final String GET_KICKOFF_OUT_OF_BOUNDS_0 = ".getKickoffOutOfBounds() != 0";
	private static final String GET_KICKOFF_ONSIDE_SUCCESS_NULL = ".getKickoffOnsideSuccess() != null";
	private static final String GET_KICKOFF_RETURN_TOUCHDOWN_0 = ".getKickoffReturnTouchdown() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0 = PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02;
	private static final String GET_KICKOFF_ONSIDE_ATTEMPT_0 = ".getKickoffOnsideAttempt() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0 = "kickoff\n";
	
    // Private constructor to prevent instantiation
    private KickoffValidationService() {
        throw new UnsupportedOperationException();
    }
    
	public static void validateKickoff(PbpServiceRequestPojo params) {

		List<PbpPlayerStatDefenseProductionPojo> coverage = params.getPlay().getPlayerStat()
				.get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage();

		for (PbpPlayerStatDefenseProductionPojo cov : coverage) {
			DefenseValidationService.validateDef(params, cov);
		}

		if (Boolean.FALSE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}

		validateKickoffHelperPlayType(params);
		validateKickoffHelperPlay(params);

		PbpPlayerStatKickoffPojo kickoff = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getSpecialTeam().getKickoff().get(0);
		PbpPlayerStatKickReturnPojo kickReturn;
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.isEmpty()) {
			kickReturn = null;
		} else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().size() != 1");
		} else {
			kickReturn = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
					.getKickReturn().get(0);
		}

		validateKickoffHelperKickoffNull(kickoff);
		validateKickoffHelperKickoffValue(kickoff);
		
		if (Objects.nonNull(kickReturn)) {
			validateKickoffHelperKickReturnNull(kickReturn);
			validateKickoffHelperKickReturnValue(params, kickReturn);
		}

		if (kickoff.getKickoffTouchback() == 1) {
			validateKickoffHelperTouchback(params, kickoff);
		}
		if (kickoff.getKickoffFairCatch() == 1) {
			validateKickoffHelperFairCatch(params, kickoff, kickReturn);
		}
		if (kickoff.getKickoffOutOfBounds() == 1) {
			validateKickoffHelperOutOfBounds(params, kickoff);
		}
		if (kickoff.getKickoffOnsideAttempt() == 1) {
			validateKickoffHelperOnside(params, kickoff);
		}


	}
	
	private static void validateKickoffHelperPlayType(PbpServiceRequestPojo params) {
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					"!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()\n"
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_DEFENSE_GET_DEFENSE_PRODUCTION
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
				.isEmpty()) {
			throw new IllegalArgumentException(
					"!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()\n"
							+ IS_EMPTY);
		}
	}

	private static void validateKickoffHelperPlay(PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getPlayStartYard())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL);
		}
		if (Objects.nonNull(params.getPlay().getPlayYardToGain())) {
			throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() != null");
		}
		if (!PlayCallTypeEnum.KICKOFF.equals(params.getPlay().getPlayCallType())) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.KICKOFF");
		}
		if (Objects.nonNull(params.getPlay().getPlayResult().getPlayResultYard())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD_NULL);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 1");
		}
	}
	
	private static void validateKickoffHelperKickoffNull(PbpPlayerStatKickoffPojo kickoff) {
		if (StringUtils.isBlank(kickoff.getPlayerName())) {
			throw new IllegalArgumentException("StringUtils.isBlank(kickoff.getPlayerName())");
		}
		if (Objects.isNull(kickoff.getKickoffYard())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffYard() == null");
		}
		if (Objects.isNull(kickoff.getKickoffTouchback())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffTouchback() == null");
		}
		if (Objects.isNull(kickoff.getKickoffOnsideSuccess())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffOnsideSuccess() == null");
		}
		if (Objects.isNull(kickoff.getKickoffOnsideAttempt())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffOnsideAttempt() == null");
		}
		if (Objects.isNull(kickoff.getKickoffReturnYard())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffReturnYard() == null");
		}
		if (Objects.isNull(kickoff.getKickoffOutOfBounds())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffOutOfBounds() == null");
		}
		if (Objects.isNull(kickoff.getKickoffReturnTouchdown())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffReturnTouchdown() == null");
		}
		if (Objects.isNull(kickoff.getKickoffLandYard())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffLandYard() == null");
		}
		if (Objects.isNull(kickoff.getKickoffFairCatch())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffFairCatch() == null");
		}
		if (Objects.isNull(kickoff.getKickoff())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoff() == null");
		}
	}

	private static void validateKickoffHelperKickoffValue(PbpPlayerStatKickoffPojo kickoff) {
		if (kickoff.getKickoffTouchback() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffTouchback() > 1");
		}
		if (kickoff.getKickoffOnsideAttempt() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffOnsideAttempt() > 1");
		}
		if (kickoff.getKickoffOutOfBounds() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffOutOfBounds() > 1");
		}
		if (kickoff.getKickoffReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffReturnTouchdown()  > 1");
		}
		if (kickoff.getKickoffFairCatch() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffFairCatch()  > 1");
		}
		if (kickoff.getKickoff() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoff() > 1");
		}
	}

		
	private static void validateKickoffHelperKickReturnNull(PbpPlayerStatKickReturnPojo kickReturn) {
		if (Objects.isNull(kickReturn.getPlayerName())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getPlayerName() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturn())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturn() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturnYard())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnYard() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturnTouchdown())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnTouchdown() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturnFairCatch())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnFairCatch() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturnStartYard())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnStartYard() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturnSafety())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnSafety() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturnFumble())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnFumble() == null");
		}
		if (Objects.isNull(kickReturn.getKickReturnFumbleLost())) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnFumbleLost() == null");
		}
	}

	private static void validateKickoffHelperKickReturnValue(PbpServiceRequestPojo params, PbpPlayerStatKickReturnPojo kickReturn) {
		if (kickReturn.getKickReturn() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturn() > 1");
		}
		if (kickReturn.getKickReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnTouchdown() > 1");
		}
		if (kickReturn.getKickReturnFairCatch() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnFairCatch() > 1");
		}
		if (kickReturn.getKickReturnStartYard() > 100 - params.getPlay().getPlayStartYard()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnStartYard() > 100 - params.getPlay().getPlayStartYard()");
		}
		if (kickReturn.getKickReturnFumble() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnFumble() > 1");
		}
		if (kickReturn.getKickReturnFumbleLost() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
							+ ".getKickReturnFumbleLost() > 1");
		}
	}

	private static void validateKickoffHelperTouchback(PbpServiceRequestPojo params, PbpPlayerStatKickoffPojo kickoff) {
			/**
			 * TOUCHBACK
			 */
			if (kickoff.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (kickoff.getKickoffOnsideSuccess() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (kickoff.getKickoffReturnYard() != 25) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (kickoff.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (kickoff.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (kickoff.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
					.isEmpty()) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
					.isEmpty()) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		
	}

	private static void validateKickoffHelperFairCatch(PbpServiceRequestPojo params, PbpPlayerStatKickoffPojo kickoff,
			PbpPlayerStatKickReturnPojo kickReturn) {
			/**
			 * FAIR CATCH
			 */
			if (Objects.isNull(kickReturn)) {
				throw new IllegalArgumentException("Objects.isNull(kickReturn)");
			}
			if (params.getPlay().getPlayResult().getPlayResultYardLine() < 25) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() < 25");
			}
			if (kickoff.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (kickoff.getKickoffOnsideSuccess() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (kickoff.getKickoffReturnYard() != 25
					- (100 - (params.getPlay().getPlayStartYard() + params.getPlay().getPlayerStat()
							.get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0).getKickoffYard()))) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (kickoff.getKickoffTouchback() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ ".getKickoffTouchback() != 0");
			}
			if (kickoff.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (kickoff.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
					.isEmpty()) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
					.size() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().size() != 1");
			}
			if (kickReturn.getKickReturnFairCatch() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0).getKickReturnFairCatch() != 1");
			}
			if (kickReturn.getKickReturnStartYard() + kickReturn.getKickReturnYard() != 25) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ ".getKickReturnStartYard()\n" + "+ kickReturn\n" + ".getKickReturnYard() != 25");
			}
		
	}

	private static void validateKickoffHelperOutOfBounds(PbpServiceRequestPojo params, PbpPlayerStatKickoffPojo kickoff) {
		/**
		 * OUT OF BOUNDS
		 */
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()");
		}
		if (kickoff.getKickoffOnsideAttempt() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
		}
		if (kickoff.getKickoffOnsideSuccess() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
		}
		if (kickoff.getKickoffReturnYard() != 35) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffReturnYard() != 35");
		}
		if (kickoff.getKickoffTouchback() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ ".getKickoffTouchback() != 0");
		}
		if (kickoff.getKickoffReturnTouchdown() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
		}
		if (kickoff.getKickoffFairCatch() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ GET_KICKOFF_FAIR_CATCH_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
		}

	}

	private static void validateKickoffHelperOnside(PbpServiceRequestPojo params, PbpPlayerStatKickoffPojo kickoff) {
			/**
			 * ONSIDE
			 */

			if (kickoff.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (kickoff.getKickoffOnsideSuccess() != 0 && params.getPlay().getPlayerStat().get(params.getDefenseTeam())
					.getSpecialTeam().getKickoff().get(0).getKickoffReturnYard() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (kickoff.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (kickoff.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (kickoff.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
					.isEmpty()) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (kickoff.getKickoffOnsideSuccess() != 0 && !params.getPlay().getPlayerStat()
					.get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().isEmpty()) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		
	}
}
