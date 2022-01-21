package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;

public final class FieldGoalValidationService {
	private static final String PARAMS_GET_DRIVE_GET_KICKOFF = "params.getDrive().getKickoff()";
	private static final String KICKING_GET_FIELD_GOAL_BLOCK_1 = "kicking.getFieldGoalBlock() == 1";
	private static final String KICKING_GET_EXTRA_POINT_BLOCK_1 = "kicking.getExtraPointBlock() == 1";
	private static final String KICKING_GET_TOTAL_POINT_0 = "kicking.getTotalPoint() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage().isEmpty()";
	private static final String KICKING_GET_EXTRA_POINT_1 = "kicking.getExtraPoint() == 1";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String KICKING_GET_FIELD_GOAL_1 = "kicking.getFieldGoal() == 1";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().isEmpty()";
	private static final String DEFENSE_IS_EMPTY = "!defense.isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().isEmpty()";
	
    // Private constructor to prevent instantiation
    private FieldGoalValidationService() {
        throw new UnsupportedOperationException();
    }
    
	public static void validateFieldGoal(PbpServiceRequestPojo params) {

		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().isEmpty()");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0);
		}

		PbpPlayerStatKickingPojo kicking = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking().get(0);
		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		validateFieldGoalHelperNulls(kicking);
		validateFieldGoalHelperValues(kicking);
		validateFieldGoalHelperKick(kicking, params);
		validateFieldGoalHelperKickBlock(kicking, defense);
		validateFieldGoalHelperKickMiss(kicking);
		validateFieldGoalHelperKickMissReason(kicking);
		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			DefenseValidationService.validateDef(params, def);
		}

	}

	private static void validateFieldGoalHelperValues(PbpPlayerStatKickingPojo kicking) {
		if (kicking.getFieldGoalAttempt() != 1) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() != 1");
		}
		if (kicking.getExtraPoint() == 1) {
			throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_1);
		}
		if (kicking.getFieldGoalYard() > 70) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() > 70");
		}
		if (kicking.getExtraPointAttempt() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() != 0");
		}
		if (kicking.getExtraPointMiss() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointMiss() != 0");
		}
		if (kicking.getExtraPointYard() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() != 0");
		}
		if (kicking.getExtraPointBlock() != 0) {
			throw new IllegalArgumentException("kicking.getExtraPointBlock() != 0");
		}
	}

	private static void validateFieldGoalHelperNulls(PbpPlayerStatKickingPojo kicking) {
		if (kicking.getPlayerName().isEmpty()) {
			throw new IllegalArgumentException("kicking.getPlayerName().isEmpty()");
		}
		if (Objects.isNull(kicking.getFieldGoalAttempt())) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() == null");
		}
		if (Objects.isNull(kicking.getExtraPoint())) {
			throw new IllegalArgumentException("kicking.getExtraPoint() == null");
		}
		if (Objects.isNull(kicking.getTotalPoint())) {
			throw new IllegalArgumentException("kicking.getTotalPoint() == null");
		}
		if (Objects.isNull(kicking.getFieldGoalYard())) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() == null");
		}
		if (Objects.isNull(kicking.getFieldGoal())) {
			throw new IllegalArgumentException("kicking.getFieldGoal() == null");
		}
		if (Objects.isNull(kicking.getExtraPointAttempt())) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() == null");
		}
		if (Objects.isNull(kicking.getExtraPointMiss())) {
			throw new IllegalArgumentException("kicking.getExtraPointMiss() == null");
		}
		if (Objects.isNull(kicking.getExtraPointYard())) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() == null");
		}
		if (Objects.isNull(kicking.getExtraPointBlock())) {
			throw new IllegalArgumentException("kicking.getExtraPointBlock() == null");
		}
	}

	private static void validateFieldGoalHelperKick(PbpPlayerStatKickingPojo kicking, PbpServiceRequestPojo params) {
		if (kicking.getFieldGoal() == 1) {
			if (kicking.getFieldGoalMiss() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalMiss() == 1");
			}
			if (kicking.getFieldGoalBlock() == 1) {
				throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_BLOCK_1);
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
	}

	private static void validateFieldGoalHelperKickBlock(PbpPlayerStatKickingPojo kicking,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(kicking.getFieldGoalBlock())) {
			throw new IllegalArgumentException("kicking.getFieldGoalBlock() == null");
		}
		if (kicking.getFieldGoalBlock() == 1) {
			if (kicking.getFieldGoalMiss() == 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalMiss() == 1");
			}
			if (kicking.getFieldGoal() == 1) {
				throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_1);
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException(KICKING_GET_TOTAL_POINT_0);
			}
			if (defense.isEmpty()) {
				throw new IllegalArgumentException("defense.isEmpty()");
			}
		} else {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
		}
	}

	private static void validateFieldGoalHelperKickMiss(PbpPlayerStatKickingPojo kicking) {
		if (Objects.isNull(kicking.getFieldGoalMiss())) {
			throw new IllegalArgumentException("kicking.getFieldGoalMiss() == null");
		}
		if (kicking.getFieldGoalMiss() == 1) {
			if (kicking.getFieldGoalBlock() == 1) {
				throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_BLOCK_1);
			}
			if (kicking.getFieldGoal() == 1) {
				throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_1);
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException(KICKING_GET_TOTAL_POINT_0);
			}
		}
	}

	private static void validateFieldGoalHelperKickMissReason(PbpPlayerStatKickingPojo kicking) {
		if (Objects.nonNull(kicking.getKickMissReason())) {
			if (kicking.getFieldGoalBlock() == 1) {
				throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_BLOCK_1);
			}
			if (kicking.getFieldGoal() == 1) {
				throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_1);
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException(KICKING_GET_TOTAL_POINT_0);
			}
			if (kicking.getFieldGoalMiss() != 1) {
				throw new IllegalArgumentException("kicking.getFieldGoalMiss() != 1");
			}
		}
	}
	
	public static void validatePat(PbpServiceRequestPojo params) {

		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().isEmpty()");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0);
		}

		PbpPlayerStatKickingPojo kicking = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking().get(0);
		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		validatePatHelperNulls(kicking);
		validatePatHelperValues(kicking);
		validatePatHelperExtraPoint(kicking, params);
		validatePatHelperExtraPointBlock(kicking, defense);
		validatePatHelperExtraPointMiss(kicking);
		validatePatHelperKickMissReason(kicking);

		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			DefenseValidationService.validateDef(params, def);
		}
	}

	private static void validatePatHelperValues(PbpPlayerStatKickingPojo kicking) {
		if (kicking.getExtraPointAttempt() != 1) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() != 1");
		}
		if (kicking.getFieldGoal() == 1) {
			throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_1);
		}
		if (kicking.getExtraPointYard() > 70) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() > 70");
		}
		if (kicking.getFieldGoalAttempt() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() != 0");
		}
		if (kicking.getFieldGoalMiss() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalMiss() != 0");
		}
		if (kicking.getFieldGoalYard() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() != 0");
		}
		if (kicking.getFieldGoalBlock() != 0) {
			throw new IllegalArgumentException("kicking.getFieldGoalBlock() != 0");
		}
	}

	private static void validatePatHelperNulls(PbpPlayerStatKickingPojo kicking) {
		if (kicking.getPlayerName().isEmpty()) {
			throw new IllegalArgumentException("kicking.getPlayerName().isEmpty()");
		}
		if (Objects.isNull(kicking.getExtraPointAttempt())) {
			throw new IllegalArgumentException("kicking.getExtraPointAttempt() == null");
		}
		if (Objects.isNull(kicking.getFieldGoal())) {
			throw new IllegalArgumentException("kicking.getFieldGoal() == null");
		}
		if (Objects.isNull(kicking.getTotalPoint())) {
			throw new IllegalArgumentException("kicking.getTotalPoint() == null");
		}
		if (Objects.isNull(kicking.getExtraPointYard())) {
			throw new IllegalArgumentException("kicking.getExtraPointYard() == null");
		}
		if (Objects.isNull(kicking.getExtraPoint())) {
			throw new IllegalArgumentException("kicking.getExtraPoint() == null");
		}
		if (Objects.isNull(kicking.getFieldGoalAttempt())) {
			throw new IllegalArgumentException("kicking.getFieldGoalAttempt() == null");
		}
		if (Objects.isNull(kicking.getFieldGoalMiss())) {
			throw new IllegalArgumentException("kicking.getFieldGoalMiss() == null");
		}
		if (Objects.isNull(kicking.getFieldGoalYard())) {
			throw new IllegalArgumentException("kicking.getFieldGoalYard() == null");
		}
		if (Objects.isNull(kicking.getFieldGoalBlock())) {
			throw new IllegalArgumentException("kicking.getFieldGoalBlock() == null");
		}
	}

	private static void validatePatHelperExtraPoint(PbpPlayerStatKickingPojo kicking, PbpServiceRequestPojo params) {
		if (kicking.getExtraPoint() == 1) {
			if (kicking.getExtraPointMiss() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPointMiss() == 1");
			}
			if (kicking.getExtraPointBlock() == 1) {
				throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_BLOCK_1);
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
	}

	private static void validatePatHelperExtraPointBlock(PbpPlayerStatKickingPojo kicking,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(kicking.getExtraPointBlock())) {
			throw new IllegalArgumentException("kicking.getExtraPointBlock() == null");
		}
		if (kicking.getExtraPointBlock() == 1) {
			if (kicking.getExtraPointMiss() == 1) {
				throw new IllegalArgumentException("kicking.getExtraPointMiss() == 1");
			}
			if (kicking.getExtraPoint() == 1) {
				throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_1);
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException(KICKING_GET_TOTAL_POINT_0);
			}
			if (defense.isEmpty()) {
				throw new IllegalArgumentException("defense.isEmpty()");
			}
		} else {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
		}
	}

	private static void validatePatHelperExtraPointMiss(PbpPlayerStatKickingPojo kicking) {
		if (Objects.isNull(kicking.getExtraPointMiss())) {
			throw new IllegalArgumentException("kicking.getExtraPointMiss() == null");
		}
		if (kicking.getExtraPointMiss() == 1) {
			if (kicking.getExtraPointBlock() == 1) {
				throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_BLOCK_1);
			}
			if (kicking.getExtraPoint() == 1) {
				throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_1);
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException(KICKING_GET_TOTAL_POINT_0);
			}
		}
	}

	private static void validatePatHelperKickMissReason(PbpPlayerStatKickingPojo kicking) {
		if (Objects.nonNull(kicking.getKickMissReason())) {
			if (kicking.getExtraPointBlock() == 1) {
				throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_BLOCK_1);
			}
			if (kicking.getExtraPoint() == 1) {
				throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_1);
			}
			if (kicking.getTotalPoint() != 0) {
				throw new IllegalArgumentException(KICKING_GET_TOTAL_POINT_0);
			}
			if (kicking.getExtraPointMiss() != 1) {
				throw new IllegalArgumentException("kicking.getExtraPointMiss() != 1");
			}
		}
	}
}
