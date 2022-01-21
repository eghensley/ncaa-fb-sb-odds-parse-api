package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;

public final class RushValidationService {
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_DEFENSE_GET_DEFENSE_PRODUCTION = "!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()\n";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE = "!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()\n";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE = "!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()\n";
	private static final String IS_EMPTY = ".isEmpty())\n";
	private static final String PARAMS_GET_DRIVE_GET_KICKOFF = "params.getDrive().getKickoff()";
	private static final String PARAMS_GET_PLAY_GET_PLAY_TYPE_PLAY_TYPE_ENUM_PAT = "params.getPlay().getPlayType() != PlayTypeEnum.PAT";
	private static final String PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_POINTS_2 = "params.getPlay().getPlayResult().getPlayResultPoints() != 2";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().isEmpty()";
	private static final String RUN_GET_RUSHING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD = "run.getRushingYard() != params.getPlay().getPlayResult().getPlayResultYard()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().isEmpty()";
	private static final String DEFENSE_IS_EMPTY = "!defense.isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().isEmpty()";

	// Private constructor to prevent instantiation
	private RushValidationService() {
		throw new UnsupportedOperationException();
	}

	public static void validateRush(PbpServiceRequestPojo params) {

		List<PlayerStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();
		List<PlayerStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();

		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_DEFENSE_GET_DEFENSE_PRODUCTION
							+ IS_EMPTY);
		}

		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()) {
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
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0);
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

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()\n"
							+ ".getRushingStat().size() != 1");
		}

		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			DefenseValidationService.validateDef(params, def);
		}
		PbpPlayerStatRushingPojo run = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
				.getRushingStat().get(0);

		validateRushHelperNulls(run);
		validateRushHelperValues(run);
		validateRushHelperStuff(run);
		validateRushHelperKneel(run, defense);
		validateRushHelperOpenField(params, run, offPenalty);
		validateRushHelperSecondLevel(params, run, offPenalty);
		validateRushHelperRushingPower(params, run);
		validateRushHelperTwoPointConversion(params, run, defense);
		validateRushHelperPenalty(params, run, defPenalty, offPenalty);
		validateRushHelperResult(params, run, defPenalty);
		validateRushHelperHavoc(params);
	}

	private static void validateRushHelperNulls(PbpPlayerStatRushingPojo run) {
		if (Objects.isNull(run.getRushingAttempt())) {
			throw new IllegalArgumentException("run.getRushingAttempt() == null");
		}
		if (run.getPlayerName().isBlank()) {
			throw new IllegalArgumentException("run.getPlayerName() == null");
		}
		if (Objects.isNull(run.getRushingSafety())) {
			throw new IllegalArgumentException("run.getRushingSafety() == null");
		}
		if (Objects.isNull(run.getRushingKneel())) {
			throw new IllegalArgumentException("run.getRushingKneel() == null");
		}
		if (Objects.isNull(run.getRushingLineYard())) {
			throw new IllegalArgumentException("run.getRushingLineYard() == null");
		}
		if (Objects.isNull(run.getRushingSuccess())) {
			throw new IllegalArgumentException("run.getRushingSuccess() == null");
		}
		if (Objects.isNull(run.getRushingStuff())) {
			throw new IllegalArgumentException("run.getRushingStuff() == null");
		}
		if (Objects.isNull(run.getRushingYard())) {
			throw new IllegalArgumentException("run.getRushingYard() == null");
		}
		if (Objects.isNull(run.getRushingFumble())) {
			throw new IllegalArgumentException("run.getRushingFumble() == null");
		}
		if (Objects.isNull(run.getRushingFumbleLost())) {
			throw new IllegalArgumentException("run.getRushingFumbleLost() == null");
		}
		if (Objects.isNull(run.getRushingFirstDown())) {
			throw new IllegalArgumentException("run.getRushingFirstDown() == null");
		}
		if (Objects.isNull(run.getRushingTouchdown())) {
			throw new IllegalArgumentException("run.getRushingTouchdown() == null");
		}
	}

	private static void validateRushHelperValues(PbpPlayerStatRushingPojo run) {
		if (run.getRushingYard() > 10) {
			if (Boolean.FALSE.equals(run.getRushingOpenField())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(run.getRushingOpenField())");
			}
		} else {
			if (Boolean.TRUE.equals(run.getRushingOpenField())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(run.getRushingOpenField())");
			}
		}
		if (run.getRushingYard() >= 5) {
			if (Boolean.FALSE.equals(run.getRushingSecondLevel())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(run.getRushingSecondLevel())");
			}
		} else {
			if (Boolean.TRUE.equals(run.getRushingSecondLevel())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(run.getRushingSecondLevel())");
			}
		}
		if (run.getRushingAttempt() != 1) {
			throw new IllegalArgumentException("run.getRushingAttempt() != 1");
		}

	}

	private static void validateRushHelperStuff(PbpPlayerStatRushingPojo run) {
		if (run.getRushingYard() > 0 || run.getRushingKneel() > 0) {
			if (Boolean.TRUE.equals(run.getRushingStuff())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(run.getRushingStuff())");
			}
		} else {
			if (Boolean.FALSE.equals(run.getRushingStuff())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(run.getRushingStuff())");
			}
		}
	}

	private static void validateRushHelperHavoc(PbpServiceRequestPojo params) {
		if (Boolean.TRUE.equals(params.getPlay().getHavocDb())) {
			throw new IllegalArgumentException("Boolean.TRUE.equals(params.getPlay().getHavocDb())");
		}
	}

	private static void validateRushHelperKneel(PbpPlayerStatRushingPojo run,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (run.getRushingKneel() == 1) {
			if (run.getRushingYard() > 0) {
				throw new IllegalArgumentException("run.getRushingYard() > 0");
			}
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
		}
	}

	private static void validateRushHelperOpenField(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run,
			List<PlayerStatPenaltyPojo> offPenalty) {
		if (Objects.isNull(run.getRushingOpenField())) {
			throw new IllegalArgumentException("run.getRushingOpenField() == null");
		}
		if (Boolean.TRUE.equals(run.getRushingOpenField())) {
			validateRushHelperOpenFieldValues(params, run, offPenalty);
		} else {
			if (run.getRushingOpenFieldYard() != null) {
				throw new IllegalArgumentException("run.getRushingOpenFieldYard() != null");
			}
		}
	}

	private static void validateRushHelperOpenFieldValues(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run,
			List<PlayerStatPenaltyPojo> offPenalty) {
		if (Objects.isNull(run.getRushingOpenFieldYard())) {
			throw new IllegalArgumentException("run.getRushingOpenFieldYard() == null");
		}
		if (run.getRushingOpenFieldYard() != run.getRushingYard() - 10) {
			throw new IllegalArgumentException("run.getRushingOpenFieldYard() != run.getRushingYard() - 10");
		}
		if (run.getRushingOpenFieldYard() != params.getPlay().getPlayResult().getPlayResultYard() - 10) {
			if (Boolean.TRUE.equals(offPenalty.isEmpty())) {
				String openFieldRushYardLogStr = String.format("Rush open field rush yard: %s",
						run.getRushingOpenFieldYard());
				String playResultYardLogStr = String.format("Play result yard: %s",
						params.getPlay().getPlayResult().getPlayResultYard());
				String rushResultYardLogStr = String.format("Rush result yard: %s", run.getRushingYard());

				LoggingUtils.logInfo(openFieldRushYardLogStr);
				LoggingUtils.logInfo(playResultYardLogStr);
				LoggingUtils.logInfo(rushResultYardLogStr);

				throw new IllegalArgumentException(
						"run.getRushingOpenFieldYard() != params.getPlay().getPlayResult().getPlayResultYard() - 10");
			} else {
				// Nothing needed here for now
			}
		}
		if (run.getRushingOpenFieldYard() < 0) {
			throw new IllegalArgumentException("run.getRushingOpenFieldYard() < 0");
		}
	}

	private static void validateRushHelperSecondLevel(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run,
			List<PlayerStatPenaltyPojo> offPenalty) {
		if (Objects.isNull(run.getRushingSecondLevel())) {
			throw new IllegalArgumentException("run.getRushingSecondLevel() == null");
		}
		if (Boolean.TRUE.equals(run.getRushingSecondLevel())) {
			validateRushHelperSecondLevelValues(params, run, offPenalty);
		} else {
			if (run.getRushingSecondLevelYard() != null) {
				throw new IllegalArgumentException("run.getRushingSecondLevelYard() != null");
			}
		}
	}

	private static void validateRushHelperSecondLevelValues(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run,
			List<PlayerStatPenaltyPojo> offPenalty) {
		if (Objects.isNull(run.getRushingSecondLevelYard())) {
			throw new IllegalArgumentException("run.getRushingSecondLevelYard() == null");
		}
		if (run.getRushingSecondLevelYard() != Math.min(run.getRushingYard() - 5, 5)) {
			throw new IllegalArgumentException(
					"run.getRushingSecondLevelYard() != Math.min(run.getRushingYard() - 5, 5)");
		}
		Integer defPenYard;
		Integer offPenYard;
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()) {
			defPenYard = params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().get(0)
					.getPenaltyYards();
		} else {
			defPenYard = 0;
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()) {
			offPenYard = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().get(0)
					.getPenaltyYards();
		} else {
			offPenYard = 0;
		}
		if (run.getRushingSecondLevelYard() != Math
				.min(params.getPlay().getPlayResult().getPlayResultYard() - defPenYard + offPenYard - 5, 5)) {
			if (Boolean.TRUE.equals(offPenalty.isEmpty())) {
				throw new IllegalArgumentException(
						"run.getRushingSecondLevelYard() != Math.min(params.getPlay().getPlayResult().getPlayResultYard() - 5, 5)");

			} else {
				// Nothing needed here for now
			}
		}
		if (run.getRushingSecondLevelYard() < 0) {
			throw new IllegalArgumentException("run.getRushingSecondLevelYard() < 0");
		}
		if (run.getRushingSecondLevelYard() > 5) {
			throw new IllegalArgumentException("run.getRushingSecondLevelYard() > 5");
		}
	}

	private static void validateRushHelperRushingPower(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run) {
		if (Objects.isNull(run.getRushingPower())) {
			throw new IllegalArgumentException("run.getRushingPower() == null");
		}
		if (PlayTypeEnum.OFFENSE.equals(params.getPlay().getPlayType()) && ((params.getPlay().getPlayStartYard() >= 98)
				|| ((PlayDownEnum.THIRD.equals(params.getPlay().getPlayStartDown())
						|| PlayDownEnum.FOURTH.equals(params.getPlay().getPlayStartDown()))
						&& (params.getPlay().getPlayYardToGain() <= 2)))) {
			if (Boolean.FALSE.equals(run.getRushingPower())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(run.getRushingPower())");
			}
		} else {
			if (Boolean.TRUE.equals(run.getRushingPower())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(run.getRushingPower())");
			}
		}
		if (Boolean.TRUE.equals(run.getRushingPower())) {
			validateRushHelperRushingPowerSuccess(params, run);
		} else {
			if (Objects.nonNull(run.getRushingPowerSuccess())) {
				throw new IllegalArgumentException("run.getRushingPowerSuccess() != null");
			}
		}
	}

	private static void validateRushHelperRushingPowerSuccess(PbpServiceRequestPojo params,
			PbpPlayerStatRushingPojo run) {
		if (Objects.isNull(run.getRushingPowerSuccess())) {
			throw new IllegalArgumentException("run.getRushingPowerSuccess() == null");
		}
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				|| Boolean.TRUE.equals(params.getPlay().getPlayResult().getPlayResultPoints() == 6)) {
			if (Boolean.FALSE.equals(run.getRushingPowerSuccess())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(run.getRushingPowerSuccess())");
			}
		} else {
			if (Boolean.TRUE.equals(run.getRushingPowerSuccess())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(run.getRushingPowerSuccess())");
			}
		}
	}

	private static void validateRushHelperTwoPointConversion(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(run.getRushingTwoPointConversion())) {
			throw new IllegalArgumentException("run.getRushingTwoPointConversion() == null");
		}
		if (run.getRushingTwoPointConversion() == 1) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_POINTS_2);
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.PAT) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_TYPE_PLAY_TYPE_ENUM_PAT);
			}
			if (run.getRushingYard() < 3) {
				throw new IllegalArgumentException("run.getRushingYard() < 3");
			}
		}
	}

	private static void validateRushHelperPenalty(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run,
			List<PlayerStatPenaltyPojo> defPenalty, List<PlayerStatPenaltyPojo> offPenalty) {
		if (!defPenalty.isEmpty()) {
			if (defPenalty.get(0).getPenaltyYards() + run.getRushingYard() != params.getPlay().getPlayResult()
					.getPlayResultYard()) {
				throw new IllegalArgumentException(
						RUN_GET_RUSHING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD);
			}
		} else if (!offPenalty.isEmpty()) {
			if (run.getRushingYard() - offPenalty.get(0).getPenaltyYards() != params.getPlay().getPlayResult()
					.getPlayResultYard()) {
				throw new IllegalArgumentException(
						RUN_GET_RUSHING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD);
			}
		} else {
			if (run.getRushingFumble() == 0
					&& !run.getRushingYard().equals(params.getPlay().getPlayResult().getPlayResultYard())) {
				throw new IllegalArgumentException(
						RUN_GET_RUSHING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD);
			}
		}
	}

	private static void validateRushHelperResult(PbpServiceRequestPojo params, PbpPlayerStatRushingPojo run,
			List<PlayerStatPenaltyPojo> defPenalty) {
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				&& run.getRushingFirstDown() == 0
				&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && run.getRushingFirstDown() == 0");
		}
		if (Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				&& run.getRushingFirstDown() == 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == false && run.getRushingFirstDown() == 1");
		}
	}
}
