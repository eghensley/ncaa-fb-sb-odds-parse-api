package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;

public final class PassValidationService {
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_DEFENSE_GET_DEFENSE_PRODUCTION = "!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()\n";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE = "!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()\n";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE = "!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()\n";
	private static final String IS_EMPTY = ".isEmpty())\n";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL = "params.getPlay().getPlayStartDown() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL = "params.getPlay().getPlayStartYard() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL = "params.getPlay().getPlayYardToGain() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL = "params.getPlay().getPlayCallType() == null";
	private static final String PARAMS_GET_DRIVE_GET_KICKOFF = "params.getDrive().getKickoff()";
	private static final String PARAMS_GET_PLAY_GET_PLAY_TYPE_PLAY_TYPE_ENUM_PAT = "params.getPlay().getPlayType() != PlayTypeEnum.PAT";
	private static final String PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_POINTS_2 = "params.getPlay().getPlayResult().getPlayResultPoints() != 2";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0 = "defense.stream().filter(d -> d.getInterceptionYard() > 0).collect(Collectors.toList()).isEmpty()";
	private static final String OBJECTS_NON_NULL_RECEIVER_RECEIVER_GET_RECEIVING_TOUCHDOWN_0 = "Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0";
	private static final String PASSER_GET_PASSING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD = "passer.getPassingYard() != params.getPlay().getPlayResult().getPlayResultYard()";
	private static final String PASSER_GET_PASSING_COMPLETION_0 = "passer.getPassingCompletion() != 0";
	private static final String DEFENSE_IS_EMPTY = "!defense.isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()";

	// Private constructor to prevent instantiation
	private PassValidationService() {
		throw new UnsupportedOperationException();
	}

	public static void validatePass(PbpServiceRequestPojo params) {
		PbpPlayerStatPassingPojo passer = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
				.getPassingStat().get(0);
		List<PlayerStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
		List<PlayerStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();

		PbpPlayerStatReceivingPojo receiver;
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
				.isEmpty()) {
			receiver = null;
		} else {
			receiver = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.get(0);
		}

		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
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
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
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

		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			DefenseValidationService.validateDef(params, def);
		}

		validatePassHelperNulls(passer, params);
		validatePassHelperTwoPointConversion(passer, receiver, params, defense);
		validatePassHelperInterception(passer, defense);
		validatePassHelperInterceptionTouchdown(passer, defense);
		validatePassHelperInterceptionYard(passer, defense);
		validatePassHelperBreakup(passer, defense);
		validatePassHelperCompletion(passer, receiver, defense);
		validatePassHelperFirstDown(passer, receiver, params, defPenalty);
		validatePassHelperPassingYard(passer, receiver);
		validatePassHelperTouchdown(passer, receiver, params);
		validatePassHelperDirectionYardThrown(passer);
		validatePassHelperDirectionYardThrownYardsAfterCatch(passer);
		validatePassHelperDrop(passer, receiver);
		validatePassHelperYardsAfterCatch(passer, receiver);
		validatePassHelperHurry(passer, defense);
		validatePassHelperFumble(passer, receiver, params);
		validatePassHelperSack(passer, defense);
		validatePassHelperResult(passer, params, defPenalty, offPenalty);
		validatePassHelperResultFirstDown(passer, params, defPenalty);

	}

	private static void validatePassHelperNulls(PbpPlayerStatPassingPojo passer, PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getPlayStartDown())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL);
		}
		if (Objects.isNull(params.getPlay().getPlayStartYard())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL);
		}
		if (Objects.isNull(params.getPlay().getPlayYardToGain())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL);
		}
		if (Objects.isNull(params.getPlay().getPlayCallType())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL);
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PASS) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PASS");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 1");
		}
		if (Objects.isNull(passer.getPassingAttempt())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingAttempt() == null");
		}
		if (passer.getPassingAttempt() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingAttempt() != 1");
		}

		if (passer.getPlayerName().isBlank()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPlayerName() == null");
		}
		if (Objects.isNull(passer.getPassingSafety())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSafety() == null");
		}
		if (Objects.isNull(passer.getPassingSpike())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSpike() == null");
		}
		if (passer.getPassingSpike() == 1) {
			if (passer.getPassingCompletion() == 1) {
				throw new IllegalArgumentException("passer.getPassingCompletion() == 1");
			} else {
				// nothing needed here yet
			}
		}
	}

	private static void validatePassHelperTwoPointConversion(PbpPlayerStatPassingPojo passer,
			PbpPlayerStatReceivingPojo receiver, PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingTwoPointConversion())) {
			throw new IllegalArgumentException("passer.getPassingTwoPointConversion() == null");
		}
		if (passer.getPassingTwoPointConversion() == 1) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_POINTS_2);
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.PAT) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_TYPE_PLAY_TYPE_ENUM_PAT);
			}
			if (Objects.nonNull(receiver) && receiver.getReceivingTwoPointConversion() != 1) {
				throw new IllegalArgumentException("receiver.getReceivingTwoPointConversion() != 1");
			}
			if (passer.getPassingYard() < 3) {
				throw new IllegalArgumentException("passer.getPassingYard() < 3");
			}
		}
	}

	private static void validatePassHelperInterception(PbpPlayerStatPassingPojo passer,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingInterception())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterception() == null");
		}
		if (passer.getPassingInterception() == 1) {
			validatePassHelperInterceptionValues(passer, defense);
		} else {
			validatePassHelperNoInterception(defense);
		}
	}

	private static void validatePassHelperInterceptionValues(PbpPlayerStatPassingPojo passer,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (passer.getPassingCompletion() != 0) {
			throw new IllegalArgumentException(PASSER_GET_PASSING_COMPLETION_0);
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
		if (Objects.isNull(passer.getPassingYardThrownTo())) {
			throw new IllegalArgumentException("passer.getPassingYardThrownTo() == null");
		}
		if (defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					"defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).isEmpty()");
		}
	}

	private static void validatePassHelperNoInterception(List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (!defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					"defense.stream().filter(d -> d.getInterception() == 1).collect(Collectors.toList()).isEmpty()");
		}
		if (!defense.stream().filter(d -> d.getInterceptionYard() > 1).collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0);
		}
	}

	private static void validatePassHelperInterceptionTouchdown(PbpPlayerStatPassingPojo passer,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingInterceptionTouchdown())) {
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
					.isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterceptionTouchdown() == 1).collect(Collectors.toList()).isEmpty()");
			}
		} else {
			if (!defense.stream().filter(d -> d.getInterceptionTouchdown() == 1).collect(Collectors.toList())
					.isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getInterceptionTouchdown() == 1).collect(Collectors.toList()).isEmpty()");
			}
		}
	}

	private static void validatePassHelperInterceptionYard(PbpPlayerStatPassingPojo passer,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingInterceptionYard())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterceptionYard() == null");
		}
		if (passer.getPassingInterceptionYard() > 0) {
			if (!defense.stream().filter(d -> d.getInterceptionTouchdown() > 1).collect(Collectors.toList())
					.isEmpty()) {
				throw new IllegalArgumentException(
						DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0);
			}
		} else {
			if (!defense.stream().filter(d -> d.getInterceptionYard() > 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0);
			}
		}
	}

	private static void validatePassHelperBreakup(PbpPlayerStatPassingPojo passer,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingBreakup())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingBreakup() == null");
		}
		if (passer.getPassingBreakup() == 1) {
			if (passer.getPassingCompletion() == 1) {
				throw new IllegalArgumentException("passer.getPassingCompletion() == 1");
			}
			if (defense.stream().filter(d -> d.getPassBreakUp() == 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getPassBreakUp() > 0).collect(Collectors.toList()).isEmpty()");
			}
			if (!defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).size() > 0");
			}
		} else {
			if (!defense.stream().filter(d -> d.getPassBreakUp() == 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getPassBreakUp() > 0).collect(Collectors.toList()).isEmpty()");
			}
		}
	}

	private static void validatePassHelperCompletion(PbpPlayerStatPassingPojo passer,
			PbpPlayerStatReceivingPojo receiver, List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingCompletion())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingCompletion() == null");
		}
		if (passer.getPassingCompletion() == 0) {
			validatePassHelperNoCompletion(passer, receiver, defense);
		} else {
			if (Objects.isNull(receiver) || receiver.getReceivingReception() == 0) {
				throw new IllegalArgumentException("Objects.isNull(receiver) || receiver.getReceivingReception() == 0");
			}
		}
	}

	private static void validatePassHelperNoCompletion(PbpPlayerStatPassingPojo passer,
			PbpPlayerStatReceivingPojo receiver, List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.nonNull(receiver) && receiver.getReceivingReception() != 0) {
			throw new IllegalArgumentException("Objects.nonNull(receiver) && receiver.getReceivingReception() != 0");
		}
		if (passer.getPassingYard() != 0) {
			throw new IllegalArgumentException("passer.getPassingYard() != 0");
		}
		if (passer.getPassingInterception() != 1 && passer.getPassingSack() != 1
				&& !defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
		}
		if (passer.getPassingSack() == 0
				&& !defense.stream().filter(d -> d.getTackleTotal() > 0 && d.getFumbleForced() == 0)
						.collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					"defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).size() > 0");
		}
	}

	private static void validatePassHelperFirstDown(PbpPlayerStatPassingPojo passer,
			PbpPlayerStatReceivingPojo receiver, PbpServiceRequestPojo params, List<PlayerStatPenaltyPojo> defPenalty) {
		if (Objects.isNull(passer.getPassingFirstDown())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFirstDown() == null");
		}
		if (passer.getPassingFirstDown() == 1) {
			if (Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())) {
				throw new IllegalArgumentException("!params.getPlay().getPlayResult().isPlayResultFirstDown()");
			}
			if (passer.getPassingCompletion() == 0) {
				throw new IllegalArgumentException("passer.getPassingCompletion() == 0");
			}
			if (Objects.isNull(receiver) || receiver.getRecievingFirstDown() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getRecievingFirstDown() == 0");
			}
			if (receiver.getReceivingReception() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingReception() == 0");
			}
		} else {
			validatePassHelperNoFirstDown(receiver, params, defPenalty);
		}
	}

	private static void validatePassHelperNoFirstDown(PbpPlayerStatReceivingPojo receiver, PbpServiceRequestPojo params,
			List<PlayerStatPenaltyPojo> defPenalty) {
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown()");
		}
		if (Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0) {
			throw new IllegalArgumentException("Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0");
		}
	}

	private static void validatePassHelperPassingYard(PbpPlayerStatPassingPojo passer,
			PbpPlayerStatReceivingPojo receiver) {
		if (Objects.isNull(passer.getPassingYard())) {
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
				throw new IllegalArgumentException(PASSER_GET_PASSING_COMPLETION_0);
			}

		}
	}

	private static void validatePassHelperTouchdown(PbpPlayerStatPassingPojo passer,
			PbpPlayerStatReceivingPojo receiver, PbpServiceRequestPojo params) {
		if (Objects.isNull(passer.getPassingTouchdown())) {
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
			if (receiver.getReceivingReception() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingReception() == 0");
			}
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
				throw new IllegalArgumentException(
						"!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			}
		} else {
			validatePassHelperNoTouchdown(receiver, params);
		}
	}

	private static void validatePassHelperNoTouchdown(PbpPlayerStatReceivingPojo receiver,
			PbpServiceRequestPojo params) {
		if (params.getPlay().getPlayResult().getPlayResultPoints() != 0
				&& params.getPlay().getPlayResult().getPlayResultPoints() != -6
				&& params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().getPlayResultPoints() != 0 && params.getPlay().getPlayResult().getPlayResultPoints() != -6");
		}
		if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
			throw new IllegalArgumentException(OBJECTS_NON_NULL_RECEIVER_RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
		}
	}

	private static void validatePassHelperYardsAfterCatch(PbpPlayerStatPassingPojo passer,
			PbpPlayerStatReceivingPojo receiver) {
		if (Objects.nonNull(passer.getPassingYardAfterCatch())) {

			if (passer.getPassingYardAfterCatch() == 0) {
				if (Objects.nonNull(receiver) && receiver.getReceivingYardAfterCatch() != 0) {
					throw new IllegalArgumentException(
							"Objects.nonNull(receiver) && receiver.getReceivingYardAfterCatch() != 0");
				}
				if (passer.getPassingCompletion() != 0) {
					throw new IllegalArgumentException(PASSER_GET_PASSING_COMPLETION_0);
				}
			} else {
				if (Objects.isNull(receiver) || receiver.getReceivingYardAfterCatch() == 0) {
					throw new IllegalArgumentException(
							"Objects.isNull(receiver) || receiver.getReceivingYardAfterCatch() == 0");
				}
			}
		}
	}

	private static void validatePassHelperDirectionYardThrown(PbpPlayerStatPassingPojo passer) {
		if (Objects.isNull(passer.getPassingDirection())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingDirection() == null");
		}
		if (Objects.isNull(passer.getPassingYardThrownTo())) {
			if (Objects.nonNull(passer.getPassingYardAfterCatch())) {
				throw new IllegalArgumentException("passer.getPassingYardAfterCatch() != null");
			}
			if (Objects.nonNull(passer.getPassingAirLessNeeded())) {
				throw new IllegalArgumentException("passer.getPassingAirLessNeeded() != null");
			}
		} else {
			if (passer.getPassingInterception() == 0 && Objects.isNull(passer.getPassingYardAfterCatch())) {
				throw new IllegalArgumentException("passer.getPassingYardAfterCatch() == null");
			}
			if (Objects.isNull(passer.getPassingAirLessNeeded())) {
				throw new IllegalArgumentException("passer.getPassingAirLessNeeded() == null");
			}
		}
	}

	private static void validatePassHelperDirectionYardThrownYardsAfterCatch(PbpPlayerStatPassingPojo passer) {
		if (Objects.isNull(passer.getPassingYardAfterCatch())) {
			if (passer.getPassingInterception() == 0 && Objects.nonNull(passer.getPassingYardThrownTo())) {
				throw new IllegalArgumentException("passer.getPassingYardThrownTo() != null");
			}
		} else {
			if (Objects.isNull(passer.getPassingYardThrownTo())) {
				throw new IllegalArgumentException("passer.getPassingYardThrownTo() == null");
			}
		}
	}

	private static void validatePassHelperDrop(PbpPlayerStatPassingPojo passer, PbpPlayerStatReceivingPojo receiver) {
		if (Objects.isNull(passer.getPassingDrop())) {
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
				throw new IllegalArgumentException(PASSER_GET_PASSING_COMPLETION_0);
			}

		}
	}

	private static void validatePassHelperHurry(PbpPlayerStatPassingPojo passer,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingHurry())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingHurry() == null");
		}
		if (passer.getPassingHurry() == 0) {
			if (!defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).isEmpty()");
			}
		} else {
			if (defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getQuarterbackHurry() == 1).collect(Collectors.toList()).isEmpty()");
			}
		}
	}

	private static void validatePassHelperFumble(PbpPlayerStatPassingPojo passer, PbpPlayerStatReceivingPojo receiver, PbpServiceRequestPojo params) {
		if (Objects.isNull(passer.getPassingFumble())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFumble() == null");
		}
		if (Objects.isNull(passer.getPassingFumbleLost())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFumbleLost() == null");
		}
		if (passer.getPassingFumbleLost() > 0) {
			if (passer.getPassingFumble() == 0) {
				throw new IllegalArgumentException("passer.getPassingFumble() == 0");
			}
			if (Objects.isNull(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
					.findDefenseWithFumbleRecovery())) {
				throw new IllegalArgumentException(
						"Objects.isNull(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().findDefenseWithFumbleRecovery())");
			}
		} else {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()
					&& Objects.nonNull(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseWithFumbleRecovery()) && (Objects.isNull(receiver) || receiver.getReceivingFumbleLost() == 0)) {
				throw new IllegalArgumentException(
						"Objects.nonNull(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().findDefenseWithFumbleRecovery())");
			}
		}
	}

	private static void validatePassHelperSack(PbpPlayerStatPassingPojo passer,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(passer.getPassingSack())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSack() == null");
		}
		if (passer.getPassingSack() == 0) {
			if (!defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).isEmpty()");
			}
		} else {
			if (defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getSack() == 1).collect(Collectors.toList()).isEmpty()");
			}
			if (defense.stream().filter(d -> d.getTackleYard() < 1).collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"defense.stream().filter(d -> d.getTackleYard() < 1).collect(Collectors.toList()).isEmpty()");
			}
		}

		if (Objects.isNull(passer.getPassingSackYard())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingSackYard() == null");
		}
		if (passer.getPassingSackYard() != 0) {
			if (passer.getPassingSack() == 0) {
				throw new IllegalArgumentException("passer.getPassingSack() == 0");
			} else {
				// nothing needed here yet
			}
		}
	}

	private static void validatePassHelperResult(PbpPlayerStatPassingPojo passer, PbpServiceRequestPojo params,
			List<PlayerStatPenaltyPojo> defPenalty, List<PlayerStatPenaltyPojo> offPenalty) {
		if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultYard())) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() == null");
		}
		if (!defPenalty.isEmpty()) {
			if (passer.getPassingSack() != 1
					&& defPenalty.get(0).getPenaltyYards() + passer.getPassingYard() != params.getPlay().getPlayResult()
							.getPlayResultYard()
					&& Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
				throw new IllegalArgumentException(
						PASSER_GET_PASSING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD);
			}
		} else if (!offPenalty.isEmpty()) {
			if (passer.getPassingSack() != 1 && passer.getPassingYard() - offPenalty.get(0).getPenaltyYards() != params
					.getPlay().getPlayResult().getPlayResultYard()) {
				throw new IllegalArgumentException(
						PASSER_GET_PASSING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD);
			}
		} else {
			if (passer.getPassingSack() != 1
					&& !passer.getPassingYard().equals(params.getPlay().getPlayResult().getPlayResultYard())) {
				throw new IllegalArgumentException(
						PASSER_GET_PASSING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD);
			}
		}
	}

	private static void validatePassHelperResultFirstDown(PbpPlayerStatPassingPojo passer, PbpServiceRequestPojo params,
			List<PlayerStatPenaltyPojo> defPenalty) {
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				&& passer.getPassingFirstDown() == 0
				&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ ".getPassingFirstDown() == 0");
		}
		if (Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				&& passer.getPassingFirstDown() == 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == false && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ ".getPassingFirstDown() == 1");
		}
	}

	public static void validateReceiver(PbpServiceRequestPojo params) {
		List<PlayerStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();
		List<PlayerStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
		PbpPlayerStatReceivingPojo receiver;
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
				.isEmpty()) {
			receiver = null;
		} else {
			receiver = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.get(0);
		}
		if (Objects.nonNull(receiver)) {
			validateReceiverHelperNulls(receiver);
			validateReceiverHelperTwoPointConversion(receiver, params);
			validateReceiverHelperResult(receiver, params, defPenalty, offPenalty);

		}
	}

	private static void validateReceiverHelperNulls(PbpPlayerStatReceivingPojo receiver) {
		if (receiver.getPlayerName().isEmpty()) {
			throw new IllegalArgumentException("receiver.getPlayerName().isEmpty()");
		}
		if (Objects.isNull(receiver.getReceivingReception())) {
			throw new IllegalArgumentException("receiver.getReceivingReception() == null");
		}
		if (Objects.isNull(receiver.getReceivingSafety())) {
			throw new IllegalArgumentException("receiver.getReceivingSafety() == null");
		}
		if (Objects.isNull(receiver.getReceivingTarget())) {
			throw new IllegalArgumentException("receiver.getReceivingTarget() == null");
		}
		if (Objects.isNull(receiver.getReceivingYard())) {
			throw new IllegalArgumentException("receiver.getReceivingYard() == null");
		}
		if (Objects.isNull(receiver.getReceivingTouchdown())) {
			throw new IllegalArgumentException("receiver.getReceivingTouchdown() == null");
		}
		if (Objects.isNull(receiver.getReceivingDrop())) {
			throw new IllegalArgumentException("receiver.getReceivingDrop() == null");
		}
		if (Objects.isNull(receiver.getRecievingFirstDown())) {
			throw new IllegalArgumentException("receiver.getRecievingFirstDown() == null");
		}
		if (Objects.isNull(receiver.getReceivingFumble())) {
			throw new IllegalArgumentException("receiver.getReceivingFumble() == null");
		}
		if (Objects.isNull(receiver.getReceivingFumbleLost())) {
			throw new IllegalArgumentException("receiver.getReceivingFumbleLost() == null");
		}
	}

	private static void validateReceiverHelperTwoPointConversion(PbpPlayerStatReceivingPojo receiver,
			PbpServiceRequestPojo params) {
		if (Objects.isNull(receiver.getReceivingTwoPointConversion())) {
			throw new IllegalArgumentException("receiver.getReceivingTwoPointConversion() == null");
		}
		if (receiver.getReceivingTwoPointConversion() == 1) {
			if (params.getPlay().getPlayResult().getPlayResultPoints() != 2) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_POINTS_2);
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.PAT) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_TYPE_PLAY_TYPE_ENUM_PAT);
			}
			if (receiver.getReceivingTwoPointConversion() != 1) {
				throw new IllegalArgumentException("receiver.getReceivingTwoPointConversion() != 1");
			}
			if (receiver.getReceivingYard() < 3) {
				throw new IllegalArgumentException("receiver.getReceivingYard() < 3");
			}
		}
	}

	private static void validateReceiverHelperResult(PbpPlayerStatReceivingPojo receiver, PbpServiceRequestPojo params,
			List<PlayerStatPenaltyPojo> defPenalty, List<PlayerStatPenaltyPojo> offPenalty) {
		if (!defPenalty.isEmpty()) {
			if (defPenalty.get(0).getPenaltyYards() + receiver.getReceivingYard() != params.getPlay().getPlayResult()
					.getPlayResultYard()
					&& Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
				throw new IllegalArgumentException(
						"defPenalty.get(0).getPenaltyYards() + receiver.getReceivingYard() != params.getPlay()\n"
								+ ".getPlayResult().getPlayResultYard()\n"
								+ "&& !params.getPlay().getPlayResult().isPlayResultTurnover()");
			}
		} else if (!offPenalty.isEmpty()) {
			if (receiver.getReceivingYard() - offPenalty.get(0).getPenaltyYards() != params.getPlay().getPlayResult()
					.getPlayResultYard()) {
				throw new IllegalArgumentException(
						"receiver.getReceivingYard() - offPenalty.get(0).getPenaltyYards() != params.getPlay()\n"
								+ ".getPlayResult().getPlayResultYard()");
			}
		} else {
			if (!receiver.getReceivingYard().equals(params.getPlay().getPlayResult().getPlayResultYard())) {
				throw new IllegalArgumentException(
						"receiver.getReceivingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
			}
		}
	}
}
