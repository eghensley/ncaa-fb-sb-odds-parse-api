package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate.FieldGoalValidationService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate.KickoffValidationService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate.OffenseValidationService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate.PassValidationService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate.PuntValidationService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate.RushValidationService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;

public final class PbpValidateService {
	private static final String PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD_NULL = "Objects.nonNull(params.getPlay().getPlayResult().getPlayResultYard())";
	private static final String PARAMS_GET_PLAY_GET_HAVOC_NULL = "params.getPlay().getHavoc())";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL = "Objects.isNull(params.getPlay().getPlayStartDown())";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL = "Objects.isNull(params.getPlay().getPlayStartYard())";
	private static final String PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL = "Objects.isNull(params.getPlay().getPlayYardToGain())";
	private static final String PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL = "Objects.isNull(params.getPlay().getPlayCallType())";

	// Private constructor to prevent instantiation
	private PbpValidateService() {
		throw new UnsupportedOperationException();
	}

	public static void validate(PbpServiceRequestPojo params) {
		try {

			validateHelperPlayNulls(params);
			validateHelperPlayValues(params);

			if (!PlayTypeEnum.KICKOFF.equals(params.getPlay().getPlayType())
					&& !PlayTypeEnum.PAT.equals(params.getPlay().getPlayType())) {
				if (Objects.isNull(params.getPlay().getPlayFieldZone())) {
					throw new IllegalArgumentException("Objects.isNull(params.getPlay().getPlayFieldZone())");
				}
				if (!PlayTypeEnum.PUNT.equals(params.getPlay().getPlayType())
						&& !PlayCallTypeEnum.FG.equals(params.getPlay().getPlayCallType())) {
					validateHelperPlayOffense(params);
				} else {
					validateHelperPlayPunt(params);
				}
			} else {
				validateHelperPlayKickoffPat(params);
			}
			if (Boolean.FALSE.equals(params.getPlay().getNoPlayPenalty())) {
				validateHelperPlayValidPlay(params);
			}
			validatePenalty(params);
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void validateHelperPlayValidPlay(PbpServiceRequestPojo params) {
		if (PlayCallTypeEnum.RUN.equals(params.getPlay().getPlayCallType())) {
			RushValidationService.validateRush(params);
		} else if (PlayCallTypeEnum.PUNT.equals(params.getPlay().getPlayCallType())) {
			PuntValidationService.validatePunt(params);
		} else if (PlayCallTypeEnum.PASS.equals(params.getPlay().getPlayCallType())) {
			PassValidationService.validateReceiver(params);
			PassValidationService.validatePass(params);
		} else if (PlayCallTypeEnum.FG.equals(params.getPlay().getPlayCallType())) {
			FieldGoalValidationService.validateFieldGoal(params);
		} else if (PlayCallTypeEnum.PAT.equals(params.getPlay().getPlayCallType())) {
			FieldGoalValidationService.validatePat(params);
		} else if (PlayTypeEnum.KICKOFF.equals(params.getPlay().getPlayType())) {
			KickoffValidationService.validateKickoff(params);
		}
		OffenseValidationService.validateOffense(params);
	}

	private static void validateHelperPlayOffense(PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getPassingDown())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getPassingDown())");
		}
		if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultSuccess())) {
			throw new IllegalArgumentException(
					"Objects.isNull(params.getPlay().getPlayResult().getPlayResultSuccess())");
		}
		if (Boolean.FALSE.equals(params.getPlay().getNoPlayPenalty())) {
			validateHelperPlayOffenseHavoc(params);
			validateHelperPlayOffenseDefeat(params);
		} else {
			if (Objects.nonNull(params.getPlay().getHavoc())) {
				throw new IllegalArgumentException("Objects.nonNull(params.getPlay().getHavoc())");
			}
			if (Objects.nonNull(params.getPlay().getHavocFront())) {
				throw new IllegalArgumentException("Objects.nonNull(params.getPlay().getHavocFront())");
			}
			if (Objects.nonNull(params.getPlay().getHavocDb())) {
				throw new IllegalArgumentException("Objects.nonNull(params.getPlay().getHavocDb())");
			}
		}
	}

	private static void validateHelperPlayOffenseHavoc(PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getHavoc())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getHavoc())");
		}
		if (Objects.isNull(params.getPlay().getHavocFront())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getHavocFront())");
		}
		if (Objects.isNull(params.getPlay().getHavocDb())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getHavocDb())");
		}
		if (Boolean.TRUE.equals(params.getPlay().getHavocFront())
				|| Boolean.TRUE.equals(params.getPlay().getHavocDb())) {
			if (Boolean.FALSE.equals(params.getPlay().getHavoc())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(params.getPlay().getHavoc())");
			}
		} else {
			if (Boolean.TRUE.equals(params.getPlay().getHavoc())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(params.getPlay().getHavoc())");
			}
		}
	}

	private static void validateHelperPlayOffenseDefeat(PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getDefeat())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getDefeat())");
		}
		if (((PlayDownEnum.THIRD.equals(params.getPlay().getPlayStartDown())
				|| PlayDownEnum.FOURTH.equals(params.getPlay().getPlayStartDown()))
				&& Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown()))
				|| Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())
				|| params.getPlay().getPlayResult().getPlayResultYard() < 0) {
			if (Boolean.FALSE.equals(params.getPlay().getDefeat())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(params.getPlay().getDefeat())");
			}
		} else {
			if (Boolean.TRUE.equals(params.getPlay().getDefeat())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(params.getPlay().getDefeat())");
			}
		}
	}

	private static void validateHelperPlayKickoffPat(PbpServiceRequestPojo params) {
		if (Objects.nonNull(params.getPlay().getPlayFieldZone())) {
			throw new IllegalArgumentException("params.getPlay().getPlayFieldZone() )");
		}
		if (Objects.nonNull(params.getPlay().getPassingDown())) {
			throw new IllegalArgumentException("params.getPlay().getPassingDown() )");
		}
		if (Objects.nonNull(params.getPlay().getPlayResult().getPlayResultSuccess())) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultSuccess() )");
		}
		if (Objects.nonNull(params.getPlay().getDefeat())) {
			throw new IllegalArgumentException("params.getPlay().getDefeat() )");
		}
		if (Objects.nonNull(params.getPlay().getHavoc())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_HAVOC_NULL);
		}
		if (Objects.nonNull(params.getPlay().getHavocFront())) {
			throw new IllegalArgumentException("params.getPlay().getHavocFront() )");
		}
		if (Objects.nonNull(params.getPlay().getHavocDb())) {
			throw new IllegalArgumentException("params.getPlay().getHavocDb() )");
		}
	}

	private static void validateHelperPlayPunt(PbpServiceRequestPojo params) {
		if (Objects.nonNull(params.getPlay().getPassingDown())) {
			throw new IllegalArgumentException("params.getPlay().getPassingDown() )");
		}
		if (Objects.nonNull(params.getPlay().getPlayResult().getPlayResultSuccess())) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultSuccess() )");
		}
		if (Objects.nonNull(params.getPlay().getDefeat())) {
			throw new IllegalArgumentException("params.getPlay().getDefeat() )");
		}
		if (Objects.nonNull(params.getPlay().getHavoc())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_HAVOC_NULL);
		}
		if (Objects.nonNull(params.getPlay().getHavocFront())) {
			throw new IllegalArgumentException("params.getPlay().getHavocFront() )");
		}
		if (Objects.nonNull(params.getPlay().getHavocDb())) {
			throw new IllegalArgumentException("params.getPlay().getHavocDb() )");
		}
	}

	private static void validateHelperPlayNulls(PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getPlayStartDown())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL);
		}
		if (Objects.isNull(params.getPlay().getPlayStartYard())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL);
		}
		if (Objects.isNull(params.getPlay().getPlayTempo())) {
			throw new IllegalArgumentException("params.getPlay().getPlayTempo())");
		}
		if (Objects.isNull(params.getPlay().getPlayStartPossessionTeamId())) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartPossessionTeamId())");
		}
		if (Objects.isNull(params.getPlay().getNoPlayPenalty())) {
			throw new IllegalArgumentException("params.getPlay().getNoPlayPenalty())");
		}
		if (Objects.isNull(params.getPlay().getPlayResult().isPlayResultFirstDown())) {
			throw new IllegalArgumentException(
					"Objects.isNull(params.getPlay().getPlayResult().isPlayResultFirstDown())");
		}
		if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultPossessionTeamId())) {
			throw new IllegalArgumentException(
					"Objects.isNull(params.getPlay().getPlayResult().getPlayResultPossessionTeamId())");
		}
		if (!PlayTypeEnum.PAT.equals(params.getPlay().getPlayType())
				&& Objects.isNull(params.getPlay().getPlayResult().getPlayResultYardLine())) {
			throw new IllegalArgumentException(
					"Objects.isNull(params.getPlay().getPlayResult().getPlayResultYardLine())");
		}
		if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultHomeScore())) {
			throw new IllegalArgumentException(
					"Objects.isNull(params.getPlay().getPlayResult().getPlayResultHomeScore())");
		}
		if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultAwayScore())) {
			throw new IllegalArgumentException(
					"Objects.isNull(params.getPlay().getPlayResult().getPlayResultAwayScore())");
		}
		if (Objects.isNull(params.getPlay().getPlayStartHomeScore())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getPlayStartHomeScore())");
		}
		if (Objects.isNull(params.getPlay().getPlayStartAwayScore())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getPlayStartAwayScore())");
		}
		if (Objects.isNull(params.getPlay().getGarbageTime())) {
			throw new IllegalArgumentException("Objects.isNull(params.getPlay().getGarbageTime())");
		}
	}

	private static void validateHelperPlayValues(PbpServiceRequestPojo params) {
		if (!PlayTypeEnum.KICKOFF.equals(params.getPlay().getPlayType())
				&& !PlayTypeEnum.PENALTY.equals(params.getPlay().getPlayType())
				&& !PlayCallTypeEnum.PAT.equals(params.getPlay().getPlayCallType())
				&& Objects.isNull(params.getPlay().getPlayYardToGain())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL);
		}
		if (!PlayTypeEnum.KICKOFF.equals(params.getPlay().getPlayType())
				&& Objects.isNull(params.getPlay().getPlayCallType())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL);
		}
		if ((!PlayCallTypeEnum.PUNT.equals(params.getPlay().getPlayCallType())
				&& !PlayTypeEnum.KICKOFF.equals(params.getPlay().getPlayType())
				&& !PlayCallTypeEnum.FG.equals(params.getPlay().getPlayCallType())
				&& !PlayCallTypeEnum.PAT.equals(params.getPlay().getPlayCallType()))
				|| Boolean.TRUE.equals(params.getPlay().getNoPlayPenalty())) {
			if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultYard())) {
				throw new IllegalArgumentException(
						"Objects.isNull(params.getPlay().getPlayResult().getPlayResultYard())");
			}
		} else {
			if (Objects.nonNull(params.getPlay().getPlayResult().getPlayResultYard())) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD_NULL);
			}
		}

		if (params.getPlay().getPlayStartHomeScore() > params.getPlay().getPlayResult().getPlayResultHomeScore()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayStartHomeScore() > params.getPlay().getPlayResult().getPlayResultHomeScore()");
		}
		if (params.getPlay().getPlayStartAwayScore() > params.getPlay().getPlayResult().getPlayResultAwayScore()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayStartAwayScore() > params.getPlay().getPlayResult().getPlayResultAwayScore()");
		}
	}

	private static void validatePenalty(PbpServiceRequestPojo params) {
		List<PlayerStatPenaltyPojo> offensePenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
		List<PlayerStatPenaltyPojo> defensePenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();

		for (PlayerStatPenaltyPojo offPen : offensePenalty) {
			if (offPen.getPenaltyFirstDown() == 1) {
				throw new IllegalArgumentException("offPen.getPenaltyFirstDown() == 1");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() > 0) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() > 0");
			}
		}
		for (PlayerStatPenaltyPojo defPen : defensePenalty) {
			if (defPen.getPenaltyFirstDown() == 1
					&& Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())) {
				throw new IllegalArgumentException(
						"defPen.getPenaltyFirstDown() == 1 && !params.getPlay().getPlayResult().isPlayResultFirstDown()");
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() < 0) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() < 0");
			}
		}
	}

}
