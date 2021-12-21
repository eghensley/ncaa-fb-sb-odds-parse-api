package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;

@Service
public class PbpValidateService {
	private static final String PLAY_RESULT_YARD_S = "Play Result Yard: %s";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL = "params.getPlay().getPlayStartDown() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL = "params.getPlay().getPlayStartYard() == null";
	private static final String PASS_GET_PASSING_TOUCHDOWN_0 = "pass.getPassingTouchdown() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL = "params.getPlay().getPlayYardToGain() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL = "params.getPlay().getPlayCallType() == null";
	private static final String PARAMS_GET_DRIVE_GET_KICKOFF = "params.getDrive().getKickoff()";
	private static final String KICKING_GET_FIELD_GOAL_BLOCK_1 = "kicking.getFieldGoalBlock() == 1";
	private static final String KICKING_GET_EXTRA_POINT_BLOCK_1 = "kicking.getExtraPointBlock() == 1";
	private static final String KICKING_GET_TOTAL_POINT_0 = "kicking.getTotalPoint() != 0";
	private static final String SIZE_0 = "				.size() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage().size() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAY_TYPE_PLAY_TYPE_ENUM_PAT = "params.getPlay().getPlayType() != PlayTypeEnum.PAT";
	private static final String PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_POINTS_2 = "params.getPlay().getPlayResult().getPlayResultPoints() != 2";
	private static final String KICKING_GET_EXTRA_POINT_1 = "kicking.getExtraPoint() == 1";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().size() != 0";
	private static final String DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0 = "defense.stream().filter(d -> d.getInterceptionYard() > 0).collect(Collectors.toList()).size() != 0";
	private static final String OBJECTS_NON_NULL_RECEIVER_RECEIVER_GET_RECEIVING_TOUCHDOWN_0 = "Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0";
	private static final String KICKING_GET_FIELD_GOAL_1 = "kicking.getFieldGoal() == 1";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn().size() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 0";
	private static final String RUN_GET_RUSHING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD = "run.getRushingYard() != params.getPlay().getPlayResult().getPlayResultYard()";
	private static final String PASSER_GET_PASSING_YARD_PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD = "passer.getPassingYard() != params.getPlay().getPlayResult().getPlayResultYard()";
	private static final String PASSER_GET_PASSING_COMPLETION_0 = "passer.getPassingCompletion() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 0";
	private static final String DEFENSE_IS_EMPTY = "!defense.isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage().size() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)\n";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0";
	private static final String PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0 = "punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0";
	private static final String PUNTER_GET_PUNT_RETURN_TOUCHDOWN_0 = "punter.getPuntReturnTouchdown() != 0";
	private static final String PASS_GET_PASSING_SAFETY_0 = "pass.getPassingSafety() != 0";
	private static final String COVERAGE_SIZE_0 = "coverage.size() != 0";
	private static final String DEF_GET_TACKLE_TOTAL_0 = "def.getTackleTotal() == 0";
	private static final String DEF_GET_TACKLE_FOR_LOSS_0 = "def.getTackleForLoss() == 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().size() != 0";
	private static final String RUSH_GET_RUSHING_TWO_POINT_CONVERSION_0 = "rush.getRushingTwoPointConversion() != 0";
	private static final String RUSH_GET_RUSHING_SAFETY_0 = "rush.getRushingSafety() != 0";
	private static final String RUSH_GET_RUSHING_TOUCHDOWN_0 = "rush.getRushingTouchdown() != 0";
	private static final String PASS_GET_PASSING_TWO_POINT_CONVERSION_0 = "pass.getPassingTwoPointConversion() != 0";
	private static final String DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY = "!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()";
	private static final String RECEIVER_GET_RECEIVING_TOUCHDOWN_0 = "receiver.getReceivingTouchdown() != 0";
	private static final String DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY = "!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0 = PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02;
	private static final String GET_KICKOFF_FAIR_CATCH_0 = "						.getKickoffFairCatch() != 0";
	private static final String GET_KICKOFF_RETURN_YARD_25 = "						.getKickoffReturnYard() != 25";
	private static final String GET_KICKOFF_OUT_OF_BOUNDS_0 = "						.getKickoffOutOfBounds() != 0";
	private static final String GET_KICKOFF_ONSIDE_SUCCESS_NULL = "						.getKickoffOnsideSuccess() != null";
	private static final String GET_KICKOFF_RETURN_TOUCHDOWN_0 = "						.getKickoffReturnTouchdown() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0 = PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02;
	private static final String GET_KICKOFF_ONSIDE_ATTEMPT_0 = "						.getKickoffOnsideAttempt() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)\n";

	private final LoggingUtils loggingUtils;

	public PbpValidateService(LoggingUtils loggingUtils) {
		this.loggingUtils = loggingUtils;
	}

	public void validate(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayStartDown() == null) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL);
			}
			if (params.getPlay().getPlayStartYard() == null) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL);
			}
			if (params.getPlay().getPlayTempo() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayTempo() == null");
			}
			if (params.getPlay().getPlayStartPossessionTeamId() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayStartPossessionTeamId() == null");
			}
			if (params.getPlay().getNoPlayPenalty() == null) {
				throw new IllegalArgumentException("params.getPlay().getNoPlayPenalty() == null");
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
					&& params.getPlay().getPlayType() != PlayTypeEnum.PENALTY
					&& params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT
					&& params.getPlay().getPlayYardToGain() == null) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL);
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF && params.getPlay().getPlayCallType() == null) {
				throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL);
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
			if (params.getPlay().getPlayResult().getPlayResultPossessionTeamId() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayResult().getPlayResultPossessionTeamId() == null");
			}
			if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT
					&& params.getPlay().getPlayResult().getPlayResultYardLine() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() == null");
			}
			if (params.getPlay().getPlayResult().getPlayResultHomeScore() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultHomeScore() == null");
			}
			if (params.getPlay().getPlayResult().getPlayResultAwayScore() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultAwayScore() == null");
			}
			if (params.getPlay().getPlayStartHomeScore() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayStartHomeScore() == null");
			}
			if (params.getPlay().getPlayStartAwayScore() == null) {
				throw new IllegalArgumentException("params.getPlay().getPlayStartAwayScore() == null");
			}

			if (params.getPlay().getPlayStartHomeScore() > params.getPlay().getPlayResult().getPlayResultHomeScore()) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayStartHomeScore() > params.getPlay().getPlayResult().getPlayResultHomeScore()");
			}
			if (params.getPlay().getPlayStartAwayScore() > params.getPlay().getPlayResult().getPlayResultAwayScore()) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayStartAwayScore() > params.getPlay().getPlayResult().getPlayResultAwayScore()");
			}
			if (params.getPlay().getGarbageTime() == null) {
				throw new IllegalArgumentException("params.getPlay().getGarbageTime() == null");
			}

			if (!PlayTypeEnum.KICKOFF.equals(params.getPlay().getPlayType())
					&& !PlayTypeEnum.PAT.equals(params.getPlay().getPlayType())) {
				if (params.getPlay().getPlayFieldZone() == null) {
					throw new IllegalArgumentException("params.getPlay().getPlayFieldZone() == null");
				}
				if (!PlayTypeEnum.PUNT.equals(params.getPlay().getPlayType())) {
					if (params.getPlay().getPassingDown() == null) {
						throw new IllegalArgumentException("params.getPlay().getPassingDown() == null");
					}
					if (params.getPlay().getPlayResult().getPlayResultSuccess() == null) {
						throw new IllegalArgumentException(
								"params.getPlay().getPlayResult().getPlayResultSuccess() == null");
					}
					if (params.getPlay().getDefeat() == null) {
						throw new IllegalArgumentException("params.getPlay().getDefeat() == null");
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
				} else {
					if (params.getPlay().getPassingDown() != null) {
						throw new IllegalArgumentException("params.getPlay().getPassingDown() != null");
					}
					if (params.getPlay().getPlayResult().getPlayResultSuccess() != null) {
						throw new IllegalArgumentException(
								"params.getPlay().getPlayResult().getPlayResultSuccess() != null");
					}
					if (params.getPlay().getDefeat() != null) {
						throw new IllegalArgumentException("params.getPlay().getDefeat() != null");
					}
				}
			} else {
				if (params.getPlay().getPlayFieldZone() != null) {
					throw new IllegalArgumentException("params.getPlay().getPlayFieldZone() != null");
				}
				if (params.getPlay().getPassingDown() != null) {
					throw new IllegalArgumentException("params.getPlay().getPassingDown() != null");
				}
				if (params.getPlay().getPlayResult().getPlayResultSuccess() != null) {
					throw new IllegalArgumentException(
							"params.getPlay().getPlayResult().getPlayResultSuccess() != null");
				}
				if (params.getPlay().getDefeat() != null) {
					throw new IllegalArgumentException("params.getPlay().getDefeat() != null");
				}
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
			loggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private void validatePenalty(PbpServiceRequestPojo params) {
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

	private void validateKickoff(PbpServiceRequestPojo params) {

		List<PbpPlayerStatDefenseProductionPojo> coverage = params.getPlay().getPlayerStat()
				.get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage();

		for (PbpPlayerStatDefenseProductionPojo cov : coverage) {
			validateDef(params, cov);
		}

		if (Boolean.FALSE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
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
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}

		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL);
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
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 1");
		}
		if (StringUtils.isBlank(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
				.getKickoff().get(0).getPlayerName())) {
			throw new IllegalArgumentException(
					"StringUtils.isBlank(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0).getPlayerName())");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffYard() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffTouchback() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffTouchback() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOnsideSuccess() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOnsideSuccess() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOnsideAttempt() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOnsideAttempt() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffReturnYard() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffReturnYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOutOfBounds() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOutOfBounds() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffReturnTouchdown() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffReturnTouchdown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffLandYard() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffLandYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffFairCatch() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffFairCatch() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoff() == null) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoff() == null");
		}

		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffTouchback() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffTouchback() > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOnsideAttempt() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOnsideAttempt() > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOutOfBounds() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffOutOfBounds() > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffReturnTouchdown()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffFairCatch() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoffFairCatch()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoff() > 1) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
							+ "					.getKickoff() > 1");
		}

		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.isEmpty()) {
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getPlayerName() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getPlayerName() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturn() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturn() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnYard() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnYard() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnTouchdown() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnTouchdown() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFairCatch() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnFairCatch() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnStartYard() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnStartYard() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnSafety() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnSafety() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumble() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnFumble() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumbleLost() == null) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnFumbleLost() == null");
			}

			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturn() > 1) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturn() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnTouchdown() > 1) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnTouchdown() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFairCatch() > 1) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnFairCatch() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnStartYard() > 100 - params.getPlay().getPlayStartYard()) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnStartYard() > 100 - params.getPlay().getPlayStartYard()");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumble() > 1) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnFumble() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnFumbleLost() > 1) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "					.getKickReturnFumbleLost() > 1");
			}

		}

		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffTouchback() == 1) {
			/**
			 * TOUCHBACK
			 */
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnYard() != 25) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		}

		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffFairCatch() == 1) {
			/**
			 * FAIR CATCH
			 */
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnYard() != 25 - (100 - (params.getPlay().getPlayStartYard()
							+ params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.getKickoff().get(0).getKickoffYard()))) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffTouchback() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ "						.getKickoffTouchback() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
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
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
					.getKickReturnStartYard()
					+ params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
							.get(0).getKickReturnYard() != 25) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_GET_0
								+ "						.getKickReturnStartYard()\n"
								+ "						+ params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)\n"
								+ "								.getKickReturnYard() != 25");
			}
		}

		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOutOfBounds() == 1) {
			/**
			 * OUT OF BOUNDS
			 */
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideAttempt() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_ATTEMPT_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_ONSIDE_SUCCESS_NULL);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnYard() != 35) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ "						.getKickoffReturnYard() != 35");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffTouchback() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ "						.getKickoffTouchback() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		}

		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
				.getKickoffOnsideAttempt() == 1) {
			/**
			 * ONSIDE
			 */

			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0
					&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff()
							.get(0).getKickoffReturnYard() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_YARD_25);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_OUT_OF_BOUNDS_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_RETURN_TOUCHDOWN_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffFairCatch() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_GET_0
								+ GET_KICKOFF_FAIR_CATCH_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_0);
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.getKickoffOnsideSuccess() != 0
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
							.size() != 0) {
				throw new IllegalArgumentException(
						PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_0);
			}
		}

	}

	private void validateReceiver(PbpServiceRequestPojo params) {
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
			if (!defPenalty.isEmpty()) {
				if (defPenalty.get(0).getPenaltyYards() + receiver.getReceivingYard() != params.getPlay()
						.getPlayResult().getPlayResultYard()
						&& Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
					throw new IllegalArgumentException(
							"defPenalty.get(0).getPenaltyYards() + receiver.getReceivingYard() != params.getPlay()\n"
									+ "						.getPlayResult().getPlayResultYard()\n"
									+ "						&& !params.getPlay().getPlayResult().isPlayResultTurnover()");
				}
			} else if (!offPenalty.isEmpty()) {
				if (receiver.getReceivingYard() - offPenalty.get(0).getPenaltyYards() != params.getPlay()
						.getPlayResult().getPlayResultYard()) {
					throw new IllegalArgumentException(
							"receiver.getReceivingYard() - offPenalty.get(0).getPenaltyYards() != params.getPlay()\n"
									+ "						.getPlayResult().getPlayResultYard()");
				}
			} else {
				if (!receiver.getReceivingYard().equals(params.getPlay().getPlayResult().getPlayResultYard())) {
					throw new IllegalArgumentException(
							"receiver.getReceivingYard() != params.getPlay().getPlayResult().getPlayResultYard()");
				}
			}
		}
	}

	private void validatePass(PbpServiceRequestPojo params) {
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
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
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
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}

		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

		if (params.getPlay().getPlayStartDown() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL);
		}
		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL);
		}
		if (params.getPlay().getPlayYardToGain() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL);
		}
		if (params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL);
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
			} else {
				// nothing needed here yet
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
		if (passer.getPassingInterception() == 1) {
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
						DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0);
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
						DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0);
			}
		} else {
			if (defense.stream().filter(d -> d.getInterceptionYard() > 1).collect(Collectors.toList()).size() != 0) {
				throw new IllegalArgumentException(
						DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_YARD_0_COLLECT_COLLECTORS_TO_LIST_SIZE_0);
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
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
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
			if (Objects.isNull(receiver) || receiver.getReceivingReception() == 0) {
				throw new IllegalArgumentException(
						"Objects.nonNull(receiver) && receiver.getReceivingReception() == 0");
			}
		} else {
			if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
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
				throw new IllegalArgumentException(OBJECTS_NON_NULL_RECEIVER_RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
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
				throw new IllegalArgumentException(PASSER_GET_PASSING_COMPLETION_0);
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
			if (passer.getPassingAirLessNeeded() != null) {
				throw new IllegalArgumentException("passer.getPassingAirLessNeeded() != null");
			}
		} else {
			if (passer.getPassingInterception() == 0 && passer.getPassingYardAfterCatch() == null) {
				throw new IllegalArgumentException("passer.getPassingYardAfterCatch() == null");
			}
			if (passer.getPassingAirLessNeeded() == null) {
				throw new IllegalArgumentException("passer.getPassingAirLessNeeded() == null");
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
				throw new IllegalArgumentException(PASSER_GET_PASSING_COMPLETION_0);
			}

		}

		if (passer.getPassingYardAfterCatch() != null) {

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
			} else {
				// nothing needed here yet
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
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				&& passer.getPassingFirstDown() == 0
				&& (defPenalty.isEmpty() || defPenalty.get(0).getPenaltyFirstDown() != 1)) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ "				.getPassingFirstDown() == 0");
		}
		if (Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())
				&& passer.getPassingFirstDown() == 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == false && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ "				.getPassingFirstDown() == 1");
		}
		if (passer.getPassingTouchdown() != 0) {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
				throw new IllegalArgumentException(
						"!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			} else {
				// Nothing needed currently
			}
		} else {
			// Nothing needed currently
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

	private void validatePat(PbpServiceRequestPojo params) {

		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
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
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0);
		}

		PbpPlayerStatKickingPojo kicking = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking().get(0);
		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
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
			throw new IllegalArgumentException(KICKING_GET_FIELD_GOAL_1);
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

		if (kicking.getExtraPointBlock() == null) {
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

		if (kicking.getExtraPointMiss() == null) {
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

		if (kicking.getKickMissReason() != null) {
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

		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

	}

	private void validateFieldGoal(PbpServiceRequestPojo params) {

		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
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
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0);
		}

		PbpPlayerStatKickingPojo kicking = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking().get(0);
		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
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
			throw new IllegalArgumentException(KICKING_GET_EXTRA_POINT_1);
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

		if (kicking.getFieldGoalBlock() == null) {
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

		if (kicking.getFieldGoalMiss() == null) {
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

		if (kicking.getKickMissReason() != null) {
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

		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

	}

	private void validateRush(PbpServiceRequestPojo params) {

		List<PlayerStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();
		List<PlayerStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();

		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
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
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException("");
		}

		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException(PARAMS_GET_DRIVE_GET_KICKOFF);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNTING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_PUNT_COVERAGE_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_PUNT_RETURN_SIZE_0);
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()\n"
							+ "					.getRushingStat().size() != 1");
		}

		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();

		for (PbpPlayerStatDefenseProductionPojo def : defense) {
			validateDef(params, def);
		}

		PbpPlayerStatRushingPojo run = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
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
		if (run.getRushingLineYard() == null) {
			throw new IllegalArgumentException("run.getRushingLineYard() == null");
		}
		if (run.getRushingSuccess() == null) {
			throw new IllegalArgumentException("run.getRushingSuccess() == null");
		}
		if (run.getRushingKneel() == 1) {
			if (run.getRushingYard() > 0) {
				throw new IllegalArgumentException("run.getRushingYard() > 0");
			}
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
		}
		if (run.getRushingAttempt() == null) {
			throw new IllegalArgumentException("run.getRushingAttempt() == null");
		}
		if (run.getRushingOpenField() == null) {
			throw new IllegalArgumentException("run.getRushingOpenField() == null");
		}
		if (run.getRushingYard() > 10) {
			if (Boolean.FALSE.equals(run.getRushingOpenField())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(run.getRushingOpenField())");
			}
		} else {
			if (Boolean.TRUE.equals(run.getRushingOpenField())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(run.getRushingOpenField())");
			}
		}

		if (Boolean.TRUE.equals(run.getRushingOpenField())) {
			if (run.getRushingOpenFieldYard() == null) {
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

					loggingUtils.logInfo(openFieldRushYardLogStr);
					loggingUtils.logInfo(playResultYardLogStr);
					loggingUtils.logInfo(rushResultYardLogStr);

					throw new IllegalArgumentException(
							"run.getRushingOpenFieldYard() != params.getPlay().getPlayResult().getPlayResultYard() - 10");
				} else {
					// TODO
				}
			}
			if (run.getRushingOpenFieldYard() < 0) {
				throw new IllegalArgumentException("run.getRushingOpenFieldYard() < 0");
			}
		} else {
			if (run.getRushingOpenFieldYard() != null) {
				throw new IllegalArgumentException("run.getRushingOpenFieldYard() != null");
			}
		}

		if (run.getRushingSecondLevel() == null) {
			throw new IllegalArgumentException("run.getRushingSecondLevel() == null");
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

		if (Boolean.TRUE.equals(run.getRushingSecondLevel())) {
			if (run.getRushingSecondLevelYard() == null) {
				throw new IllegalArgumentException("run.getRushingSecondLevelYard() == null");
			}
			if (run.getRushingSecondLevelYard() != Math.min(run.getRushingYard() - 5, 5)) {
				throw new IllegalArgumentException(
						"run.getRushingSecondLevelYard() != Math.min(run.getRushingYard() - 5, 5)");
			}
			if (run.getRushingSecondLevelYard() != Math.min(params.getPlay().getPlayResult().getPlayResultYard() - 5,
					5)) {
				if (Boolean.TRUE.equals(offPenalty.isEmpty())) {
				throw new IllegalArgumentException(
						"run.getRushingSecondLevelYard() != Math.min(params.getPlay().getPlayResult().getPlayResultYard() - 5, 5)");
			
				} else {
					// TODO
				}
			}
			if (run.getRushingSecondLevelYard() < 0) {
				throw new IllegalArgumentException("run.getRushingSecondLevelYard() < 0");
			}
			if (run.getRushingSecondLevelYard() > 5) {
				throw new IllegalArgumentException("run.getRushingSecondLevelYard() > 5");
			}
		} else {
			if (run.getRushingSecondLevelYard() != null) {
				throw new IllegalArgumentException("run.getRushingSecondLevelYard() != null");
			}
		}

		if (run.getRushingStuff() == null) {
			throw new IllegalArgumentException("run.getRushingStuff() == null");
		}
		if (run.getRushingPower() == null) {
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
			if (run.getRushingPowerSuccess() == null) {
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
		} else {
			if (run.getRushingPowerSuccess() != null) {
				throw new IllegalArgumentException("run.getRushingPowerSuccess() != null");
			}
		}

		if (run.getRushingTwoPointConversion() == null) {
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
		if (run.getRushingAttempt() != 1) {
			throw new IllegalArgumentException("run.getRushingAttempt() != 1");
		}
		if (run.getRushingYard() == null) {
			throw new IllegalArgumentException("run.getRushingYard() == null");
		}
		if (run.getRushingYard() > 0) {
			if (Boolean.TRUE.equals(run.getRushingStuff())) {
				throw new IllegalArgumentException("Boolean.TRUE.equals(run.getRushingStuff())");
			}
		} else {
			if (Boolean.FALSE.equals(run.getRushingStuff())) {
				throw new IllegalArgumentException("Boolean.FALSE.equals(run.getRushingStuff())");
			}
		}
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

	private void validatePunt(PbpServiceRequestPojo params) {
		PbpPlayerStatPuntingPojo punter = params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
				.getPunting().get(0);
		PbpPlayerStatPuntReturnPojo puntReturner;
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
				.isEmpty()) {
			puntReturner = null;
		} else {
			puntReturner = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
					.findPuntReturner();
		}

		List<PbpPlayerStatPuntReturnPojo> allReturnStats = params.getPlay().getPlayerStat()
				.get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn();
		List<PbpPlayerStatDefenseProductionPojo> coverage = params.getPlay().getPlayerStat()
				.get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage();

		for (PbpPlayerStatDefenseProductionPojo cov : coverage) {
			validateDef(params, cov);
		}
		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException("Drive.getKickoff()");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()\n"
							+ SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()\n"
							+ SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()\n"
							+ SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()\n"
							+ SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()\n"
							+ SIZE_0);
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICK_COVERAGE_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
				.size() != 0) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02);
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()\n"
							+ "					.getRushingStat().size() != 0");
		}

		if (params.getPlay().getPlayStartDown() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL);
		}
		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL);
		}
		if (params.getPlay().getPlayYardToGain() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL);
		}
		if (params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL);
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() != null");
		}
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() != Boolean.FALSE");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 1");
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
			throw new IllegalArgumentException("punter.getPuntTouchback() > 1");
		}

		if (punter.getPuntReturnTouchdown() > 1) {
			throw new IllegalArgumentException("punter.getPuntReturnTouchdown() > 1");
		}
		if (punter.getPuntFairCatch() > 1) {
			throw new IllegalArgumentException("punter.getPuntFairCatch() > 1");
		}
		if (punter.getPunt() > 1) {
			throw new IllegalArgumentException("punter.getPunt() > 1");
		}

		if (allReturnStats.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1) {
			throw new IllegalArgumentException(
					"returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1");

		}
		for (PbpPlayerStatPuntReturnPojo allReturn : allReturnStats) {
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
				String puntLandYardLogStr = String.format("Punt Land Yard: %s", punter.getPuntLandYard());
				String puntReturnYardLogStr = String.format("Punt Return Yard: %s", punter.getPuntReturnYard());
				loggingUtils.logInfo(puntLandYardLogStr);
				loggingUtils.logInfo(puntReturnYardLogStr);
				throw new IllegalArgumentException(
						PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0);
			}
			if (punter.getPuntBlocked() == 0
					&& puntReturner.getPuntReturnStartYard() + puntReturner.getPuntReturnYard() != 100) {
				String puntLandYardLogStr = String.format("Return Start Yard: %s",
						puntReturner.getPuntReturnStartYard());
				String puntReturnYardLogStr = String.format("Return Yard: %s", puntReturner.getPuntReturnYard());
				loggingUtils.logInfo(puntLandYardLogStr);
				loggingUtils.logInfo(puntReturnYardLogStr);
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
						PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0);
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
				throw new IllegalArgumentException(PUNTER_GET_PUNT_RETURN_TOUCHDOWN_0);
			}
			if (punter.getPuntFairCatch() != 0) {
				throw new IllegalArgumentException("punter.getPuntFairCatch() != 0");
			}
			if (coverage.size() != 0) {
				throw new IllegalArgumentException(COVERAGE_SIZE_0);
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
				throw new IllegalArgumentException(PUNTER_GET_PUNT_RETURN_TOUCHDOWN_0);
			}
			if (coverage.size() != 0) {
				throw new IllegalArgumentException(COVERAGE_SIZE_0);
			}
			if (allReturnStats.size() != 1) {
				throw new IllegalArgumentException("returner.size() != 1");
			}
			if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnFairCatch() != 1) {
				throw new IllegalArgumentException("returner.get(0).getPuntReturnFairCatch() != 1");
			}
			if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnStartYard() != 100
					- (params.getPlay().getPlayStartYard() + punter.getPuntYard())) {

				String returnYardLogStr = String.format("Return Start Yard: %s",
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.getPuntReturn().get(0).getPuntReturnStartYard());
				String startYardLogStr = String.format("Start Yard: %s", params.getPlay().getPlayStartYard());
				String puntYardLogStr = String.format("Punt Yards: %s", params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0).getPuntYard());
				String driveTextLogStr = String.format("Drive Text: %s", params.getPlay().getDriveText());
				String abbrevLogStr = String.format("Abbrev: %s", params.getTeamAbbrevDict());
				loggingUtils.logInfo(returnYardLogStr);
				loggingUtils.logInfo(startYardLogStr);
				loggingUtils.logInfo(puntYardLogStr);
				loggingUtils.logInfo(driveTextLogStr);
				loggingUtils.logInfo(abbrevLogStr);

				throw new IllegalArgumentException("returner.get(0).getPuntReturnStartYard() != 100\n"
						+ "					- (params.getPlay().getPlayStartYard() + punter.getPuntYard())");
			}
			if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnFumble() != 0) {
				throw new IllegalArgumentException("returner.get(0).getPuntReturnFumble() != 0");
			}
			if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnFumbleLost() != 0) {
				throw new IllegalArgumentException("returner.get(0).getPuntReturnFumbleLost() != 0");
			}
			if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnBlock() != 0) {
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
				throw new IllegalArgumentException(COVERAGE_SIZE_0);
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

	private void validateDef(PbpServiceRequestPojo params, PbpPlayerStatDefenseProductionPojo def) {

		List<PbpPlayerStatPassingPojo> pass = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getOffense().getPassingStat();
		List<PbpPlayerStatRushingPojo> rush = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getOffense().getRushingStat();
		List<PbpPlayerStatReceivingPojo> receive = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getOffense().getReceivingStat();
		List<PbpPlayerStatPuntReturnPojo> puntRet = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getSpecialTeam().getPuntReturn();
		List<PbpPlayerStatKickReturnPojo> kickRet = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getSpecialTeam().getKickReturn();
		List<PbpPlayerStatKickingPojo> kick = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
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
			} else {
				// nothing needed here yet
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
			if (Boolean.FALSE.equals(params.getDrive().getKickoff()) && def.getFumbleYard() == 0) {
				throw new IllegalArgumentException("def.getFumbleYard() == 0");
			}
		}

		if (def.getFumbleRecovered() == 1) {
			if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
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
				throw new IllegalArgumentException(DEF_GET_TACKLE_FOR_LOSS_0);
			}
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_TOTAL_0);
			}
		}

		if (def.getSafety() == 1) {
			if (def.getTackleForLoss() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_FOR_LOSS_0);
			}
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != -2) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() != -2");
			}
		}

		if (def.getTackleForLoss() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getTackleYard() > 0) {
				throw new IllegalArgumentException("def.getTackleYard() > 0");
			}
		}

		if (def.getTackleYard() < 0) {
			if (def.getTackleForLoss() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_FOR_LOSS_0);
			} else {
				// nothing needed right now
			}
		}

		if (def.getTackleAssist() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getFumbleForced() != 1 && def.getTackleSolo() != 0) {
				throw new IllegalArgumentException("def.getTackleSolo() != 0");
			}
		}

		if (def.getTackleSolo() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getFumbleForced() != 1 && def.getTackleAssist() != 0) {
				throw new IllegalArgumentException("def.getTackleAssist() != 0");
			}
		}

		if (def.getSack() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getTackleForLoss() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_FOR_LOSS_0);
			}
			if (pass.get(0).getPassingSack() == 0) {
				throw new IllegalArgumentException("pass.getPassingSack() == 0");
			}
		}

		if (def.getFumbleForced() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new IllegalArgumentException(DEF_GET_TACKLE_TOTAL_0);
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

		List<PlayerStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();
		List<PlayerStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();

		/**
		 * Pull test cases
		 */

		if (params.getPlayRawText().equals(
				"Brin,Davis pass complete to Stokes,Keylon for 11 yards to the UCD01 fumbled by Stokes,Keylon at UCD08 forced by Venable,Chris, Touchback")) {
			return;
		}
		// TODO add safety checks
		PbpPlayerStatRushingPojo rush;
		PbpPlayerStatPassingPojo pass;
		PbpPlayerStatReceivingPojo receiver;
		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
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
			PbpPlayerStatDefenseProductionPojo turnoverDef = params.getPlay().getPlayerStat()
					.get(params.getDefenseTeam()).getDefense().findDefenseWithTurnover();
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
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())
				&& params.getPlay().getPlayResult().getPlayResultPossessionTeamId()
						.equals(params.getPlay().getPlayStartPossessionTeamId())) {
			throw new IllegalArgumentException(
					"Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover()) && params.getPlay().getPlayResult().getPlayResultPossessionTeamId().equals(params.getPlay().getPlayStartPossessionTeamId())");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown()
				&& params.getPlay().getPlayResult().isPlayResultTurnover()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayResult().isPlayResultTurnover()");
		}
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
			if (defense.stream().filter(d -> d.getTackleTotal() > 0 && d.getFumbleForced() == 0)
					.collect(Collectors.toList()).size() > 0) {
				if (!(params.getPlay().getPlayStartDown() == PlayDownEnum.FOURTH && yardToGain > playResultYard)) {
					throw new IllegalArgumentException(
							"params.getPlay().getPlayResult().isPlayResultTurnover() && defense.stream().filter(d -> d.getTackleTotal() > 0).collect(Collectors.toList()).size() > 0");
				} else {
					// nothing needed right now
				}
			}
		} else {
			if (params.getPlay().getPlayType() == PlayTypeEnum.OFFENSE
					&& params.getPlay().getPlayCallType() != PlayCallTypeEnum.FG
					&& params.getPlay().getPlayStartDown() == PlayDownEnum.FOURTH && yardToGain > playResultYard) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayStartDown() == PlayDownEnum.FOURTH && yardToGain > playResultYard");
			}
		}

		if (params.getPlay().getPlayResult().getPlayResultPoints() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() == null");
		}
		if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF && params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL);
		}

		if (Boolean.FALSE.equals(params.getDrive().getKickoff()) && params.getPlay().getPlayType() != PlayTypeEnum.PUNT
				&& params.getPlay().getPlayType() != PlayTypeEnum.PAT) {
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

					String playStartYardlineLogStr = String.format("Play Start Yardline: %s", playStartYard);
					String playResultYardLogStr = String.format(PLAY_RESULT_YARD_S, playResultYard);
					String passYardThrownLogStr = String.format("Pass Yard Thrown: %s", passYardThrown);
					String offPenaltyYardLogStr = String.format("Offense Penalty Yard: %s", offensePenalty);
					String defPenaltyYardLogStr = String.format("Defense Penalty Yard: %s", defensePenalty);
					String turnoverYardLogStr = String.format("Turnover Return Yard: %s", turnoverReturnYard);
					String playResultLogStr = String.format("Play Result Yardline: %s", playResultYardLine);
					loggingUtils.logInfo(playStartYardlineLogStr);
					loggingUtils.logInfo(playResultYardLogStr);
					loggingUtils.logInfo(passYardThrownLogStr);
					loggingUtils.logInfo(offPenaltyYardLogStr);
					loggingUtils.logInfo(defPenaltyYardLogStr);
					loggingUtils.logInfo(turnoverYardLogStr);
					loggingUtils.logInfo(playResultLogStr);
					throw new IllegalArgumentException(
							"playStartYard + playResultYard + passYardThrown != playResultYardLine");
				} else {
					// Nothing needed currently
				}
			} else {
				// Nothing needed currently
			}

			if (yardToGain < playResultYard && !firstDown && !turnover && playPoints != 6 && playPoints != 2) {
				String playYardToGainLogStr = String.format("Play Yard to Gain: %s", yardToGain);
				String playResultYardLogStr = String.format(PLAY_RESULT_YARD_S, playResultYard);
				String playResultFirstDownLogStr = String.format("Play First Down: %s", firstDown);
				String playResultTurnoverLogStr = String.format("Play Turnover: %s", turnover);
				String playResultPointsLogStr = String.format("Play Points: %s", playPoints);

				loggingUtils.logInfo(playYardToGainLogStr);
				loggingUtils.logInfo(playResultYardLogStr);
				loggingUtils.logInfo(playResultFirstDownLogStr);
				loggingUtils.logInfo(playResultTurnoverLogStr);
				loggingUtils.logInfo(playResultPointsLogStr);

				throw new IllegalArgumentException(
						"yardToGain <= playResultYard && !firstDown && !turnover && playPoints != 6");
			}
			if (offPenalty.isEmpty() && yardToGain > playResultYard && firstDown && defPenalty.stream()
					.filter(dp -> dp.getPenaltyFirstDown() == 1).collect(Collectors.toList()).isEmpty()) {
				String playYardToGainLogStr = String.format("Play Yard to Gain: %s", yardToGain);
				String playResultYardLogStr = String.format(PLAY_RESULT_YARD_S, playResultYard);
				String playResultFirstDownLogStr = String.format("Play First Down: %s", firstDown);
				String playResultTurnoverLogStr = String.format("Play Turnover: %s", turnover);
				loggingUtils.logInfo(playYardToGainLogStr);
				loggingUtils.logInfo(playResultYardLogStr);
				loggingUtils.logInfo(playResultFirstDownLogStr);
				loggingUtils.logInfo(playResultTurnoverLogStr);
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
					throw new IllegalArgumentException(RUSH_GET_RUSHING_SAFETY_0);
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
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_SAFETY_0);
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TWO_POINT_CONVERSION_0);
				}
			} else if (playPoints == 0) {
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TOUCHDOWN_0);
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_SAFETY_0);
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TWO_POINT_CONVERSION_0);
				}
			} else if (playPoints == -2) {
				if (rush.getRushingSafety() != 1) {
					throw new IllegalArgumentException("rush.getRushingSafety() != 1");
				}
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TOUCHDOWN_0);
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TWO_POINT_CONVERSION_0);
				}
			} else if (playPoints == 2) {
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TOUCHDOWN_0);
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_SAFETY_0);
				}
				if (rush.getRushingTwoPointConversion() == 0) {
					throw new IllegalArgumentException("rush.getRushingTwoPointConversion() == 0");
				}
			} else {
				if (rush.getRushingTouchdown() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TOUCHDOWN_0);
				}
				if (defense.stream().filter(d -> d.getFumbleTouchdown() == 1).collect(Collectors.toList())
						.size() != 1) {
					throw new IllegalArgumentException(
							"defense.stream().filter(d -> d.getFumbleTouchdown() == 1).collect(Collectors.toList()).size() != 1");
				}
				if (rush.getRushingSafety() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_SAFETY_0);
				}
				if (rush.getRushingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RUSH_GET_RUSHING_TWO_POINT_CONVERSION_0);
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
					throw new IllegalArgumentException(PASS_GET_PASSING_SAFETY_0);
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
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_SAFETY_0);
				}
				if (receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TWO_POINT_CONVERSION_0);
				}
			} else if (playPoints == 2) {
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TOUCHDOWN_0);
				}
				if (pass.getPassingTwoPointConversion() != 1) {
					throw new IllegalArgumentException("pass.getPassingTwoPointConversion() != 1");
				}
				if (receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException(RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (receiver.getReceivingTwoPointConversion() != 1) {
					throw new IllegalArgumentException("receiver.getReceivingTouchdown() != 1");
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_SAFETY_0);
				}
			} else if (playPoints == 0) {
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TOUCHDOWN_0);
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException(OBJECTS_NON_NULL_RECEIVER_RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_SAFETY_0);
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TWO_POINT_CONVERSION_0);
				}
			} else if (playPoints == -2) {
				if (pass.getPassingSafety() != 1) {
					throw new IllegalArgumentException("pass.getPassingSafety() != 1");
				}
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TOUCHDOWN_0);
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException(OBJECTS_NON_NULL_RECEIVER_RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList())
						.isEmpty()) {
					throw new IllegalArgumentException(
							DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
				}
				if (receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TWO_POINT_CONVERSION_0);
				}
			} else if (playPoints == -6) {
				if (pass.getPassingTouchdown() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TOUCHDOWN_0);
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0) {
					throw new IllegalArgumentException(RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (defense.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveDefenseScore)
						.collect(Collectors.toList()).size() != 1) {
					throw new IllegalArgumentException(
							"defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).size() == 1");
				}
				if (pass.getPassingSafety() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_SAFETY_0);
				}
				if (Objects.nonNull(receiver) && receiver.getReceivingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
				}
				if (pass.getPassingTwoPointConversion() != 0) {
					throw new IllegalArgumentException(PASS_GET_PASSING_TWO_POINT_CONVERSION_0);
				}
			} else {
				throw new IllegalArgumentException("playPoints");
			}
			/**
			 * PUNT Tests
			 */
		} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
			PbpPlayerStatPuntingPojo punter = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
					.getSpecialTeam().getPunting().get(0);
			List<PbpPlayerStatPuntReturnPojo> returner = params.getPlay().getPlayerStat()
					.get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn();
			if (punter.getPuntReturnTouchdown() == null) {
				throw new IllegalArgumentException("punter.getPuntReturnTouchdown() == null");
			}
			if (returner.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1) {
				throw new IllegalArgumentException(
						"puntReturn.stream().filter(ret -> ret.getPuntReturn() == 1).collect(Collectors.toList()).size() > 1");
			}
			if (punter.getPuntReturnTouchdown() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntReturnTouchdown()  > 1");
			}
			if (punter.getPuntReturnTouchdown() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntReturnTouchdown()  > 1");
			}
			for (PbpPlayerStatPuntReturnPojo puntReturn : returner) {
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
								PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0);
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
							PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0);
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
					throw new IllegalArgumentException(PUNTER_GET_PUNT_RETURN_TOUCHDOWN_0);
				} else {
					// nothing needed yet
				}
			}
		}
		if (params.getPlay().getPlayType() != PlayTypeEnum.PAT
				&& params.getPlay().getPlayResult().getPlayResultYardLine() > 100) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() > 100");
		}
		if (params.getPlay().getPlayType() != PlayTypeEnum.PAT
				&& params.getPlay().getPlayResult().getPlayResultYardLine() < 0) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() < 0");
		}
		/**
		 * Score tests
		 */
		if (playPoints == 6) {
			if (!defense.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveDefenseScore)
					.collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()");
			}
			if (params.getPlay().getPlayResult().getPlayResultYardLine() != 100) {
				throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() != 100");
			}
		} else if (playPoints == 0) {
			if (!defense.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveDefenseScore)
					.collect(Collectors.toList()).isEmpty()) {
				throw new IllegalArgumentException(
						"!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()");
			}
			if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT
					&& (params.getPlay().getPlayResult().getPlayResultYardLine() == 0
							|| params.getPlay().getPlayResult().getPlayResultYardLine() == 100)) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayResult().getPlayResultYardLine() == 0 || params.getPlay().getPlayResult().getPlayResultYardLine() == 100");
			}
		} else if (playPoints == -6) {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				if (defense.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveDefenseScore)
						.collect(Collectors.toList()).size() != 1) {
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
				if (defense.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveDefenseScore)
						.collect(Collectors.toList()).size() != 1) {
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
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
		} else if (playPoints == 2) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
			if (!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN)) {
				throw new IllegalArgumentException("!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS\n"
						+ "					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN)");
			}
		} else if (playPoints == 1) {
			if (!defense.isEmpty()) {
				throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
			}
			if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT) {
				throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT");
			}
		} else {
			throw new IllegalArgumentException("CATCH THIS");
		}

	}

}
