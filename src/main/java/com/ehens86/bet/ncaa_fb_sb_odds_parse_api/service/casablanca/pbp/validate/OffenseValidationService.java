package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;

public final class OffenseValidationService {
	private static final String OBJECTS_IS_NULL_RECEIVER = "Objects.isNull(receiver)";
	private static final String PLAY_RESULT_YARD_S = "Play Result Yard: %s";
	private static final String PASS_GET_PASSING_TOUCHDOWN_0 = "pass.getPassingTouchdown() != 0";
	private static final String PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL = "params.getPlay().getPlayCallType() == null";
	private static final String OBJECTS_NON_NULL_RECEIVER_RECEIVER_GET_RECEIVING_TOUCHDOWN_0 = "Objects.nonNull(receiver) && receiver.getReceivingTouchdown() != 0";
	private static final String DEFENSE_IS_EMPTY = "!defense.isEmpty()";
	private static final String PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0 = "punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0";
	private static final String PUNTER_GET_PUNT_RETURN_TOUCHDOWN_0 = "punter.getPuntReturnTouchdown() != 0";
	private static final String PASS_GET_PASSING_SAFETY_0 = "pass.getPassingSafety() != 0";
	private static final String RUSH_GET_RUSHING_TWO_POINT_CONVERSION_0 = "rush.getRushingTwoPointConversion() != 0";
	private static final String RUSH_GET_RUSHING_SAFETY_0 = "rush.getRushingSafety() != 0";
	private static final String RUSH_GET_RUSHING_TOUCHDOWN_0 = "rush.getRushingTouchdown() != 0";
	private static final String PASS_GET_PASSING_TWO_POINT_CONVERSION_0 = "pass.getPassingTwoPointConversion() != 0";
	private static final String DEFENSE_STREAM_FILTER_D_D_GET_FUMBLE_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY = "!defense.stream().filter(d -> d.getFumbleTouchdown() != 0).collect(Collectors.toList()).isEmpty()";
	private static final String RECEIVER_GET_RECEIVING_TOUCHDOWN_0 = "receiver.getReceivingTouchdown() != 0";
	private static final String DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY = "!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()";

	// Private constructor to prevent instantiation
	private OffenseValidationService() {
		throw new UnsupportedOperationException();
	}

	public static void validateOffense(PbpServiceRequestPojo params) {

		List<PlayerStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();

		/**
		 * Pull test cases
		 */

		if (params.getPlayRawText().equals(
				"Brin,Davis pass complete to Stokes,Keylon for 11 yards to the UCD01 fumbled by Stokes,Keylon at UCD08 forced by Venable,Chris, Touchback")) {
			throw new IllegalArgumentException(params.getPlayRawText());
		}

		List<PbpPlayerStatDefenseProductionPojo> defense = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getDefense().getDefenseProduction();
		Integer playPoints = params.getPlay().getPlayResult().getPlayResultPoints();

		Integer playResultYard = params.getPlay().getPlayResult().getPlayResultYard();

		validateOffenseHelperResultYard(params, defPenalty, playResultYard);

		validateOffenseHelperDefense(params, defense, playResultYard);

		/**
		 * Null Tests
		 */
		validateOffenseHelperNulls(params);

		/**
		 * Play result either -6, 0, -2, 3, or 6
		 */
		validateOffenseHelperResultPoints(params);

		/**
		 * RUN Tests
		 */
		if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
			validateOffenseHelperRush(params, defense, playPoints, defPenalty);
			/**
			 * PASS Tests
			 */
		} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
			validateOffenseHelperPass(params, defense, playPoints, defPenalty);
		} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
			validateOffenseHelperPunt(params);
		}

		if (params.getPlay().getPlayType() != PlayTypeEnum.PAT
				&& params.getPlay().getPlayResult().getPlayResultYardLine() > 100) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() > 100");
		}
		if (params.getPlay().getPlayType() != PlayTypeEnum.PAT
				&& params.getPlay().getPlayResult().getPlayResultYardLine() < 0) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() < 0");
		}

		validateOffenseHelperScore(params, defense, playPoints);
	}

	private static void validateOffenseHelperNulls(PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getPlayResult().isPlayResultFirstDown())) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown() == null");
		}
		if (Objects.isNull(params.getPlay().getPlayResult().isPlayResultTurnover())) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultTurnover() == null");
		}
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())
				&& params.getPlay().getPlayResult().getPlayResultPossessionTeamId()
						.equals(params.getPlay().getPlayStartPossessionTeamId())
						&& !PlayTypeEnum.PUNT.equals(params.getPlay().getPlayType())) {
			throw new IllegalArgumentException(
					"Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover()) && params.getPlay().getPlayResult().getPlayResultPossessionTeamId().equals(params.getPlay().getPlayStartPossessionTeamId())");
		}
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())
				&& !params.getPlay().getPlayResult().getPlayResultPossessionTeamId()
						.equals(params.getPlay().getPlayStartPossessionTeamId())
						&& PlayTypeEnum.PUNT.equals(params.getPlay().getPlayType())) {
			throw new IllegalArgumentException(
					"Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover()) && params.getPlay().getPlayResult().getPlayResultPossessionTeamId().equals(params.getPlay().getPlayStartPossessionTeamId())");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown()
				&& params.getPlay().getPlayResult().isPlayResultTurnover()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayResult().isPlayResultTurnover()");
		}
		if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultPoints())) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultPoints() == null");
		}
		if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
				&& Objects.isNull(params.getPlay().getPlayCallType())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL);
		}
	}

	private static void validateOffenseHelperDefense(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense, Integer playResultYard) {
		Integer yardToGain = params.getPlay().getPlayYardToGain();
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
			if (!defense.stream().filter(d -> d.getTackleTotal() > 0 && d.getFumbleForced() == 0)
					.collect(Collectors.toList()).isEmpty()) {
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
	}

	private static void validateOffenseHelperResultYard(PbpServiceRequestPojo params,
			List<PlayerStatPenaltyPojo> defPenalty, Integer playResultYard) {
		Integer offensePenalty;
		Integer defensePenalty;
		Integer passYardThrown;
		List<PlayerStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
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

		validateOffenseHelperResult(params, playResultYard, passYardThrown, offensePenalty, defensePenalty);

	}

	private static void validateOffenseHelperResult(PbpServiceRequestPojo params, Integer playResultYard,
			Integer passYardThrown, Integer offensePenalty, Integer defensePenalty) {
		Integer playStartYard = params.getPlay().getPlayStartYard();
		boolean turnover = params.getPlay().getPlayResult().isPlayResultTurnover();
		Integer playResultYardLine = params.getPlay().getPlayResult().getPlayResultYardLine();

		Integer turnoverReturnYard;
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
		if (Boolean.FALSE.equals(params.getDrive().getKickoff())
				&& !PlayTypeEnum.PUNT.equals(params.getPlay().getPlayType())
				&& !PlayTypeEnum.PAT.equals(params.getPlay().getPlayType())
				&& !PlayCallTypeEnum.FG.equals(params.getPlay().getPlayCallType())) {
			if (playStartYard + playResultYard + passYardThrown - turnoverReturnYard != playResultYardLine && !(params
					.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
							.isEmpty())) {
				validateOffenseHelperResultExceptionCatch(params, playResultYard, passYardThrown, turnoverReturnYard,
						offensePenalty, defensePenalty);
			} else {
				// Nothing needed currently
			}
			validateOffenseHelperResultYardToGain(params, playResultYard);
		}
	}

	private static void validateOffenseHelperResultExceptionCatch(PbpServiceRequestPojo params, Integer playResultYard,
			Integer passYardThrown, Integer turnoverReturnYard, Integer offensePenalty, Integer defensePenalty) {
		Integer playResultYardLine = params.getPlay().getPlayResult().getPlayResultYardLine();
		Integer playStartYard = params.getPlay().getPlayStartYard();
		if (!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN
				&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.getRushingFumble() == 1
				&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.getRushingFumbleLost() == 0)
				&& !params.getPlayRawText().toUpperCase().contains("TOUCHBACK")) {

			String playStartYardlineLogStr = String.format("Play Start Yardline: %s", playStartYard);
			String playResultYardLogStr = String.format(PLAY_RESULT_YARD_S, playResultYard);
			String passYardThrownLogStr = String.format("Pass Yard Thrown: %s", passYardThrown);
			String offPenaltyYardLogStr = String.format("Offense Penalty Yard: %s", offensePenalty);
			String defPenaltyYardLogStr = String.format("Defense Penalty Yard: %s", defensePenalty);
			String turnoverYardLogStr = String.format("Turnover Return Yard: %s", turnoverReturnYard);
			String playResultLogStr = String.format("Play Result Yardline: %s", playResultYardLine);
			LoggingUtils.logInfo(playStartYardlineLogStr);
			LoggingUtils.logInfo(playResultYardLogStr);
			LoggingUtils.logInfo(passYardThrownLogStr);
			LoggingUtils.logInfo(offPenaltyYardLogStr);
			LoggingUtils.logInfo(defPenaltyYardLogStr);
			LoggingUtils.logInfo(turnoverYardLogStr);
			LoggingUtils.logInfo(playResultLogStr);
			throw new IllegalArgumentException("playStartYard + playResultYard + passYardThrown != playResultYardLine");
		} else {
			// Nothing needed currently
		}
	}

	private static void validateOffenseHelperResultYardToGain(PbpServiceRequestPojo params, Integer playResultYard) {
		List<PlayerStatPenaltyPojo> offPenalty = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getPenalty();
		List<PlayerStatPenaltyPojo> defPenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
				.getPenalty();

		boolean firstDown = params.getPlay().getPlayResult().isPlayResultFirstDown();
		boolean turnover = params.getPlay().getPlayResult().isPlayResultTurnover();
		Integer yardToGain = params.getPlay().getPlayYardToGain();
		Integer playPoints = params.getPlay().getPlayResult().getPlayResultPoints();

		if (yardToGain < playResultYard && !firstDown && !turnover && playPoints != 6 && playPoints != 2) {
			String playYardToGainLogStr = String.format("Play Yard to Gain: %s", yardToGain);
			String playResultYardLogStr = String.format(PLAY_RESULT_YARD_S, playResultYard);
			String playResultFirstDownLogStr = String.format("Play First Down: %s", firstDown);
			String playResultTurnoverLogStr = String.format("Play Turnover: %s", turnover);
			String playResultPointsLogStr = String.format("Play Points: %s", playPoints);

			LoggingUtils.logInfo(playYardToGainLogStr);
			LoggingUtils.logInfo(playResultYardLogStr);
			LoggingUtils.logInfo(playResultFirstDownLogStr);
			LoggingUtils.logInfo(playResultTurnoverLogStr);
			LoggingUtils.logInfo(playResultPointsLogStr);

			throw new IllegalArgumentException(
					"yardToGain <= playResultYard && !firstDown && !turnover && playPoints != 6");
		}
		if (offPenalty.isEmpty() && yardToGain > playResultYard && firstDown && defPenalty.stream()
				.filter(dp -> dp.getPenaltyFirstDown() == 1).collect(Collectors.toList()).isEmpty()) {
			String playYardToGainLogStr = String.format("Play Yard to Gain: %s", yardToGain);
			String playResultYardLogStr = String.format(PLAY_RESULT_YARD_S, playResultYard);
			String playResultFirstDownLogStr = String.format("Play First Down: %s", firstDown);
			String playResultTurnoverLogStr = String.format("Play Turnover: %s", turnover);
			LoggingUtils.logInfo(playYardToGainLogStr);
			LoggingUtils.logInfo(playResultYardLogStr);
			LoggingUtils.logInfo(playResultFirstDownLogStr);
			LoggingUtils.logInfo(playResultTurnoverLogStr);
			throw new IllegalArgumentException(
					"params.getPlay().getPlayYardToGain() > params.getPlay().getPlayResult().getPlayResultYard() && params.getPlay().getPlayResult().isPlayResultFirstDown()");
		}
	}

	private static void validateOffenseHelperResultPoints(PbpServiceRequestPojo params) {
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
	}

	private static void validateOffenseHelperRush(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense, Integer playPoints,
			List<PlayerStatPenaltyPojo> defPenalty) {
		PbpPlayerStatRushingPojo rush;
		boolean firstDown = params.getPlay().getPlayResult().isPlayResultFirstDown();
		rush = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0);

		validateOffenseHelperRushNulls(rush);

		validateOffenseHelperRushFirstDown(rush, firstDown, defPenalty);

		if (playPoints == 6) {
			validateOffenseHelperPointsSix(rush, defense);
		} else if (playPoints == 0) {
			validateOffenseHelperPointsZero(rush, defense);
		} else if (playPoints == -2) {
			validateOffenseHelperPointsNegTwo(rush, defense);
		} else if (playPoints == 2) {
			validateOffenseHelperPointsTwo(rush, defense);
		} else {
			validateOffenseHelperPointsOther(rush, defense);
		}
	}

	private static void validateOffenseHelperPointsSix(PbpPlayerStatRushingPojo rush,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
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
	}

	private static void validateOffenseHelperPointsZero(PbpPlayerStatRushingPojo rush,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
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
	}

	private static void validateOffenseHelperPointsNegTwo(PbpPlayerStatRushingPojo rush,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
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
	}

	private static void validateOffenseHelperPointsTwo(PbpPlayerStatRushingPojo rush,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
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
	}

	private static void validateOffenseHelperPointsOther(PbpPlayerStatRushingPojo rush,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (rush.getRushingTouchdown() != 0) {
			throw new IllegalArgumentException(RUSH_GET_RUSHING_TOUCHDOWN_0);
		}
		if (defense.stream().filter(d -> d.getFumbleTouchdown() == 1).collect(Collectors.toList()).size() != 1) {
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

	private static void validateOffenseHelperRushNulls(PbpPlayerStatRushingPojo rush) {
		if (Objects.isNull(rush.getRushingFirstDown())) {
			throw new IllegalArgumentException("rush.getRushingFirstDown() == null");
		}
		if (Objects.isNull(rush.getRushingSafety())) {
			throw new IllegalArgumentException("rush.getRushingSafety() == null");
		}
		if (Objects.isNull(rush.getRushingTouchdown())) {
			throw new IllegalArgumentException("rush.getRushingTouchdown() == null");
		}
		if (Objects.isNull(rush.getRushingFumble())) {
			throw new IllegalArgumentException("rush.getRushingFumble() == null");
		}
		if (Objects.isNull(rush.getRushingFumbleLost())) {
			throw new IllegalArgumentException("rush.getRushingFumbleLost() == null");
		}
		if (rush.getRushingFumbleLost() == 1 && rush.getRushingFumble() == 0) {
			throw new IllegalArgumentException("rush.getRushingFumbleLost() == 1 && rush.getRushingFumble() == 0");
		}
	}

	private static void validateOffenseHelperRushFirstDown(PbpPlayerStatRushingPojo rush, boolean firstDown,
			List<PlayerStatPenaltyPojo> defPenalty) {
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
	}

	private static void validateOffenseHelperPass(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense, Integer playPoints,
			List<PlayerStatPenaltyPojo> defPenalty) {
		PbpPlayerStatPassingPojo pass;
		PbpPlayerStatReceivingPojo receiver;
		boolean firstDown = params.getPlay().getPlayResult().isPlayResultFirstDown();
		pass = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0);
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
				.isEmpty()) {
			receiver = null;
		} else {
			receiver = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.get(0);
		}

		validateOffenseHelperPassNulls(pass, defense);
		if (Objects.nonNull(receiver)) {
			validateOffenseHelperReceiver(receiver);
		}

		validateOffenseHelperPassFirstDown(pass, receiver, firstDown, defPenalty);

		if (playPoints == 6) {
			validateOffenseHelperPassPointsTouchdown(pass, receiver, defense);
		} else if (playPoints == 2) {
			validateOffenseHelperPassPointsTwoPointConversion(pass, receiver, defense);
		} else if (playPoints == 0) {
			validateOffenseHelperPassPointsNone(pass, receiver, defense);
		} else if (playPoints == -2) {
			validateOffenseHelperPassPointsDefSafety(pass, receiver, defense);
		} else if (playPoints == -6) {
			validateOffenseHelperPassPointsDefTouchdown(pass, receiver, defense);
		} else {
			throw new IllegalArgumentException("playPoints");
		}
	}

	private static void validateOffenseHelperPassNulls(PbpPlayerStatPassingPojo pass,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(pass.getPassingFirstDown())) {
			throw new IllegalArgumentException("pass.getPassingFirstDown() == null");
		}
		if (Objects.isNull(pass.getPassingSafety())) {
			throw new IllegalArgumentException("pass.getPassingSafety() == null");
		}
		if (Objects.isNull(pass.getPassingTouchdown())) {
			throw new IllegalArgumentException("pass.getPassingTouchdown() == null");
		}
		if (Objects.isNull(pass.getPassingFumble())) {
			throw new IllegalArgumentException("pass.getPassingFumble() == null");
		}
		if (Objects.isNull(pass.getPassingFumbleLost())) {
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
	}

	private static void validateOffenseHelperReceiver(PbpPlayerStatReceivingPojo receiver) {
		if (Objects.isNull(receiver.getRecievingFirstDown())) {
			throw new IllegalArgumentException("receiver.getRecievingFirstDown() == null");
		}
		if (Objects.isNull(receiver.getReceivingSafety())) {
			throw new IllegalArgumentException("receiver.getReceivingSafety() == null");
		}
		if (Objects.isNull(receiver.getReceivingTouchdown())) {
			throw new IllegalArgumentException("receiver.getReceivingTouchdown() == null");
		}
		if (Objects.isNull(receiver.getReceivingFumble())) {
			throw new IllegalArgumentException("receiver.getReceivingFumble() == null");
		}
		if (Objects.isNull(receiver.getReceivingFumbleLost())) {
			throw new IllegalArgumentException("receiver.getReceivingFumbleLost() == null");
		}
		if (receiver.getReceivingFumbleLost() == 1 && receiver.getReceivingFumble() == 0) {
			throw new IllegalArgumentException(
					"receiver.getReceivingFumbleLost() == 1 && receiver.getReceivingFumble() == 0");
		}
	}

	private static void validateOffenseHelperPassFirstDown(PbpPlayerStatPassingPojo pass,
			PbpPlayerStatReceivingPojo receiver, boolean firstDown, List<PlayerStatPenaltyPojo> defPenalty) {
		if (Boolean.TRUE.equals(firstDown)) {
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
			validateOffenseHelperPassNoFirstDown(pass, receiver);
		}
	}

	private static void validateOffenseHelperPassNoFirstDown(PbpPlayerStatPassingPojo pass,
			PbpPlayerStatReceivingPojo receiver) {
		if (pass.getPassingFirstDown() != 0) {
			throw new IllegalArgumentException("pass.getPassingFirstDown() != 0");
		}
		if (Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0) {
			throw new IllegalArgumentException("Objects.nonNull(receiver) && receiver.getRecievingFirstDown() != 0");
		}

	}

	private static void validateOffenseHelperPassPointsTouchdown(PbpPlayerStatPassingPojo pass,
			PbpPlayerStatReceivingPojo receiver, List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(receiver)) {
			throw new IllegalArgumentException(OBJECTS_IS_NULL_RECEIVER);
		}
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
		if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
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
	}

	private static void validateOffenseHelperPassPointsTwoPointConversion(PbpPlayerStatPassingPojo pass,
			PbpPlayerStatReceivingPojo receiver, List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (Objects.isNull(receiver)) {
			throw new IllegalArgumentException(OBJECTS_IS_NULL_RECEIVER);
		}
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
		if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
		}
		if (pass.getPassingSafety() != 0) {
			throw new IllegalArgumentException(PASS_GET_PASSING_SAFETY_0);
		}
	}

	private static void validateOffenseHelperPassPointsNone(PbpPlayerStatPassingPojo pass,
			PbpPlayerStatReceivingPojo receiver, List<PbpPlayerStatDefenseProductionPojo> defense) {
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
		if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
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
	}

	private static void validateOffenseHelperPassPointsDefSafety(PbpPlayerStatPassingPojo pass,
			PbpPlayerStatReceivingPojo receiver, List<PbpPlayerStatDefenseProductionPojo> defense) {
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
		if (!defense.stream().filter(d -> d.getInterceptionTouchdown() != 0).collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					DEFENSE_STREAM_FILTER_D_D_GET_INTERCEPTION_TOUCHDOWN_0_COLLECT_COLLECTORS_TO_LIST_IS_EMPTY);
		}
		if (Objects.nonNull(receiver) && receiver.getReceivingTwoPointConversion() != 0) {
			throw new IllegalArgumentException(RECEIVER_GET_RECEIVING_TOUCHDOWN_0);
		}
		if (pass.getPassingTwoPointConversion() != 0) {
			throw new IllegalArgumentException(PASS_GET_PASSING_TWO_POINT_CONVERSION_0);
		}
	}

	private static void validateOffenseHelperPassPointsDefTouchdown(PbpPlayerStatPassingPojo pass,
			PbpPlayerStatReceivingPojo receiver, List<PbpPlayerStatDefenseProductionPojo> defense) {
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
	}

	private static void validateOffenseHelperPunt(PbpServiceRequestPojo params) {
		/**
		 * PUNT Tests
		 */
		PbpPlayerStatPuntingPojo punter = params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
				.getPunting().get(0);
		List<PbpPlayerStatPuntReturnPojo> returner = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
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
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ ".getPuntReturnTouchdown()  > 1");
		}
		if (punter.getPuntReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ ".getPuntReturnTouchdown()  > 1");
		}
		for (PbpPlayerStatPuntReturnPojo puntReturn : returner) {
			validateOffenseHelperPuntReturner(punter, puntReturn);
		}
		validateOffenseHelperPuntReturnTouchdown(punter, returner);
		validateOffenseHelperPuntReturnTouchback(punter);
	}

	private static void validateOffenseHelperPuntReturner(PbpPlayerStatPuntingPojo punter,
			PbpPlayerStatPuntReturnPojo puntReturn) {
		if (Objects.isNull(puntReturn.getPuntReturnSafety())) {
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
						+ "&& returner.get(0).getPuntReturnStartYard() + puntReturn.getPuntReturnYard() != 100");
			}
		}
	}

	private static void validateOffenseHelperPuntReturnTouchdown(PbpPlayerStatPuntingPojo punter,
			List<PbpPlayerStatPuntReturnPojo> returner) {
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
	}

	private static void validateOffenseHelperPuntReturnTouchback(PbpPlayerStatPuntingPojo punter) {
		if (punter.getPuntTouchback() == 1 || punter.getPuntFairCatch() == 1) {
			if (punter.getPuntReturnTouchdown() != 0) {
				throw new IllegalArgumentException(PUNTER_GET_PUNT_RETURN_TOUCHDOWN_0);
			} else {
				// nothing needed yet
			}
		}
	}

	private static void validateOffenseHelperScore(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense, Integer playPoints) {
		/**
		 * Score tests
		 */
		if (playPoints == 6) {
			validateOffenseHelperScoreSix(params, defense);
		} else if (playPoints == 0) {
			validateOffenseHelperScoreZero(params, defense);
		} else if (playPoints == -6) {
			validateOffenseHelperScoreNegSix(params, defense);
		} else if (playPoints == -2) {
			validateOffenseHelperScoreNegTwo(params, defense);
		} else if (playPoints == 3) {
			validateOffenseHelperScoreThree(defense);
		} else if (playPoints == 2) {
			validateOffenseHelperScoreTwo(params, defense);
		} else if (playPoints == 1) {
			validateOffenseHelperScoreOne(params, defense);
		} else {
			throw new IllegalArgumentException("CATCH THIS");
		}
	}

	private static void validateOffenseHelperScoreSix(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (!defense.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveDefenseScore)
				.collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					"!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()");
		}
		if (params.getPlay().getPlayResult().getPlayResultYardLine() != 100) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYardLine() != 100");
		}
	}

	private static void validateOffenseHelperScoreZero(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (!defense.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveDefenseScore)
				.collect(Collectors.toList()).isEmpty()) {
			throw new IllegalArgumentException(
					"!defense.stream().filter(d -> d.resolveDefenseScore()).collect(Collectors.toList()).isEmpty()");
		}
		if (params.getPlay().getPlayType() != PlayTypeEnum.PAT
				&& (params.getPlay().getPlayResult().getPlayResultYardLine() == 0
						|| params.getPlay().getPlayResult().getPlayResultYardLine() == 100)) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().getPlayResultYardLine() == 0 || params.getPlay().getPlayResult().getPlayResultYardLine() == 100");
		}
	}

	private static void validateOffenseHelperScoreNegSix(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
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
	}

	private static void validateOffenseHelperScoreNegTwo(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
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
	}

	private static void validateOffenseHelperScoreThree(List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (!defense.isEmpty()) {
			throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
		}
	}

	private static void validateOffenseHelperScoreTwo(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (!defense.isEmpty()) {
			throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
		}
		if (!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS
				|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN)) {
			throw new IllegalArgumentException("!(params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS\n"
					+ "|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN)");
		}
	}

	private static void validateOffenseHelperScoreOne(PbpServiceRequestPojo params,
			List<PbpPlayerStatDefenseProductionPojo> defense) {
		if (!defense.isEmpty()) {
			throw new IllegalArgumentException(DEFENSE_IS_EMPTY);
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT");
		}
	}

}
