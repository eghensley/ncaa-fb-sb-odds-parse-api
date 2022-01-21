package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;

public final class PuntValidationService {
	private static final String PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD_NULL = "params.getPlay().getPlayResult().getPlayResultYard() != null";
	private static final String IS_EMPTY = ".isEmpty())\n";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_DOWN_NULL = "params.getPlay().getPlayStartDown() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_START_YARD_NULL = "params.getPlay().getPlayStartYard() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_YARD_TO_GAIN_NULL = "params.getPlay().getPlayYardToGain() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAY_CALL_TYPE_NULL = "params.getPlay().getPlayCallType() == null";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKOFF_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0 = "params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()";
	private static final String PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0 = "punter.getPuntBlocked() == 0 && punter.getPuntLandYard() - punter.getPuntReturnYard() != 0";
	private static final String PUNTER_GET_PUNT_RETURN_TOUCHDOWN_0 = "punter.getPuntReturnTouchdown() != 0";
	private static final String COVERAGE_SIZE_0 = "!coverage.isEmpty()";
	private static final String PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICK_RETURN_SIZE_02 = "params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().isEmpty()";
	
    // Private constructor to prevent instantiation
    private PuntValidationService() {
        throw new UnsupportedOperationException();
    }
    
	public static void validatePunt(PbpServiceRequestPojo params) {
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
			DefenseValidationService.validateDef(params, cov);
		}
		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			throw new IllegalArgumentException("Drive.getKickoff()");
		}

		validatePuntHelperPlayType(params);
		validatePuntHelperPlay(params);
		for (PbpPlayerStatPuntReturnPojo returner : allReturnStats) {
			validatePuntHelperReturnNulls(returner);
			validatePuntHelperReturnValues(returner);
		}

		/**
		 * CONDITIONAL CHECK
		 */

		validatePuntHelperPunterNulls(punter);
		validatePuntHelperPunterValues(punter, allReturnStats, coverage);
		validatePuntHelperReturnTouchdown(punter, puntReturner);
		validatePuntHelperTouchdown(punter, puntReturner);
		validatePuntHelperTouchback(punter, allReturnStats, coverage);
		validatePuntHelperFairCatch(params, punter, puntReturner, allReturnStats, coverage);
		validatePuntHelperBlock(punter, allReturnStats, coverage);

	}

	private static void validatePuntHelperPlayType(PbpServiceRequestPojo params) {
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()\n"
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()\n"
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()
				.isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()\n"
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()
				.isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getDefense().getDefenseProduction()\n"
							+ IS_EMPTY);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
				.isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()\n"
							+ IS_EMPTY);
		}

		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKicking().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_DEFENSE_TEAM_GET_SPECIAL_TEAM_GET_KICKING_SIZE_0);
		}
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().isEmpty()) {
			throw new IllegalArgumentException(
					PARAMS_GET_PLAY_GET_PLAYER_STAT_GET_PARAMS_GET_POSSESSION_TEAM_GET_OFFENSE_GET_PASSING_STAT_SIZE_0);
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 1");
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
		if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()\n"
							+ ".getRushingStat().isEmpty()");
		}
	}

	private static void validatePuntHelperPlay(PbpServiceRequestPojo params) {
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
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT");
		}
		if (Objects.nonNull(params.getPlay().getPlayResult().getPlayResultYard())) {
			throw new IllegalArgumentException(PARAMS_GET_PLAY_GET_PLAY_RESULT_GET_PLAY_RESULT_YARD_NULL);
		}
		if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultFirstDown())) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() != Boolean.FALSE");
		}
		if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().size() != 1");
		}
	}

	private static void validatePuntHelperPunterNulls(PbpPlayerStatPuntingPojo punter) {
		/**
		 * NULL CHECK
		 */
		if (StringUtils.isBlank(punter.getPlayerName())) {
			throw new IllegalArgumentException("StringUtils.isBlank(punter.getPlayerName())");
		}
		if (Objects.isNull(punter.getPuntYard())) {
			throw new IllegalArgumentException("punter.getPuntYard() == null");
		}
		if (Objects.isNull(punter.getPuntTouchback())) {
			throw new IllegalArgumentException("punter.getPuntTouchback() == null");
		}
		if (Objects.isNull(punter.getPuntReturnYard())) {
			throw new IllegalArgumentException("punter.getPuntReturnYard() == null");
		}
		if (Objects.isNull(punter.getPuntReturnTouchdown())) {
			throw new IllegalArgumentException("punter.getPuntReturnTouchdown() == null");
		}
		if (Objects.isNull(punter.getPuntFairCatch())) {
			throw new IllegalArgumentException("punter.getPuntFairCatch() == null");
		}
		if (Objects.isNull(punter.getPunt())) {
			throw new IllegalArgumentException("punter.getPunt() == null");
		}
		if (Objects.isNull(punter.getPuntLandYard())) {
			throw new IllegalArgumentException("punter.getPuntLandYard() == null");
		}
	}
	
	private static void validatePuntHelperPunterValues(PbpPlayerStatPuntingPojo punter,
			List<PbpPlayerStatPuntReturnPojo> allReturnStats, List<PbpPlayerStatDefenseProductionPojo> coverage) {
		/**
		 * VALUE CHECK
		 */
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
	}

	private static void validatePuntHelperReturnNulls(PbpPlayerStatPuntReturnPojo puntReturner) {
		if (Objects.isNull(puntReturner.getPlayerName())) {
			throw new IllegalArgumentException("allReturn.getPlayerName() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturn())) {
			throw new IllegalArgumentException("allReturn.getpuntReturn() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturnYard())) {
			throw new IllegalArgumentException("allReturn.getpuntReturnYard() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturnTouchdown())) {
			throw new IllegalArgumentException("allReturn.getpuntReturnTouchdown() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturnFairCatch())) {
			throw new IllegalArgumentException("allReturn.getpuntReturnFairCatch() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturnStartYard())) {
			throw new IllegalArgumentException("allReturn.getpuntReturnStartYard() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturnFumble())) {
			throw new IllegalArgumentException("allReturn.getpuntReturnFumble() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturnFumbleLost())) {
			throw new IllegalArgumentException("allReturn.getpuntReturnFumbleLost() == null");
		}
		if (Objects.isNull(puntReturner.getPuntReturnBlock())) {
			throw new IllegalArgumentException("allReturn.getPuntReturnBlock() == null");
		}
	}

	private static void validatePuntHelperReturnValues(PbpPlayerStatPuntReturnPojo puntReturner) {
		if (puntReturner.getPuntReturn() > 1) {
			throw new IllegalArgumentException("allReturn.getPuntReturn() > 1");
		}
		if (puntReturner.getPuntReturnTouchdown() > 1) {
			throw new IllegalArgumentException("allReturn.getPuntReturnTouchdown() > 1");
		}
		if (puntReturner.getPuntReturnFairCatch() > 1) {
			throw new IllegalArgumentException("allReturn.getPuntReturnFairCatch() > 1");
		}
		if (puntReturner.getPuntReturnFumble() > 1) {
			throw new IllegalArgumentException("allReturn.getPuntReturnFumble() > 1");
		}
		if (puntReturner.getPuntReturnFumbleLost() > 1) {
			throw new IllegalArgumentException("allReturn.getPuntReturnFumbleLost() > 1");
		}

		if (puntReturner.getPuntReturn() == 0) {
			validatePuntHelperReturnValuesReturn(puntReturner);
		}
	}

	private static void validatePuntHelperReturnValuesReturn(PbpPlayerStatPuntReturnPojo puntReturner) {
		if (puntReturner.getPuntReturnTouchdown() != 0) {
			throw new IllegalArgumentException("allReturn.getPuntReturnTouchdown() != 0");
		}
		if (puntReturner.getPuntReturnFairCatch() != 0) {
			throw new IllegalArgumentException("allReturn.getPuntReturnFairCatch() != 0");
		}
		if (puntReturner.getPuntReturnStartYard() != 0) {
			throw new IllegalArgumentException("allReturn.getPuntReturnStartYard() != 0");
		}
		if (puntReturner.getPuntReturnFumble() != 0) {
			throw new IllegalArgumentException("allReturn.getPuntReturnFumble() != 0");
		}
		if (puntReturner.getPuntReturnFumbleLost() != 0) {
			throw new IllegalArgumentException("allReturn.getPuntReturnFumbleLost() != 0");
		}
		if (puntReturner.getPuntReturnBlock() != 1) {
			throw new IllegalArgumentException("allReturn.getPuntReturnBlock() != 1");
		}
	}

	private static void validatePuntHelperReturnTouchdown(PbpPlayerStatPuntingPojo punter,
			PbpPlayerStatPuntReturnPojo puntReturner) {
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
				LoggingUtils.logInfo(puntLandYardLogStr);
				LoggingUtils.logInfo(puntReturnYardLogStr);
				throw new IllegalArgumentException(
						PUNTER_GET_PUNT_BLOCKED_0_PUNTER_GET_PUNT_LAND_YARD_PUNTER_GET_PUNT_RETURN_YARD_0);
			}
			if (punter.getPuntBlocked() == 0
					&& puntReturner.getPuntReturnStartYard() + puntReturner.getPuntReturnYard() != 100) {
				String puntLandYardLogStr = String.format("Return Start Yard: %s",
						puntReturner.getPuntReturnStartYard());
				String puntReturnYardLogStr = String.format("Return Yard: %s", puntReturner.getPuntReturnYard());
				LoggingUtils.logInfo(puntLandYardLogStr);
				LoggingUtils.logInfo(puntReturnYardLogStr);
				throw new IllegalArgumentException("punter.getPuntBlocked() == 0\n"
						+ "&& puntReturner.getPuntReturnStartYard() + puntReturner.getPuntReturnYard() != 100");
			}

		}
	}

	private static void validatePuntHelperTouchdown(PbpPlayerStatPuntingPojo punter,
			PbpPlayerStatPuntReturnPojo puntReturner) {
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
	}

	private static void validatePuntHelperTouchback(PbpPlayerStatPuntingPojo punter,
			List<PbpPlayerStatPuntReturnPojo> allReturnStats, List<PbpPlayerStatDefenseProductionPojo> coverage) {
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
			if (!coverage.isEmpty()) {
				throw new IllegalArgumentException(COVERAGE_SIZE_0);
			}
			if (!allReturnStats.isEmpty()) {
				throw new IllegalArgumentException("returner.isEmpty()");
			}
		}
	}

	private static void validatePuntHelperFairCatch(PbpServiceRequestPojo params, PbpPlayerStatPuntingPojo punter,
			PbpPlayerStatPuntReturnPojo puntReturner, List<PbpPlayerStatPuntReturnPojo> allReturnStats,
			List<PbpPlayerStatDefenseProductionPojo> coverage) {
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
			if (!coverage.isEmpty()) {
				throw new IllegalArgumentException(COVERAGE_SIZE_0);
			}
			if (allReturnStats.size() != 1) {
				throw new IllegalArgumentException("returner.size() != 1");
			}
			validatePuntHelperFairCatchExtra(params, punter, puntReturner);
		}
	}

	private static void validatePuntHelperFairCatchExtra(PbpServiceRequestPojo params, PbpPlayerStatPuntingPojo punter,
			PbpPlayerStatPuntReturnPojo puntReturner) {
		if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnFairCatch() != 1) {
			throw new IllegalArgumentException("returner.get(0).getPuntReturnFairCatch() != 1");
		}
		if (Objects.nonNull(puntReturner) && puntReturner.getPuntReturnStartYard() != 100
				- (params.getPlay().getPlayStartYard() + punter.getPuntYard())) {

			String returnYardLogStr = String.format("Return Start Yard: %s", params.getPlay().getPlayerStat()
					.get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn().get(0).getPuntReturnStartYard());
			String startYardLogStr = String.format("Start Yard: %s", params.getPlay().getPlayStartYard());
			String puntYardLogStr = String.format("Punt Yards: %s", params.getPlay().getPlayerStat()
					.get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0).getPuntYard());
			String driveTextLogStr = String.format("Drive Text: %s", params.getPlay().getDriveText());
			String abbrevLogStr = String.format("Abbrev: %s", params.getTeamAbbrevDict());
			LoggingUtils.logInfo(returnYardLogStr);
			LoggingUtils.logInfo(startYardLogStr);
			LoggingUtils.logInfo(puntYardLogStr);
			LoggingUtils.logInfo(driveTextLogStr);
			LoggingUtils.logInfo(abbrevLogStr);

			throw new IllegalArgumentException("returner.get(0).getPuntReturnStartYard() != 100\n"
					+ "- (params.getPlay().getPlayStartYard() + punter.getPuntYard())");
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

	private static void validatePuntHelperBlock(PbpPlayerStatPuntingPojo punter,
			List<PbpPlayerStatPuntReturnPojo> allReturnStats, List<PbpPlayerStatDefenseProductionPojo> coverage) {
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
			if (!coverage.isEmpty()) {
				throw new IllegalArgumentException(COVERAGE_SIZE_0);
			}
			if (allReturnStats.isEmpty()) {
				throw new IllegalArgumentException("returner.isEmpty()");
			}
			if (allReturnStats.stream().filter(ret -> ret.getPuntReturnBlock() == 1).collect(Collectors.toList())
					.isEmpty()) {
				throw new IllegalArgumentException(
						"returner.stream().filter(ret -> ret.getPuntReturnBlock() == 1).collect(Collectors.toList())\n"
								+ IS_EMPTY);
			}
		}
	}
}
