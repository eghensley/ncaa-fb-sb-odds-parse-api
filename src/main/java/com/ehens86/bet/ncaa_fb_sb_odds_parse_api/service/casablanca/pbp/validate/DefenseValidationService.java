package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.exceptions.PbpValidationException;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;

public final class DefenseValidationService {
	private static final String IS_EMPTY = ".isEmpty())\n";
	private static final String PARAMS_GET_PLAY_GET_HAVOC_NULL = "params.getPlay().getHavoc() != null";
	private static final String DEF_GET_TACKLE_TOTAL_0 = "def.getTackleTotal() == 0";
	private static final String DEF_GET_TACKLE_FOR_LOSS_0 = "def.getTackleForLoss() == 0";

    // Private constructor to prevent instantiation
    private DefenseValidationService() {
        throw new UnsupportedOperationException();
    }
    
	public static void validateDef(PbpServiceRequestPojo params, PbpPlayerStatDefenseProductionPojo def) {

		List<PbpPlayerStatPassingPojo> pass = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getOffense().getPassingStat();
		List<PbpPlayerStatRushingPojo> rush = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getOffense().getRushingStat();
		List<PbpPlayerStatReceivingPojo> receive = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getOffense().getReceivingStat();
		List<PbpPlayerStatPuntReturnPojo> puntRet = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getPuntReturn();
		List<PbpPlayerStatKickReturnPojo> kickRet = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKickReturn();
		List<PbpPlayerStatKickingPojo> kick = params.getPlay().getPlayerStat().get(params.getPossessionTeam())
				.getSpecialTeam().getKicking();

		validateDefenseHelperNulls(def);
		validateDefenseHelperTackle(def);
		validateDefenseHelperTackleAssistSolo(def);
		validateDefenseHelperSafety(def, params);
		validateDefenseHelperTouchdown(def, params);
		validateDefenseHelperPunt(def, params);
		validateDefenseHelperFieldGoal(def, params, kick);
		validateDefenseHelperPass(def, params);
		validateDefenseHelperSack(def, pass);
		if (def.getFumbleRecovered() == 1) {
			validateDefenseHelperRecoveredFumble(params, puntRet, kickRet, receive, pass, rush, kick);
		}

		validateDefenseHelperForcedFumble(def, receive, pass, rush, puntRet, kickRet);
		validateDefenseHelperHavoc(def, params);		
	}

	
	private static void validateDefenseHelperNulls(PbpPlayerStatDefenseProductionPojo def) {
		if (def.getPlayerName().isBlank()) {
			throw new PbpValidationException("def.getPlayerName().isBlank()");
		}
		if (Objects.isNull(def.getTackleSolo())) {
			throw new PbpValidationException("def.getTackleSolo() == null");
		}
		if (Objects.isNull(def.getTackleAssist())) {
			throw new PbpValidationException("def.getTackleAssist() == null");
		}
		if (Objects.isNull(def.getSack())) {
			throw new PbpValidationException("def.getSack() == null");
		}
		if (Objects.isNull(def.getPassBreakUp())) {
			throw new PbpValidationException("def.getPassBreakUp() == null");
		}
		if (Objects.isNull(def.getInterception())) {
			throw new PbpValidationException("def.getInterception() == null");
		}
		if (Objects.isNull(def.getFumbleForced())) {
			throw new PbpValidationException("def.getFumbleForced() == null");
		}
		if (Objects.isNull(def.getFumbleRecovered())) {
			throw new PbpValidationException("def.getFumbleRecovered() == null");
		}
		if (Objects.isNull(def.getFumbleYard())) {
			throw new PbpValidationException("def.getFumbleYard() == null");
		}
		if (Objects.isNull(def.getFumbleTouchdown())) {
			throw new PbpValidationException("def.getFumbleTouchdown() == null");
		}
		if (Objects.isNull(def.getSafety())) {
			throw new PbpValidationException("def.getSafety() == null");
		}
		if (Objects.isNull(def.getInterceptionTouchdown())) {
			throw new PbpValidationException("def.getInterceptionTouchdown() == null");
		}
		if (Objects.isNull(def.getInterceptionYard())) {
			throw new PbpValidationException("def.getInterceptionYard() == null");
		}
		if (Objects.isNull(def.getQuarterbackHurry())) {
			throw new PbpValidationException("def.getQuarterbackHurry() == null");
		}
		if (Objects.isNull(def.getKickBlock())) {
			throw new PbpValidationException("def.getKickBlock() == null");
		}
	}

	private static void validateDefenseHelperTackle(PbpPlayerStatDefenseProductionPojo def) {
		if (Objects.isNull(def.getTackleTotal())) {
			throw new PbpValidationException("def.getTackleTotal() == null");
		}
		if (Objects.isNull(def.getTackleForLoss())) {
			throw new PbpValidationException("def.getTackleForLoss() == null");
		}
		if (def.getTackleForLoss() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getTackleYard() > 0) {
				throw new PbpValidationException("def.getTackleYard() > 0");
			}
		}

		if (def.getTackleYard() < 0) {
			if (def.getTackleForLoss() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_FOR_LOSS_0);
			} else {
				// nothing needed right now
			}
		}
	}

	private static void validateDefenseHelperTackleAssistSolo(PbpPlayerStatDefenseProductionPojo def) {
		if (def.getTackleAssist() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getFumbleForced() != 1 && def.getTackleSolo() != 0) {
				throw new PbpValidationException("def.getTackleSolo() != 0");
			}
		}

		if (def.getTackleSolo() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getFumbleForced() != 1 && def.getTackleAssist() != 0) {
				throw new PbpValidationException("def.getTackleAssist() != 0");
			}
		}
	}

	private static void validateDefenseHelperSafety(PbpPlayerStatDefenseProductionPojo def, PbpServiceRequestPojo params) {
		if (def.getSafety() == 1) {
			if (def.getTackleForLoss() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_FOR_LOSS_0);
			}
			if (def.getTackleTotal() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (params.getPlay().getPlayResult().getPlayResultPoints() != -2) {
				throw new PbpValidationException("params.getPlay().getPlayResult().getPlayResultPoints() != -2");
			}
		}
	}

	private static void validateDefenseHelperPunt(PbpPlayerStatDefenseProductionPojo def, PbpServiceRequestPojo params) {
		if (!PlayCallTypeEnum.PUNT.equals(params.getPlay().getPlayCallType())) {
			if (Objects.isNull(def.getTackleYard())) {
				throw new PbpValidationException("def.getTackleYard() == null");
			} else {
				// nothing needed here yet
			}
		}
	}

	private static void validateDefenseHelperFieldGoal(PbpPlayerStatDefenseProductionPojo def, PbpServiceRequestPojo params,
			List<PbpPlayerStatKickingPojo> kick) {
		if (PlayCallTypeEnum.FG.equals(params.getPlay().getPlayCallType())
				|| PlayCallTypeEnum.PAT.equals(params.getPlay().getPlayCallType())) {
			if (def.getKickBlock() == 1 && kick.get(0).getFieldGoalBlock() == 0
					&& kick.get(0).getExtraPointBlock() == 0) {
				throw new PbpValidationException("def.getKickBlock() == 1 && kick.get(0).getFieldGoalBlock() == 0");
			}
		} else {
			if (def.getKickBlock() == 1) {
				throw new PbpValidationException("def.getKickBlock() == 1");
			}
		}
	}

	private static void validateDefenseHelperTouchdown(PbpPlayerStatDefenseProductionPojo def, PbpServiceRequestPojo params) {
		if (def.getInterceptionTouchdown() == 1) {
			if (def.getInterception() == 0) {
				throw new PbpValidationException("def.getInterception() == 0");
			}
			if (def.getInterceptionYard() == 0) {
				throw new PbpValidationException("def.getInterceptionYard() == 0");
			}
		}

		if (def.getFumbleTouchdown() == 1) {
			if (def.getFumbleRecovered() == 0) {
				throw new PbpValidationException("def.getFumbleRecovered() == 0");
			}
			if (Boolean.FALSE.equals(params.getDrive().getKickoff()) && def.getFumbleYard() == 0) {
				throw new PbpValidationException("def.getFumbleYard() == 0");
			}
		}
	}

	private static void validateDefenseHelperPass(PbpPlayerStatDefenseProductionPojo def, PbpServiceRequestPojo params) {
		if (!PlayCallTypeEnum.PASS.equals(params.getPlay().getPlayCallType())) {
			if (def.getSack() == 1) {
				throw new PbpValidationException("def.getSack() == 1");
			}
			if (def.getPassBreakUp() == 1) {
				throw new PbpValidationException("def.getPassBreakUp() == 1");
			}
			if (def.getInterception() == 1) {
				throw new PbpValidationException("def.getInterception() == 1");
			}
			if (def.getInterceptionTouchdown() == 1) {
				throw new PbpValidationException("def.getInterceptionTouchdown() == 1");
			}
			if (def.getInterceptionYard() == 1) {
				throw new PbpValidationException("def.getInterceptionYard() == 1");
			}
			if (def.getQuarterbackHurry() == 1) {
				throw new PbpValidationException("def.getQuarterbackHurry() == 1");
			}
		}
	}

	private static void validateDefenseHelperSack(PbpPlayerStatDefenseProductionPojo def,
			List<PbpPlayerStatPassingPojo> pass) {
		if (def.getSack() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getTackleYard() > 0) {
				throw new PbpValidationException("def.getTackleYard() > 0");
			}
			if (pass.get(0).getPassingSack() == 0) {
				throw new PbpValidationException("pass.getPassingSack() == 0");
			}
		} else {
			if (Objects.nonNull(pass) && !pass.isEmpty() && pass.get(0).getPassingSack() != 0) {
				throw new PbpValidationException("pass.getPassingSack() != 0");
			}
		}
	}

	private static void validateDefenseHelperRecoveredFumble(PbpServiceRequestPojo params,
			List<PbpPlayerStatPuntReturnPojo> puntRet, List<PbpPlayerStatKickReturnPojo> kickRet, List<PbpPlayerStatReceivingPojo> receive,
			List<PbpPlayerStatPassingPojo> pass, List<PbpPlayerStatRushingPojo> rush,
			List<PbpPlayerStatKickingPojo> kick) {
		if (Boolean.TRUE.equals(params.getDrive().getKickoff())) {
			if (kickRet.stream().filter(kr -> kr.getKickReturnFumbleLost() == 1).collect(Collectors.toList())
					.isEmpty()) {
				throw new PbpValidationException(
						"kickRet.stream().filter(kr -> kr.getKickReturnFumbleLost() == 1).collect(Collectors.toList())\n"
								+ IS_EMPTY);
			}
		} else if (PlayCallTypeEnum.PUNT.equals(params.getPlay().getPlayCallType())) {
			if (puntRet.stream().filter(pr -> pr.getPuntReturnFumbleLost() == 1).collect(Collectors.toList())
					.isEmpty()) {
				throw new PbpValidationException(
						"puntRet.stream().filter(pr -> pr.getPuntReturnFumbleLost() == 1).collect(Collectors.toList())\n"
								+ IS_EMPTY);
			} else {
				// nothing needed here yet
			}
		} else {
			validateDefenseHelperNoRecoveredFumble(receive, pass, rush, kick);
		}
	}

	private static void validateDefenseHelperNoRecoveredFumble(List<PbpPlayerStatReceivingPojo> receive,
			List<PbpPlayerStatPassingPojo> pass, List<PbpPlayerStatRushingPojo> rush,
			List<PbpPlayerStatKickingPojo> kick) {
		if ((pass.stream().filter(p -> p.getPassingFumbleLost() == 1).collect(Collectors.toList()).isEmpty())
				&& (rush.stream().filter(r -> r.getRushingFumbleLost() == 1).collect(Collectors.toList()).isEmpty())
				&& (receive.stream().filter(rc -> rc.getReceivingFumbleLost() == 1).collect(Collectors.toList())
						.isEmpty())
				&& (kick.stream().filter(k -> k.getFieldGoalBlock() == 1).collect(Collectors.toList()).isEmpty())) {
			throw new PbpValidationException(
					"(pass.stream().filter(p -> p.getPassingFumbleLost() == 1).collect(Collectors.toList()).isEmpty())\n"
							+ "&& (rush.stream().filter(r -> r.getRushingFumbleLost() == 1).collect(Collectors.toList())\n"
							+ IS_EMPTY
							+ "&& (receive.stream().filter(rc -> rc.getReceivingFumbleLost() == 1).collect(Collectors.toList())\n"
							+ ".isEmpty())");
		}
	}

	private static void validateDefenseHelperForcedFumble(PbpPlayerStatDefenseProductionPojo def,
			List<PbpPlayerStatReceivingPojo> receive, List<PbpPlayerStatPassingPojo> pass,
			List<PbpPlayerStatRushingPojo> rush, List<PbpPlayerStatPuntReturnPojo> puntRet,
			List<PbpPlayerStatKickReturnPojo> kickRet) {
		if (def.getFumbleForced() == 1) {
			if (def.getTackleTotal() == 0) {
				throw new PbpValidationException(DEF_GET_TACKLE_TOTAL_0);
			}
			if (def.getTackleSolo() == 0.0) {
				throw new PbpValidationException("def.getTackleSolo() == 0");
			}
			if ((pass.stream().filter(p -> p.getPassingFumble() == 1).collect(Collectors.toList()).isEmpty())
					&& (rush.stream().filter(r -> r.getRushingFumble() == 1).collect(Collectors.toList()).isEmpty())
					&& (receive.stream().filter(rc -> rc.getReceivingFumble() == 1).collect(Collectors.toList())
							.isEmpty())
					&& (kickRet.stream().filter(kr -> kr.getKickReturnFumble() == 1).collect(Collectors.toList())
							.isEmpty())
					&& (puntRet.stream().filter(pr -> pr.getPuntReturnFumble() == 1).collect(Collectors.toList())
							.isEmpty())) {
				throw new PbpValidationException(
						"(pass.stream().filter(p -> p.getPassingFumble() == 1).collect(Collectors.toList()).isEmpty())\n"
								+ "&& (rush.stream().filter(r -> r.getRushingFumble() == 1).collect(Collectors.toList()).isEmpty())\n"
								+ "&& (receive.stream().filter(rc -> rc.getReceivingFumble() == 1).collect(Collectors.toList())\n"
								+ IS_EMPTY
								+ "&& (kickRet.stream().filter(kr -> kr.getKickReturnFumble() == 1).collect(Collectors.toList())\n"
								+ IS_EMPTY
								+ "&& (puntRet.stream().filter(pr -> pr.getPuntReturnFumble() == 1).collect(Collectors.toList())\n"
								+ ".isEmpty())");
			}
		}
	}

	private static void validateDefenseHelperHavoc(PbpPlayerStatDefenseProductionPojo def, PbpServiceRequestPojo params) {
		if (PlayTypeEnum.OFFENSE.equals(params.getPlay().getPlayType())) {
			validateDefenseHelperHavocOffense(def, params);
		} else {
			if (Objects.nonNull(params.getPlay().getHavoc())) {
				throw new PbpValidationException("Objects.nonNull(params.getPlay().getHavoc())");
			}
			if (Objects.nonNull(params.getPlay().getHavocFront())) {
				throw new PbpValidationException("Objects.nonNull(params.getPlay().getHavocFront())");
			}
			if (Objects.nonNull(params.getPlay().getHavocDb())) {
				throw new PbpValidationException("Objects.nonNull(params.getPlay().getHavocDb())");
			}
		}
	}

	private static void validateDefenseHelperHavocOffense(PbpPlayerStatDefenseProductionPojo def,
			PbpServiceRequestPojo params) {
		if (Objects.isNull(params.getPlay().getHavoc())) {
			throw new PbpValidationException(PARAMS_GET_PLAY_GET_HAVOC_NULL);
		}

		if (def.getPassBreakUp() == 1 || def.getInterception() == 1) {
			if (Boolean.FALSE.equals(params.getPlay().getHavocDb())) {
				throw new PbpValidationException("Boolean.FALSE.equals(params.getPlay().getHavocDb())");
			} else {
				// nothing needed here yet
			}
		}
		if (def.getFumbleForced() == 1 || def.getTackleForLoss() == 1) {
			if (Boolean.FALSE.equals(params.getPlay().getHavocFront())) {
				throw new PbpValidationException("Boolean.FALSE.equals(params.getPlay().getHavocFront())");
			} else {
				// nothing needed here yet
			}
		}
	}

}
