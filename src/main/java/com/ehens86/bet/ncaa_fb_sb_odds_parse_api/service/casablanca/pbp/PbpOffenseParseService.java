package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.Objects;

import org.apache.logging.log4j.ThreadContext;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;

public final class PbpOffenseParseService {
	private static final String MISSING_PLAY_CALL_TYPE_FOR_CONDITIONAL_EVALUATION = "Missing play call type for conditional evaluation";

	private static final String PLAY_CALL_TYPE_S_PLAY_TEXT_S = "Play Call Type: %s | Play text: %s";

    // private static constructor to prevent instantiation
    private PbpOffenseParseService() {
        throw new UnsupportedOperationException();
    }

	public static void parseOffense(PbpServiceRequestPojo params) {
		try {
			parseFirstDown(params);
			parseTouchdown(params);
			parseTurnoverOnDowns(params);

			if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
				LoggingUtils.logInfo("- [parseOffense] Results -");
				LoggingUtils.logInfo(String.format("Play Start Yard Line: %s", params.getPlay().getPlayStartYard()));
				LoggingUtils.logInfo(String.format("Play End Yard Line: %s",
						params.getPlay().getPlayResult().getPlayResultYardLine()));
				LoggingUtils.logInfo(
						String.format("Play Yards Gained: %s", params.getPlay().getPlayResult().getPlayResultYard()));
				LoggingUtils.logInfo(String.format("Play Points Gained: %s",
						params.getPlay().getPlayResult().getPlayResultPoints()));
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void parseFirstDown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("1ST DOWN")
					&& Boolean.FALSE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
				parseFirstDownHelper(params);
			} else {
				parseNoFirstDownHelper(params);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFirstDownHelper(PbpServiceRequestPojo params) {
		try {
			params.getPlay().getPlayResult().setPlayResultFirstDown(true);
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingFirstDown(1);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.setRecievingFirstDown(1);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFirstDown(1);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				throw new IllegalArgumentException("HANDLE CASE");
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				throw new IllegalArgumentException("Handle kickoff");
			} else {
				String logInfo = String.format(PLAY_CALL_TYPE_S_PLAY_TEXT_S, params.getPlay().getPlayCallType(),
						params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(MISSING_PLAY_CALL_TYPE_FOR_CONDITIONAL_EVALUATION);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseNoFirstDownHelper(PbpServiceRequestPojo params) {
		try {
			params.getPlay().getPlayResult().setPlayResultFirstDown(false);
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingFirstDown(0);
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).setRecievingFirstDown(0);
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFirstDown(0);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				// Nothing needed here right now
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				// Nothing needed here right now
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				// Nothing needed here right now
			} else {
				String logInfo = String.format(PLAY_CALL_TYPE_S_PLAY_TEXT_S, params.getPlay().getPlayCallType(),
						params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(MISSING_PLAY_CALL_TYPE_FOR_CONDITIONAL_EVALUATION);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseTouchdown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("TOUCHDOWN")) {
				if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
					params.getPlay().getPlayResult().setPlayResultPoints(-6);
					if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findPuntCoverageWithTurnover().applyReturnTouchdown();
					} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findKickCoverageWithTurnover().applyReturnTouchdown();
					} else {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findDefenseWithTurnover().applyReturnTouchdown();
					}
					parseTouchdownTurnoverHelper(params);
				} else {
					params.getPlay().getPlayResult().setPlayResultPoints(6);
					parseTouchdownNoTurnoverHelper(params);
				}
			} else if (Objects.nonNull(params.getPlay().getPlayResult().getPlayResultPoints())
					&& (params.getPlay().getPlayResult().getPlayResultPoints() == -2
							|| params.getPlay().getPlayResult().getPlayResultPoints() == 3
							|| params.getPlay().getPlayResult().getPlayResultPoints() == 1
							|| params.getPlay().getPlayResult().getPlayResultPoints() == 2)) {
				parseNoTouchdownHelper(params);
			} else {
				params.getPlay().getPlayResult().setPlayResultPoints(0);
				parseNoTouchdownHelper(params);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseTouchdownTurnoverHelper(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingTouchdown(0);
				if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().findDefenseWithTurnover()
						.getInterception() == 1) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingInterceptionTouchdown(1);
				}
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).setReceivingTouchdown(0);
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingTouchdown(0);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnTouchdown(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findKickCoverageWithTurnover().applyReturnTouchdown();
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findKickoffReturner()
						.setKickReturnTouchdown(0);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG) {
				// nothing needed right now
			} else {
				String logInfo = String.format(PLAY_CALL_TYPE_S_PLAY_TEXT_S, params.getPlay().getPlayCallType(),
						params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(MISSING_PLAY_CALL_TYPE_FOR_CONDITIONAL_EVALUATION);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseTouchdownNoTurnoverHelper(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingTouchdown(1);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingInterceptionTouchdown(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.setReceivingTouchdown(1);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingTouchdown(1);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnTouchdown(1);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().findPuntReturner()
						.setPuntReturnTouchdown(1);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnTouchdown(1);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().findKickoffReturner()
						.setKickReturnTouchdown(1);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG) {
				params.getPlay().getPlayResult().setPlayResultPoints(6);
			} else {
				String logInfo = String.format(PLAY_CALL_TYPE_S_PLAY_TEXT_S, params.getPlay().getPlayCallType(),
						params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(MISSING_PLAY_CALL_TYPE_FOR_CONDITIONAL_EVALUATION);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseNoTouchdownHelper(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				parseNoTouchdownHelperPassHelper(params);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingTouchdown(0);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				parseNoTouchdownHelperPuntHelper(params);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				parseNoTouchdownHelperKickoffHelper(params);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				// Nothing needed here right now
			} else {
				String logInfo = String.format(PLAY_CALL_TYPE_S_PLAY_TEXT_S, params.getPlay().getPlayCallType(),
						params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(MISSING_PLAY_CALL_TYPE_FOR_CONDITIONAL_EVALUATION);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseNoTouchdownHelperPassHelper(PbpServiceRequestPojo params) {
		try {
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
					.setPassingTouchdown(0);
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
					.setPassingInterceptionTouchdown(0);
			if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.isEmpty()) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.setReceivingTouchdown(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.getMessage());

		}
	}

	private static void parseNoTouchdownHelperPuntHelper(PbpServiceRequestPojo params) {
		try {
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
					.setPuntReturnTouchdown(0);
			if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
					.isEmpty()
					&& Objects.nonNull(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
							.findPuntReturner())) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().findPuntReturner()
						.setPuntReturnTouchdown(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.getMessage());

		}
	}

	private static void parseNoTouchdownHelperKickoffHelper(PbpServiceRequestPojo params) {
		try {
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
					.setKickoffReturnTouchdown(0);
			if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
					.isEmpty()
					&& Objects.nonNull(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
							.findKickoffReturner())) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().findKickoffReturner()
						.setKickReturnTouchdown(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.getMessage());

		}
	}

	private static void parseTurnoverOnDowns(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().contains("turnover on downs")) {
				params.getPlay().getPlayResult().setPlayResultTurnover(true);
				params.getPlay().getPlayResult().setPlayResultPossessionTeamId(params.getDefenseTeam());
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

}
