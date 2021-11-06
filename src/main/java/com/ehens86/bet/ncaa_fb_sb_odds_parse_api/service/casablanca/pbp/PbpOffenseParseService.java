package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpOffenseParseService {
	private static final Logger LOG = Logger.getLogger(PbpOffenseParseService.class.toString());

	@SuppressWarnings("unused")
	private final PbpParsingUtils pbpParsingUtils;

	public PbpOffenseParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public void parseOffense(PbpServiceRequestPojo params) {
		try {
			parseFirstDown(params);
			parseTouchdown(params);
			parseTurnoverOnDowns(params);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseFirstDown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("1ST DOWN")
					&& !params.getPlay().getPlayResult().isPlayResultTurnover()) {
				parseFirstDownHelper(params);
			} else {
				parseNoFirstDownHelper(params);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
//			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseFirstDownHelper(PbpServiceRequestPojo params) {
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
				throw new IllegalArgumentException("MISSING PLAY TYPE FOR PLAY");
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseNoFirstDownHelper(PbpServiceRequestPojo params) {
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

			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {

			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {

			} else {
				throw new IllegalArgumentException("MISSING PLAY TYPE FOR PLAY");
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTouchdown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("TOUCHDOWN")) {
				if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
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
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTouchdownTurnoverHelper(PbpServiceRequestPojo params) {
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

			} else {
				throw new IllegalArgumentException("MISSING PLAY TYPE FOR PLAY");
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTouchdownNoTurnoverHelper(PbpServiceRequestPojo params) {
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
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnTouchdown(1);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findPuntReturner()
						.setPuntReturnTouchdown(1);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnTouchdown(1);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findKickoffReturner()
						.setKickReturnTouchdown(1);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG) {
				params.getPlay().getPlayResult().setPlayResultPoints(6);
			} else {
				throw new IllegalArgumentException("MISSING PLAY TYPE FOR PLAY");
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseNoTouchdownHelper(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingTouchdown(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingInterceptionTouchdown(0);
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).setReceivingTouchdown(0);
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingTouchdown(0);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnTouchdown(0);
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()
						&& Objects.nonNull(params.getPlay().getPlayerStat().get(params.getDefenseTeam())
								.getSpecialTeam().findPuntReturner())) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findPuntReturner()
							.setPuntReturnTouchdown(0);
				}
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnTouchdown(0);
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()
						&& Objects.nonNull(params.getPlay().getPlayerStat().get(params.getDefenseTeam())
								.getSpecialTeam().findKickoffReturner())) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findKickoffReturner()
							.setKickReturnTouchdown(0);
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {

			} else {
				throw new IllegalArgumentException("MISSING PLAY TYPE FOR PLAY");
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
	
	private void parseTurnoverOnDowns(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().contains("turnover on downs")) {
				params.getPlay().getPlayResult().setPlayResultTurnover(true);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

}
