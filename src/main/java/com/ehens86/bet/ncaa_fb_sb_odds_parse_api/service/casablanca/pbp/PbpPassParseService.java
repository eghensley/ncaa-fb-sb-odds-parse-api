package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PassDirectionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpPassParseService {
	private static final Logger LOG = Logger.getLogger(PbpPassParseService.class.toString());

	private final PbpParsingUtils pbpParsingUtils;

	public PbpPassParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public void parsePass(PbpServiceRequestPojo params) {
		try {

//			params.setPlayRawText(params.getPlayRawText().replaceAll("(( [A-Z][a-z])\\.,? )((?:at|rush))", "$2 $3"));
			params.setPlayRawText(params.getPlayRawText().replace(". St. pass", " pass"));
			params.setPlayRawText(params.getPlayRawText().replace(", pass ", " pass "));

			if (params.getPlayRawText().startsWith("pass")) {
				String replacementPlayerName = String.format("TEAM, %s", params.getTeamAbbrevDict()
						.get(params.getPossessionTeam()).getSeoName().toUpperCase().replace(" ", "").replace("-", ""));
				String replacementPlayText = String.format("%s %s", replacementPlayerName, params.getPlayRawText());
				replacementPlayText = replacementPlayText.replaceAll("((fumbled by)( at))",
						String.format("$2 %s$3", replacementPlayerName));

				params.setPlayRawText(replacementPlayText);
				params.getPlay().setPlayText(params.getPlayRawText());
			}

//			if ("[SG] WILSON, Darius pass to the left incomplete intended for MAYO, JT broken up by SMALLS, Shakur at the MAI32; QB hurried by LEZIN, Josh".equals(params.getPlayRawText())) {
//				params.setPossessionTeam("1835");
//			}
//			if ("[NHSG] WILSON, Darius pass to the left incomplete intended for BURDICK, Zach thrown to the MAI36".equals(params.getPlayRawText())) {
//				params.setPossessionTeam("1835");
//			}

			params.getDrive().setKickoff(false);
			parsePassingBasic(params);
			parseReceivingBasic(params);
			parseDropped(params);
			parsePassingYards(params);
//			parseFirstDown(params);
//			parseTouchdown(params);
//			parseCompletion(params);
//			parseYardsAfterCatch(params);
//			//Defense
//			parseBreakup(params);
//			parseTackles(params);
//			parseQbHurry(params);
//			parseSack(params);
//			parseInterception(params);
//			parseFumble(params);
//			parseRecovery(params);
//			validate(params);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void validate(PbpServiceRequestPojo params) {
		// LOG.log(Level.INFO, "-- Passing validation");
		// TODO Receiving Tests
		// TODO QB Tests
		// TODO Defense Tests
		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
		}

		if (params.getPlay().getPlayStartDown() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartDown() == null");
		}
		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
		}
		if (params.getPlay().getPlayYardToGain() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() == null");
		}
		if (params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() == null");
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PASS) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PASS");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().size() != 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPlayerName() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPlayerName() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingInterceptionTouchdown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterceptionTouchdown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingInterceptionYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterceptionYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingBreakup() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingBreakup() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingCompletion() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingCompletion() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingAttempt() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingAttempt() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingAttempt() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingAttempt() != 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingInterception() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingInterception() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingFirstDown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFirstDown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingTouchdown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingTouchdown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingYard() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayerStat()
				.get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFirstDown() == 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ "				.getPassingFirstDown() == 0");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == false && params.getPlay().getPlayerStat()
				.get(params.getPossessionTeam()).getOffense().getPassingStat().get(0).getPassingFirstDown() == 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == false && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)\n"
							+ "				.getPassingFirstDown() == 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
				.getPassingTouchdown() == 0) {
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
//				throw new IllegalArgumentException(
//						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			}
		} else {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
				throw new IllegalArgumentException(
						"!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			}
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().size() != 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
				.getPlayerName() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0).getPlayerName() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
				.getReceivingTarget() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0).getReceivingTarget() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
				.getReceivingReception() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0).getReceivingReception() == null");
		}

	}

	private void parsePassingBasic(PbpServiceRequestPojo params) {
		try {
			String[] downAndDistance = pbpParsingUtils.convertDownAndDistance(params.getPlay().getDriveText());
			String rushString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"%s pass ?((?:up the middle)|(?:to the (?:(?:right)|(?:left))))? ((?:(?:in)?complete)|(?:intercepted))",
					NcaaConstants.playerNameRegex), 9);
			String[] passStringArray = rushString.split("\\|")[0].split("\\~");
			PlayerStatPassingPojo passingStat = new PlayerStatPassingPojo(
					pbpParsingUtils.formatName(passStringArray[0]));

			if (!"null".equals(passStringArray[7])) {
				// throw new IllegalArgumentException("Handle pass direction");
				passingStat
						.setPassingDirection(PassDirectionEnum.valueOf(passStringArray[7].split(" ")[2].toUpperCase()));

			}
			if ("incomplete".equals(passStringArray[8])) {
				passingStat.setPassingYard(0);
				passingStat.setPassingCompletion(0);
				params.getPlay().getPlayResult().setPlayResultYard(0);
			} else if ("interception".equals(passStringArray[8])) {
				passingStat.setPassingYard(0);
				passingStat.setPassingCompletion(0);
				passingStat.setPassingInterception(1);
				params.getPlay().getPlayResult().setPlayResultTurnover(true);
			}

			params.getPlay().setPlayStartYard(pbpParsingUtils.formatYardLine(downAndDistance[2],
					params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			params.getPlay().setPlayStartDown(PlayDownEnum.valueOf(downAndDistance[0]));
			params.getPlay().setPlayYardToGain(Integer.valueOf(downAndDistance[1]));

			params.getPlay().getPlayResult().setPlayResultTurnover(false);
			params.getPlay().setPlayCallType(PlayCallTypeEnum.PASS);
			// TODO find by player name
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
					.add(passingStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseReceivingBasic(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().contains("intercepted")
					|| params.getPlayRawText().contains("incomplete broken up by")) {
				if (params.getPlayRawText().contains("intended")) {
					// throw new IllegalArgumentException("HANDLE RECEIVING INTENDED RECEIVER");
				} else {
					return;
				}

			}

			Integer completion;
			String receiverName;
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format(
					"((?:(?:in)?complete)|(?:intercepted)).+?((?:to)|(?:intended for)|(?:dropped by)) %s(?:(?:$)|(?: (?:for))|(?: \\((.+)\\))|(?:,? QB hurr(?:(?:y)|(?:ied)))|(?: broken up)|(?: thrown)|(?:\\. )|(?:, dropped))",
					NcaaConstants.playerNameRegex))) {
				String receivingString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
						"((?:(?:in)?complete)|(?:intercepted)).+?((?:to)|(?:intended for)|(?:dropped by)) %s(?:(?:$)|(?: (?:for))|(?: \\((.+)\\))|(?:,? QB hurr(?:(?:y)|(?:ied)))|(?: broken up)|(?: thrown)|(?:\\. )|(?:, dropped))",
						NcaaConstants.playerNameRegex), 9);
				String[] receivingStringArray = receivingString.split("\\|")[0].split("\\~");

				if ("complete".equals(receivingStringArray[0])) {
					completion = 1;
				} else {
					completion = 0;
				}
				receiverName = pbpParsingUtils.formatName(receivingStringArray[2]);
			} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("yards? to %s (?:(?:caught)|(?:to))", NcaaConstants.playerNameRegex))) {
				String receivingString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("yards? to %s (?:(?:caught)|(?:to))", NcaaConstants.playerNameRegex), 7);
				String[] receivingStringArray = receivingString.split("\\|")[0].split("\\~");
				completion = 1;
				receiverName = pbpParsingUtils.formatName(receivingStringArray[0]);
			} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					"pass .*?incomplete,? (?:(?:thrown to the )|(?: ?QB hurr(?:(?:y)|(?:ied)))|(?:\\((?:.+)\\)))")
					&& !params.getPlayRawText().contains("intended")) {
				return;
			} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s pass incomplete$", NcaaConstants.playerNameRegex))) {
				return;
			} else {
				throw new IllegalArgumentException("No Receiver Match Found");
			}

			PlayerStatReceivingPojo receiverStat = new PlayerStatReceivingPojo(receiverName, completion);

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
					.add(receiverStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseDropped(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), " drop(ped)? ")) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.setReceivingDrop(1);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingDrop(1);
			} else {
				if (params.getPlayRawText().contains("dropped")) {
					throw new IllegalArgumentException("HANDLE DROP CASE");
				}
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).setReceivingDrop(0);
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingDrop(0);

			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePassingYards(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format(" complete .+? for (-?\\d{1,3}) yards?"))) {
				String yardsString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format(" complete .+? for (-?\\d{1,3}) yards?"), 1);
				String[] yardsStringArray = yardsString.split("\\|")[0].split("\\~");
				Integer yard = Integer.valueOf(yardsStringArray[0]);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.setReceivingYard(yard);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingYard(yard);
			} else {
				if (params.getPlayRawText().contains(" yard") && !params.getPlayRawText().contains(" intercept")) {
					throw new IllegalArgumentException("HANDLE YARDS CASE");
				} else {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingYard(0);
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
}
