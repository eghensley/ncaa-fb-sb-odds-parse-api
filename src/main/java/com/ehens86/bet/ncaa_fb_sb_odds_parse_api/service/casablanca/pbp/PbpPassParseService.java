package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.Objects;

import org.apache.logging.log4j.ThreadContext;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpPassParseService {
	private final PbpParsingUtils pbpParsingUtils;
	private final LoggingUtils loggingUtils;

	public PbpPassParseService(PbpParsingUtils pbpParsingUtils, LoggingUtils loggingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
		this.loggingUtils = loggingUtils;
	}

	public boolean parsePass(PbpServiceRequestPojo params, boolean update) {
		try {
			if (params.getPlayRawText().contains("pass") || params.getPlayRawText().toUpperCase().contains(" SACKED ")
					|| params.getPlayRawText().toUpperCase().contains("SPIKE")
					|| params.getPlayRawText().toUpperCase().contains("PASS ATTEMPT")) {
				cleanUpPassingString(params);

				if (params.getPlayRawText().toUpperCase().contains("PASSING ATTEMPT")
						|| params.getPlayRawText().toUpperCase().contains("PASS ATTEMPT")) {
					parsePatPassing(params);
				} else {
					parsePassingBasic(params);
				}
				parseReceivingBasic(params);
				parseDropped(params);
				parsePassingYards(params);
				parseYardsAfterCatch(params);

				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					String offTeam = params.getPossessionTeam();
					PbpPlayerStatPassingPojo passingInfo = params.getPlay().getPlayerStat().get(offTeam).getOffense()
							.getPassingStat().get(0);

					loggingUtils.logInfo("- [parseRush] Results -");
					loggingUtils.logInfo(
							String.format("Offense Team: %s", params.getTeamAbbrevDict().get(offTeam).getShortname()));
//					loggingUtils.logInfo( String.format("Pass Yards: %s", passingInfo.getPassingYard()));
					loggingUtils.logInfo(String.format("Pass Info: %s", passingInfo.toString()));
				}

				update = true;
			}
			return update;
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

			throw new IllegalArgumentException(e.toString());
		}
	}

	private void cleanUpPassingString(PbpServiceRequestPojo params) {
		try {
			params.setPlayRawText(params.getPlayRawText().replace(". St. pass", " pass"));
			params.setPlayRawText(params.getPlayRawText().replace(", pass ", " pass "));
			params.setPlayRawText(params.getPlayRawText().replace("KannelyRobles, for", "KannelyRobles, Andrew for"));

			if (params.getPlayRawText().startsWith("pass")) {
				String replacementPlayerName = String.format("TEAM, %s", params.getTeamAbbrevDict()
						.get(params.getPossessionTeam()).getSeoName().toUpperCase().replace(" ", "").replace("-", ""));
				String replacementPlayText = String.format("%s %s", replacementPlayerName, params.getPlayRawText());
				replacementPlayText = replacementPlayText.replaceAll("((fumbled by)( at))",
						String.format("$2 %s$3", replacementPlayerName));

				params.setPlayRawText(replacementPlayText);
				params.getPlay().setPlayText(params.getPlayRawText());
			}
			params.setPlayRawText(params.getPlayRawText().replaceAll("pass complete to( )?for (\\d{1,3}) yard",
					String.format("pass complete to TEAM, %s for $2 yard",
							params.getTeamAbbrevDict().get(params.getPossessionTeam()).getSeoName().toUpperCase()
									.replace(" ", "").replace("-", ""))));

			params.setPlayRawText(params.getPlayRawText().replaceAll(
					String.format("pass complete for (\\d{1,3}) yard(s)? to the%s", NcaaConstants.TEAM_YARD_REGEX),
					String.format("pass complete for $1 yard$2 to TEAM, %s to the $3",
							params.getTeamAbbrevDict().get(params.getPossessionTeam()).getSeoName().toUpperCase()
									.replace(" ", "").replace("-", ""))));

			params.setPlayRawText(params.getPlayRawText().replaceAll("(QB hurr(?:(?:y)|(?:ied)) by) ?$",
					String.format("$1 TEAM, %s", params.getTeamAbbrevDict().get(params.getPossessionTeam()).getSeoName()
							.toUpperCase().replace(" ", "").replace("-", ""))));

			params.setPlayRawText(params.getPlayRawText().replaceAll("complete to (\\d{1,2})",
					String.format("complete to TEAM, %s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
							.getSeoName().toUpperCase().replace(" ", "").replace("-", ""))));

			params.setPlayRawText(params.getPlayRawText().replaceAll("complete to ([A-Z][a-z]+?) ([A-Z][a-z]+?) Jr",
					"complete to $1 $2"));
			params.setPlayRawText(params.getPlayRawText().replaceAll("complete to ([A-Z][a-z]+?) Jr ([A-Z][a-z]+?)",
					"complete to $1 $2"));

			params.setPlayRawText(params.getPlayRawText().replaceAll(
					"(pass ?((?:up the middle)|(?:to the (?:(?:right)|(?:left))))? ((?:in)?complete));",
					String.format("$1 intended for TEAM, %s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
							.getSeoName().toUpperCase().replace(" ", "").replace("-", ""))));

			params.setPlayRawText(params.getPlayRawText().replace(
					"UKY fumbled snap, LEVIS, Will pass to the right complete for 1 yard to EPPS, Isaiah to the CHA30 (SMITH, CaMiron)",
					"LEVIS, Will pass to the right complete for 1 yard to EPPS, Isaiah to the CHA30 (SMITH, CaMiron)"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"GEO fumbled snap, KNOOP, Tyler sacked for a loss of 6 yards at the GEO19 (SYKES, Jacob)",
					"KNOOP, Tyler sacked for a loss of 6 yards at the GEO19 (SYKES, Jacob)"));

			params.setPlayRawText(params.getPlayRawText().replace("pass attempt to failed",
					String.format("pass attempt to TEAM,%s failed",
							params.getTeamAbbrevDict().get(params.getPossessionTeam()).getSeoName().toUpperCase()
									.replace(" ", "").replace("-", ""))));
			params.setPlayRawText(params.getPlayRawText().replace("SHENKER, John Samuel", "SHENKER, JohnSamuel"));

			params.setPlayRawText(params.getPlayRawText()
					.replace(". The previous play is under review. The ruling on the field stands", ""));

			params.setPlayRawText(params.getPlayRawText().replace(
					"Widener,Jeff pass intercepted by Mascorro,Ysidro at Mascorro,Ysidro return 0 yards to the NAU20 (Daniels,Kevin), out of bounds",
					"Widener,Jeff pass intercepted by Mascorro,Ysidro at the NAU20 Mascorro,Ysidro return 0 yards to the NAU20 (Daniels,Kevin), out of bounds"));
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parsePatPassing(PbpServiceRequestPojo params) {
		try {
			String passString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("%s pass(:?ing)? attempt.+(?i)((?:is good)|(?:failed)|(?:successful)|(?:good$))",
							NcaaConstants.PLAYER_NAME_REGEX),
					9);
			String[] passStringArray = passString.split("\\|")[0].split("\\~");

			PbpPlayerStatPassingPojo passingStat = new PbpPlayerStatPassingPojo(
					pbpParsingUtils.formatName(passStringArray[0]));
			passingStat.setPassingDirection(PlayDirectionEnum.MISSING);
			passingStat.setPassingSack(0);
			passingStat.setPassingSpike(0);

			if ("is good".equalsIgnoreCase(passStringArray[8]) || "good".equalsIgnoreCase(passStringArray[8])
					|| "successful".equalsIgnoreCase(passStringArray[8])) {
				passingStat.setPassingYard(100 - params.getPlay().getPlayStartYard());
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(passingStat.getPassingYard());
				passingStat.setPassingTwoPointConversion(1);
				passingStat.setPassingCompletion(1);
				params.getPlay().getPlayResult().setPlayResultPoints(2);
				params.getPlay().getPlayResult().setPlayResultYardLine(100);
			} else if ("failed".equalsIgnoreCase(passStringArray[8])) {
				passingStat.setPassingTwoPointConversion(0);
			} else {
				String logInfo = String.format("Play text: %s", params.getPlayRawText());
				loggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("PAT passing did not evaluate");
			}

			params.getPlay().setPlayYardToGain(100 - params.getPlay().getPlayStartYard());
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
					.add(passingStat);
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parsePassingBasic(PbpServiceRequestPojo params) {
		try {
			String passString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"%s ((?:pass ?((?:up the middle)|(?:to the (?:(?:right)|(?:left))))? ((?:(?:in)?complete)|(?:intercepted)))|(sacked)|(spike))(,? (?i)spike)?",
					NcaaConstants.PLAYER_NAME_REGEX), 13);
			String[] passStringArray = passString.split("\\|")[0].split("\\~");
			PbpPlayerStatPassingPojo passingStat = new PbpPlayerStatPassingPojo(
					pbpParsingUtils.formatName(passStringArray[0]));

			if (!"null".equals(passStringArray[8])) {
				passingStat
						.setPassingDirection(PlayDirectionEnum.valueOf(passStringArray[8].split(" ")[2].toUpperCase()));
			} else {
				passingStat.setPassingDirection(PlayDirectionEnum.MISSING);
			}
			params.getPlay().getPlayResult().setPlayResultTurnover(false);

			if (!"null".equals(passStringArray[11]) || !"null".equals(passStringArray[12])) {
				passingStat.setPassingSpike(1);
			} else {
				passingStat.setPassingSpike(0);
			}

			passingStat.setPassingTwoPointConversion(0);
			passingStat.setPassingSack(0);
			if ("incomplete".equals(passStringArray[9]) || passingStat.getPassingSpike() == 1) {
				passingStat.setPassingYard(0);
				passingStat.setPassingCompletion(0);
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(0);
				params.getPlay().getPlayResult().setPlayResultYardLine(params.getPlay().getPlayStartYard());
			} else if ("interception".equals(passStringArray[9]) || "intercepted".equals(passStringArray[9])) {
				passingStat.setPassingYard(0);
				passingStat.setPassingCompletion(0);
				passingStat.setPassingInterception(1);
				params.getPlay().getPlayResult().setPlayResultTurnover(true);
				params.getPlay().getPlayResult().setPlayResultPossessionTeamId(params.getDefenseTeam());
			} else if ("sacked".equals(passStringArray[10])) {
				passingStat.setPassingSack(1);
				passingStat.setPassingCompletion(0);
				passingStat.setPassingYard(0);
			}

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
					.add(passingStat);
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseReceivingBasic(PbpServiceRequestPojo params) {
		try {
			if (parseReceivingBasicFilterHelper(params)) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("%s pass(:?ing)? attempt (?i)(failed)$", NcaaConstants.PLAYER_NAME_REGEX))) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingCompletion(0);
					return;
				}

				Integer twoPointConversion;
				Integer completion;
				String receiverName;
				String receivingString;
				String[] receivingStringArray;
				String firstMatchStr = String.format(
						"((?:(?:in)?complete)|(?:intercepted)).+?((?:to)|(?:intended for)|(?:dropped by)) %s(?:(?:$)|(?: (?:for))|(?: \\((.+)\\))|(?:,? QB hurr(?:(?:y)|(?:ied)))|(?: broken up)|(?: thrown)|(?:\\. )|(?:, dropped)|(?: to the)|(?: caught at)|(?:, [A-Z]))",
						NcaaConstants.PLAYER_NAME_REGEX);
				String secondMatchStr = String.format("yards? to %s (?:(?:caught)|(?:to))",
						NcaaConstants.PLAYER_NAME_REGEX);
				String thirdMatchStr = "pass .*?incomplete,? (?:(?:thrown to the )|(?: ?QB hurr(?:(?:y)|(?:ied)))|(?:\\((?:.+)\\)))";
				String fourthMatchStr = String.format("%s pass incomplete$", NcaaConstants.PLAYER_NAME_REGEX);
				String fifthMatchStr = String.format("pass(:?ing)? attempt to %s good$",
						NcaaConstants.PLAYER_NAME_REGEX);
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {
					receivingString = pbpParsingUtils.extractCustom(params.getPlayRawText(), firstMatchStr, 9);
					receivingStringArray = receivingString.split("\\|")[0].split("\\~");

					if ("complete".equals(receivingStringArray[0])) {
						completion = 1;
					} else {
						completion = 0;
					}
					receiverName = pbpParsingUtils.formatName(receivingStringArray[2]);
					twoPointConversion = 0;
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr)) {
					receivingString = pbpParsingUtils.extractCustom(params.getPlayRawText(), secondMatchStr, 7);
					receivingStringArray = receivingString.split("\\|")[0].split("\\~");
					completion = 1;
					receiverName = pbpParsingUtils.formatName(receivingStringArray[0]);
					twoPointConversion = 0;
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), thirdMatchStr)) {
					receivingString = pbpParsingUtils.extractCustom(params.getPlayRawText(), thirdMatchStr, 9);
					receivingStringArray = receivingString.split("\\|")[0].split("\\~");
					completion = 1;
					receiverName = pbpParsingUtils.formatName(receivingStringArray[0]);

					if ("is good".equalsIgnoreCase(receivingStringArray[7])) {
						twoPointConversion = 1;
					} else {
						twoPointConversion = 0;
					}
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), fourthMatchStr)) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingCompletion(1);
					receiverName = String.format("TEAM,%s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
							.getSeoName().replace(" ", "").replace("-", ""));
					completion = 1;
					twoPointConversion = 1;
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), fifthMatchStr)) {
					receivingString = pbpParsingUtils.extractCustom(params.getPlayRawText(), fifthMatchStr, 8);
					receivingStringArray = receivingString.split("\\|")[0].split("\\~");
					completion = 1;
					receiverName = pbpParsingUtils.formatName(receivingStringArray[1]);
					twoPointConversion = 1;
				} else {
					String logInfo = String.format(
							"Regex1: %s | Regex2: %s | Regex3: %s | Regex4: %s | Regex5: %s | Play Text: %s",
							firstMatchStr, secondMatchStr, thirdMatchStr, fourthMatchStr, fifthMatchStr,
							params.getPlayRawText());
					loggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Receiving parsing did not match regex");
				}

				PbpPlayerStatReceivingPojo receiverStat = new PbpPlayerStatReceivingPojo(receiverName, completion,
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
								.get(0).getPassingYard());
				receiverStat.setReceivingTwoPointConversion(twoPointConversion);

				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.add(receiverStat);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingCompletion(completion);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private boolean parseReceivingBasicFilterHelper(PbpServiceRequestPojo params) {
		try {
			String firstMatchStr = String.format(
					"attempt to %s ((?:is good)|(?:failed))(; conversion is no good \\[down at the%s\\])?",
					NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX);
			String secondMatchStr = String.format("%s pass(:?ing)? attempt (?i)(successful)$",
					NcaaConstants.PLAYER_NAME_REGEX);
			return (!(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
					.get(0).getPassingSack() == 1
					|| params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).getPassingSpike() == 1
					|| params.getPlayRawText().contains("intercepted")
					|| params.getPlayRawText().contains("incomplete broken up by")
					|| (pbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)
							&& !params.getPlayRawText().contains("intended"))
					|| (pbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr))));
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
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
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parsePassingYards(PbpServiceRequestPojo params) {
		try {
			String yardsString;
			String[] yardsStringArray;
			Integer yard;
			String firstMatchStr = " complete ?.+? ?for( loss of)? (((-?\\d{1,3}) yards?)|(no gain))( loss)?()";
			String secondMatchStr = " sacked for (?:a )?loss of (-?\\d{1,3}) yards?";
			String thirdMatchStr = String.format("; conversion is no good \\[down at the%s\\]",
					NcaaConstants.TEAM_YARD_REGEX);
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {
				yardsString = pbpParsingUtils.extractCustom(params.getPlayRawText(), firstMatchStr, 6);
				yardsStringArray = yardsString.split("\\|")[0].split("\\~");

				if ("no gain".equals(yardsStringArray[4])) {
					yard = 0;
				} else {
					yard = Integer.valueOf(yardsStringArray[3]);
				}

				if (" loss of".equals(yardsStringArray[0]) || " loss".equals(yardsStringArray[5])) {
					yard *= -1;
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.setReceivingYard(yard);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingYard(yard);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingSackYard(0);
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(yard);

			} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr)) {
				yardsString = pbpParsingUtils.extractCustom(params.getPlayRawText(), secondMatchStr, 1);
				yardsStringArray = yardsString.split("\\|")[0].split("\\~");
				yard = -1 * Integer.valueOf(yardsStringArray[0]);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingSackYard(yard);
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(yard);
			} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), thirdMatchStr)) {
				yardsString = pbpParsingUtils.extractCustom(params.getPlayRawText(), thirdMatchStr, 1);
				yardsStringArray = yardsString.split("\\|")[0].split("\\~");
				yard = pbpParsingUtils.formatYardLine(yardsStringArray[0], params.getPossessionTeam(),
						params.getDefenseTeam(), params.getTeamAbbrevDict()) - params.getPlay().getPlayStartYard();
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(yard);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingYard(yard);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.setReceivingYard(yard);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingSackYard(0);
			} else {
				if (params.getPlayRawText().contains(" yard") && !params.getPlayRawText().contains(" intercept")) {
					String logInfo = String.format("Regex1: %s | Regex2: %s | Regex3: %s | Play Text: %s",
							firstMatchStr, secondMatchStr, thirdMatchStr, params.getPlayRawText());
					loggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Passing yards parsing did not match regex");
				} else {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingSackYard(0);
					if (Objects.isNull(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
							.getPassingStat().get(0).getPassingYard())) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
								.get(0).setPassingYard(0);
					}
					params.getPlay().getPlayResult().updatePenaltyPlayResultYard(0);
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseYardsAfterCatch(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("ADVANCE")) {
				String matchStr = String.format("caught at the%s and advanced to the%s", NcaaConstants.TEAM_YARD_REGEX,
						NcaaConstants.TEAM_YARD_REGEX);
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					String yacString = pbpParsingUtils.extractCustom(params.getPlayRawText(), matchStr, 2);
					String[] yacStringArray = yacString.split("\\|")[0].split("\\~");
					Integer formattedStartYard = pbpParsingUtils.formatYardLine(yacStringArray[0],
							params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
					Integer formattedEndYard = pbpParsingUtils.formatYardLine(yacStringArray[1],
							params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
					Integer formattedThrownYard = formattedStartYard - params.getPlay().getPlayStartYard();
					Integer yardsAfterCatch = formattedEndYard - formattedStartYard;
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).setReceivingYardAfterCatch(yardsAfterCatch);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingYardAfterCatch(yardsAfterCatch);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingYardThrownTo(formattedThrownYard);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingAirLessNeeded(formattedThrownYard - params.getPlay().getPlayYardToGain());
				} else {
					String logInfo = String.format("Regex: %s | Play Text: %s", matchStr, params.getPlayRawText());
					loggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Yards after catch parsing did not match regex");
				}
			} else {
				// Nothing needed here right now
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

}
