package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.logging.log4j.ThreadContext;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

public final class PbpDefenseParseService {
	private static final String PLAY_TEXT_S_REGEX_S = "Play text: %s | Regex: %s";
	private static final String PLAY_TEXT_S_RETURN_YARDS_S = "Play text: %s | Return Yards: %s";
	private static final String PLAY_TEXT_S = "Play text: %s";
	private static final String MISSING_PLAY_CALL_TYPE_FOR_CONDITIONAL_EVALUATION = "Missing play call type for conditional evaluation";

	private static final String PLAY_CALL_TYPE_S_PLAY_TEXT_S = "Play Call Type: %s | Play text: %s";

	// private static static constructor to prevent instantiation
	private PbpDefenseParseService() {
		throw new UnsupportedOperationException();
	}

	public static void parseDefense(PbpServiceRequestPojo params) {
		try {
			if (PlayTypeEnum.OFFENSE.equals(params.getPlay().getPlayType())
					&& !PlayCallTypeEnum.FG.equals(params.getPlay().getPlayCallType())) {
				params.getPlay().setHavocDb(false);
				params.getPlay().setHavocFront(false);
				params.getPlay().setHavoc(false);
			}

			parseTackles(params);
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				parseSack(params);
				parseInterception(params);
				parseBreakup(params);
				parseQbHurry(params);
			}
			parseFumbleRecovery(params);
			parseFumble(params);
			parseFumbleForced(params);
			parseTurnoverReturn(params);
			parseSafety(params);
			removeTurnoverTackles(params);

			if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
				String defTeam = params.getDefenseTeam();
				List<PbpPlayerStatDefenseProductionPojo> defenseInfo = params.getPlay().getPlayerStat().get(defTeam)
						.getDefense().getDefenseProduction();

				LoggingUtils.logInfo(
						String.format("Defense Team: %s", params.getTeamAbbrevDict().get(defTeam).getShortname()));
				if (!defenseInfo.isEmpty()) {
					for (PbpPlayerStatDefenseProductionPojo def : defenseInfo) {
						LoggingUtils.logInfo(String.format("Defense Info: %s", def.toString()));
					}
				} else {
					LoggingUtils.logInfo("Defense: None");
				}
			}

		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void parseTackles(PbpServiceRequestPojo params) {
		try {
			boolean solo = false;
			if (Objects.isNull(params.getPlayTackles())) {
				params.setPlayTackles(new String[0]);
			} else if (params.getPlayTackles().length != 0) {
				if (params.getPlayTackles().length == 1) {
					solo = true;
				}
				for (String tackleName : params.getPlayTackles()) {
					parseTacklesHelper(params, tackleName, solo);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parseTacklesHelper(PbpServiceRequestPojo params, String tackleName, boolean solo) {
		try {
			Integer yards;
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				yards = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findPuntReturner().getPuntReturnYard();
				if (solo) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findPuntCoverageByName(tackleName).applyTackleSolo(tackleName, yards);
				} else {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findPuntCoverageByName(tackleName).applyTackle(tackleName, yards);
				}
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				yards = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findKickoffReturner().getKickReturnYard();
				if (solo) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findKickCoverageByName(tackleName).applyTackleSolo(tackleName, yards);
				} else {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findKickCoverageByName(tackleName).applyTackle(tackleName, yards);
				}
			} else {
				yards = params.getPlay().getPlayResult().getPlayResultYard();
				if (solo) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(tackleName).applyTackleSolo(tackleName, yards);
				} else {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(tackleName).applyTackle(tackleName, yards);
				}
				if (yards < 0) {
					params.getPlay().setHavocFront(true);
					params.getPlay().setHavoc(true);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseBreakup(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("DEFENDED")
					|| params.getPlayRawText().toUpperCase().contains("BROKEN UP")) {
				String matchStr = String.format("broken up by %s", NcaaConstants.PLAYER_NAME_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					String pbuString = PbpParsingUtils.extractCustom(params.getPlayRawText(), matchStr, 7);
					String[] pbuStringArray = pbuString.split("\\|")[0].split("\\~");
					String pbuName = PbpParsingUtils.formatName(pbuStringArray[0]);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(pbuName).setPassBreakUp(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingBreakup(1);
					params.getPlay().setHavocDb(true);
					params.getPlay().setHavoc(true);
				} else {
					String logInfo = String.format(PLAY_TEXT_S_REGEX_S, params.getPlayRawText(), matchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Pass breakup did not evaluate regex");
				}
			} else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
					.get(0).getPassingCompletion() != 1
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).getPassingInterception() != 1
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).getPassingSack() != 1
					&& params.getPlayTackles().length > 0) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingBreakup(1);
				for (PbpPlayerStatDefenseProductionPojo def : params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getDefense().getDefenseProduction()) {
					def.clearTackles();
					def.setPassBreakUp(1);
					params.getPlay().setHavocDb(true);
					params.getPlay().setHavoc(true);
				}

			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingBreakup(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseQbHurry(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("HURRY")
					|| params.getPlayRawText().toUpperCase().contains("HURRIED")) {
				String matchStr = String.format("QB hurr(?:(?:y)|(?:ied)) by %s", NcaaConstants.PLAYER_NAME_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					String hurryString = PbpParsingUtils.extractCustom(params.getPlayRawText(), matchStr, 7);
					String[] hurryStringArray = hurryString.split("\\|")[0].split("\\~");
					String hurryName = PbpParsingUtils.formatName(hurryStringArray[0]);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(hurryName).setQuarterbackHurry(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingHurry(1);
				} else {
					String logInfo = String.format(PLAY_TEXT_S_REGEX_S, params.getPlayRawText(), matchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("QB hurry did not evaluate");
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingHurry(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseSack(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("SACK")) {
				String matchStr = String.format("%s sacked for (?:(?:(?:a )?loss of \\d{1,3} yards?)|(?:no gain))",
						NcaaConstants.PLAYER_NAME_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					for (PbpPlayerStatDefenseProductionPojo tackle : params.getPlay().getPlayerStat()
							.get(params.getDefenseTeam()).getDefense().getDefenseProduction()) {
						if (tackle.getTackleTotal() == 0) {
							continue;
						}
						tackle.setSack(1.0);
					}
				} else {
					String logInfo = String.format(PLAY_TEXT_S_REGEX_S, params.getPlayRawText(), matchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Sack did not evaluate");
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingSack(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseInterception(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("INTERCEPT")) {
				String matchStr = String.format("intercepted by %s at(?: the)?(%s|( , Touchback))",
						NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					Integer formattedThrownYard;
					String interceptionString = PbpParsingUtils.extractCustom(params.getPlayRawText(), matchStr, 10);
					String[] interceptionStringArray = interceptionString.split("\\|")[0].split("\\~");
					String interceptionName = PbpParsingUtils.formatName(interceptionStringArray[0]);
					if (" , Touchback".equals(interceptionStringArray[7])) {
						formattedThrownYard = 100 - params.getPlay().getPlayStartYard();
					} else {
						Integer formattedYardline = PbpParsingUtils.formatYardLine(interceptionStringArray[7],
								params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
						formattedThrownYard = formattedYardline - params.getPlay().getPlayStartYard();
					}
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(interceptionName).setInterception(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingInterception(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingYardThrownTo(formattedThrownYard);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingAirLessNeeded(formattedThrownYard - params.getPlay().getPlayYardToGain());

					params.getPlay().getPlayResult().setPlayResultTurnover(true);
					params.getPlay().getPlayResult().setPlayResultPossessionTeamId(params.getDefenseTeam());

					params.getPlay().setHavocDb(true);
					params.getPlay().setHavoc(true);
				} else {
					String logInfo = String.format(PLAY_TEXT_S_REGEX_S, params.getPlayRawText(), matchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Interception did not evaluate");
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingInterception(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingInterceptionTouchdown(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingInterceptionYard(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleForced(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FORCED")) {
				String firstMatchStr = String.format(" (to the|at)%s,? (fumble )?forced by %s",
						NcaaConstants.TEAM_YARD_REGEX, NcaaConstants.PLAYER_NAME_REGEX);
				String secondMatchStr = String.format("fumble on the sack \\(forced by %s\\)",
						NcaaConstants.PLAYER_NAME_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {
					parseFumbleForcedHelper(params, firstMatchStr);
					params.getPlay().setHavocFront(true);
					params.getPlay().setHavoc(true);
				} else if (PbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr)) {
					String fumbleForceString = PbpParsingUtils.extractCustom(params.getPlayRawText(), secondMatchStr,
							7);
					String[] fumbleForceStringArray = fumbleForceString.split("\\|")[0].split("\\~");
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(PbpParsingUtils.formatName(fumbleForceStringArray[0]))
							.setFumbleForced(1);
					params.getPlay().setHavocFront(true);
					params.getPlay().setHavoc(true);
				} else {
					String logInfo = String.format("Play text: %s | First Regex: %s | Second Regex: %s",
							params.getPlayRawText(), firstMatchStr, secondMatchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Forced Fumble did not evaluate");
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleForcedHelper(PbpServiceRequestPojo params, String firstMatchStr) {
		try {
			String fumbleForceString = PbpParsingUtils.extractCustom(params.getPlayRawText(), firstMatchStr, 10);
			String[] fumbleForceStringArray = fumbleForceString.split("\\|")[0].split("\\~");
			String formatedFumbleName = PbpParsingUtils.formatName(fumbleForceStringArray[3]);
			if (Objects.isNull(params.getPlayTackles())
					|| Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
				params.setPlayTackles(new String[] { formatedFumbleName });
				parseTackles(params);
				if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
							.findPuntCoverageByName(formatedFumbleName).setFumbleForced(1);
				} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
							.findKickCoverageByName(formatedFumbleName).setFumbleForced(1);
				} else {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(formatedFumbleName).setFumbleForced(1);
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
						.findDefenseProductionByName(formatedFumbleName).addForcedFumbleTackle();
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleRecovery(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("RECOVER")) {
				String matchStr = String.format("recovered by ([A-Z]*?-?[aA-zZ]{2,3}).? %s(?: at)?(?: the)?(%s)?",
						NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {

					String returnerFumbleRecoverString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
							matchStr, 10);
					String[] returnerFumbleRecoverStringArray = returnerFumbleRecoverString.split("\\|");
					String[] returnerFumbleInfo = returnerFumbleRecoverStringArray[returnerFumbleRecoverStringArray.length
							- 1].split("\\~");
					String abbrev = returnerFumbleInfo[0];
					String recoverName = PbpParsingUtils.formatName(returnerFumbleInfo[1]);

					parseFumbleRecoveryHelperPlayResultYard(params, returnerFumbleInfo);
					parseFumbleRecoveryHelper(params, recoverName, abbrev);
				} else {
					String logInfo = String.format(PLAY_TEXT_S_REGEX_S, params.getPlayRawText(), matchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Fumble recovery did not evaluate");
				}
			} else {
				// Nothing needed here for now
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleRecoveryHelperPlayResultYard(PbpServiceRequestPojo params,
			String[] returnerFumbleInfo) {
		try {
			if (!"null".equals(returnerFumbleInfo[9]) && params.getPlay().getPlayType() != PlayTypeEnum.PUNT
					&& params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF) {
				Integer recoveredYardLine = PbpParsingUtils.formatYardLine(returnerFumbleInfo[9],
						params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
				Integer defPenYard;
				Integer offPenYard;
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()) {
					defPenYard = params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().get(0)
							.getPenaltyYards();
				} else {
					defPenYard = 0;
				}
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()) {
					offPenYard = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().get(0)
							.getPenaltyYards();
				} else {
					offPenYard = 0;
				}
				Integer yardDiff = recoveredYardLine
						- (params.getPlay().getPlayStartYard() + params.getPlay().getPlayResult().getPlayResultYard())
						+ defPenYard - offPenYard;
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(yardDiff);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parseFumbleRecoveryHelper(PbpServiceRequestPojo params, String recoverName, String abbrev) {
		try {
			boolean turnover = !PbpParsingUtils.resolvePossesionTeam(abbrev, params.getPossessionTeam(),
					params.getDefenseTeam(), params.getTeamAbbrevDict());

			if (turnover) {
				parseFumbleRecoveryTurnoverHelper(params, recoverName);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT && params.getPlay().getPlayerStat()
					.get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0).getPuntBlocked() == 1) {
				parseFumbleRecoveryPuntBlockHelper(params, recoverName);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				// Nothing needed here for now
			}
			params.getPlay().getPlayResult().setPlayResultTurnover(turnover);
			if (Boolean.TRUE.equals(turnover)) {
				params.getPlay().getPlayResult().setPlayResultPossessionTeamId(params.getDefenseTeam());
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleRecoveryTurnoverHelper(PbpServiceRequestPojo params, String recoverName) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findPuntCoverageByName(recoverName).setFumbleRecovered(1);
				params.getPlay().getPlayResult()
						.setPlayResultYardLine(100 - params.getPlay().getPlayResult().getPlayResultYardLine());
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickCoverageByName(recoverName).setFumbleRecovered(1);
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
						.findDefenseProductionByName(recoverName).setFumbleRecovered(1);
				if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
						.findDefenseProductionByName(recoverName).getSack() == 1) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(recoverName).setFumbleForced(1);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleRecoveryPuntBlockHelper(PbpServiceRequestPojo params, String recoverName) {
		try {
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
					.findPuntReturnByName(recoverName).applyNoPuntReturnFumble();
			String blockRecoverString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("recovered by ([A-Z]*?-?[aA-zZ]{2,3}).? %s at the%s", NcaaConstants.PLAYER_NAME_REGEX,
							NcaaConstants.TEAM_YARD_REGEX),
					9);
			String[] blockRecoverStringArray = blockRecoverString.split("\\~");
			Integer puntReturnStartYard = PbpParsingUtils.formatYardLine(blockRecoverStringArray[8],
					params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
					.findPuntReturnByName(recoverName).setPuntReturnStartYard(puntReturnStartYard);
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumble(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FUMBLE")
					|| params.getPlayRawText().toUpperCase().contains(" MUFFED")) {
				String formatFumbleName;

				String firstMatchStr = String.format(
						"((?:(?:fumbled? by %s ?)| %s fumbled at)|((fumble on the sack)|(fumbled snap)))",
						NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.PLAYER_NAME_REGEX);
				String secondMatchStr = String.format("(?:return)? muffed by %s at(?: the)?%s",
						NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {
					formatFumbleName = parseFumbleFirstMatchHelper(params, firstMatchStr);
				} else if (PbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr)) {
					String returnerMuffString = PbpParsingUtils.extractCustom(params.getPlayRawText(), secondMatchStr,
							8);
					formatFumbleName = PbpParsingUtils.formatName(returnerMuffString.split("\\~")[0]);
				} else {
					String logInfo = String.format("Play text: %s | Regex1: %s | Regex2: %s", params.getPlayRawText(),
							firstMatchStr, secondMatchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Fumble did not evaluate");

				}

				parseFumbleHelper(params, formatFumbleName);
				if (PbpParsingUtils.evalMatch(params.getPlay().getPlayText(), "Touchback.$")) {
					params.getPlay().getPlayResult().setPlayResultTurnover(true);
					params.getPlay().getPlayResult().setPlayResultPossessionTeamId(params.getDefenseTeam());
				}

			} else {
				parseNoFumbleHelper(params);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static String parseFumbleFirstMatchHelper(PbpServiceRequestPojo params, String firstMatchStr) {
		try {
			String fumbleString = PbpParsingUtils.extractCustom(params.getPlayRawText(), firstMatchStr, 18);
			String[] fumbleStringArray = fumbleString.split("\\|")[0].split("\\~");
			fumbleStringArray[1] = fumbleStringArray[1].replace(" recovered", "");
			fumbleStringArray[1] = fumbleStringArray[1].replace(" at", "");

			String formatFumbleName;
			String fumbleName;
			if (!"null".equals(fumbleStringArray[1])) {
				fumbleName = fumbleStringArray[1];
				formatFumbleName = PbpParsingUtils.formatName(fumbleName);
			} else if (!"null".equals(fumbleStringArray[8])) {
				fumbleName = fumbleStringArray[8];
				formatFumbleName = PbpParsingUtils.formatName(fumbleName);
			} else if ("fumble on the sack".equals(fumbleStringArray[15])) {
				formatFumbleName = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
						.getPassingStat().get(0).getPlayerName();
			} else if ("fumbled snap".equals(fumbleStringArray[15])) {
				formatFumbleName = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
						.getRushingStat().get(0).getPlayerName();
			} else {
				String logInfo = String.format(PLAY_TEXT_S, params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Fumble matched regex but failed conditional processing");
			}
			return formatFumbleName;
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());

		}
	}

	private static void parseFumbleHelper(PbpServiceRequestPojo params, String formatFumbleName) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				parseFumbleHelperPass(params, formatFumbleName);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				parseFumbleHelperRun(params, formatFumbleName);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				parseFumbleHelperPunt(params, formatFumbleName);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				parseFumbleHelperKickoff(params, formatFumbleName);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				// Nothing needed here for now
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

	private static void parseFumbleHelperPass(PbpServiceRequestPojo params, String formatFumbleName) {
		try {
			if (formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
					.getPassingStat().get(0).getPlayerName())) {
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).applyNoReceivingFumble();
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.applyPassingFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
			} else if (formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getPossessionTeam())
					.getOffense().getReceivingStat().get(0).getPlayerName())) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.applyNoPassingFumble();
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat().get(0)
						.applyReceivingFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
			} else {
				String logInfo = String.format(
						"Play text: %s | Fumble Player: %s | Passing Player: %s | Receiving Player: %s",
						params.getPlayRawText(), formatFumbleName,
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
								.get(0).getPlayerName(),
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
								.get(0).getPlayerName());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Passing fumble does not match passing player");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleHelperRun(PbpServiceRequestPojo params, String formatFumbleName) {
		try {
			if (!formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
					.getRushingStat().get(0).getPlayerName())) {
				String logInfo = String.format("Play text: %s | Fumble Player: %s | Rushing Player: %s",
						params.getPlayRawText(), formatFumbleName, params.getPlay().getPlayerStat()
								.get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getPlayerName());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Rushing fumble does not match rushing player");
			}
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
					.applyRushingFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseFumbleHelperPunt(PbpServiceRequestPojo params, String formatFumbleName) {
		try {
			if (!formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getPossessionTeam())
					.getSpecialTeam().findPuntReturner().getPlayerName())) {
				String logInfo = String.format("Play text: %s | Fumble Player: %s | Punt Return Player: %s",
						params.getPlayRawText(), formatFumbleName, params.getPlay().getPlayerStat()
								.get(params.getDefenseTeam()).getSpecialTeam().findPuntReturner().getPlayerName());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Punt fumble does not match punt return player");
			}
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().findPuntReturner()
					.applyPuntReturnFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parseFumbleHelperKickoff(PbpServiceRequestPojo params, String formatFumbleName) {
		try {
			if (!formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
					.findKickoffReturner().getPlayerName())) {
				String logInfo = String.format("Play text: %s | Fumble Player: %s | Kick Return Player: %s",
						params.getPlayRawText(), formatFumbleName, params.getPlay().getPlayerStat()
								.get(params.getDefenseTeam()).getSpecialTeam().findKickoffReturner().getPlayerName());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Kickoff fumble does not match kick return player");
			}
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findKickoffReturner()
					.applyKickReturnFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseNoFumbleHelper(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.applyNoRushingFumble();
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.applyNoPassingFumble();
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).applyNoReceivingFumble();
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				for (PbpPlayerStatPuntReturnPojo puntReturner : params.getPlay().getPlayerStat()
						.get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()) {
					puntReturner.applyNoPuntReturnFumble();
				}
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				for (PbpPlayerStatKickReturnPojo kickReturner : params.getPlay().getPlayerStat()
						.get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()) {
					kickReturner.applyNoKickReturnFumble();
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				// Nothing needed here for now
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

	private static void parseTurnoverReturn(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("RETURN")) {
				String turnoverReturnString;
				Integer turnoverReturnYards;
				Integer returnStartYard;

				String firstMatchStr = String.format(
						"at(?: the)?%s,?.+ return(?:ed)?(?: for)? (-?\\d+) yards? to the%s",
						NcaaConstants.TEAM_YARD_REGEX, NcaaConstants.TEAM_YARD_REGEX);
				String secondMatchStr = String.format("at%s, return(?:ed)? by %s (\\d{1,3}) yards? to the ",
						NcaaConstants.TEAM_YARD_REGEX, NcaaConstants.PLAYER_NAME_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {

					turnoverReturnString = PbpParsingUtils.extractCustom(params.getPlayRawText(), firstMatchStr, 3);
					turnoverReturnYards = Integer.valueOf(turnoverReturnString.split("\\~")[1]);
					returnStartYard = PbpParsingUtils.formatYardLine(turnoverReturnString.split("\\~")[0],
							params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
				} else if (PbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr)) {
					turnoverReturnString = PbpParsingUtils.extractCustom(params.getPlayRawText(), secondMatchStr, 9);
					turnoverReturnYards = Integer.valueOf(turnoverReturnString.split("\\~")[8]);
					returnStartYard = PbpParsingUtils.formatYardLine(turnoverReturnString.split("\\~")[0],
							params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
				} else {
					turnoverReturnYards = 0;
					returnStartYard = 0;
				}

				if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
					parseTurnoverReturnTurnoverHelper(params, turnoverReturnYards, returnStartYard);
				} else {
					parseTurnoverReturnNoTurnoverHelper(params, turnoverReturnYards, returnStartYard);
				}
			} else {
				parseTurnoverReturnNoReturnHelper(params);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseTurnoverReturnTurnoverHelper(PbpServiceRequestPojo params, Integer turnoverReturnYards,
			Integer returnStartYard) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findPuntCoverageWithTurnover().applyReturnYards(turnoverReturnYards);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickCoverageWithTurnover().applyReturnYards(turnoverReturnYards);
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().findDefenseWithTurnover()
						.applyReturnYards(turnoverReturnYards);
				if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().findDefenseWithTurnover()
						.getInterception() == 1) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingInterceptionYard(returnStartYard);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseTurnoverReturnNoTurnoverHelper(PbpServiceRequestPojo params, Integer turnoverReturnYards,
			Integer returnStartYard) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findPuntReturnWithFumble().isEmpty() && turnoverReturnYards > 0) {
					String logInfo = String.format(PLAY_TEXT_S_RETURN_YARDS_S, params.getPlayRawText(),
							turnoverReturnYards);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Punt return fumble, recovery, advancement not supported");
				} else if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting()
						.get(0).getPuntBlocked() == 1) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().findPuntReturner()
							.applyBlockReturn(turnoverReturnYards, returnStartYard);
				}
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findKickReturnWithFumble().isEmpty() && turnoverReturnYards > 0) {
					String logInfo = String.format(PLAY_TEXT_S_RETURN_YARDS_S, params.getPlayRawText(),
							turnoverReturnYards);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Kick return fumble, recovery, advancement not supported");
				}
			} else {
				if (turnoverReturnYards != 0) {
					if (PlayCallTypeEnum.RUN.equals(params.getPlay().getPlayCallType())) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).addRushingYard(turnoverReturnYards);
					} else {
						String logInfo = String.format(PLAY_TEXT_S_RETURN_YARDS_S, params.getPlayRawText(),
								turnoverReturnYards);
						LoggingUtils.logInfo(logInfo);
						throw new IllegalArgumentException(
								"Conditions for fumble recovery but not a turnover not supported");
					}
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseTurnoverReturnNoReturnHelper(PbpServiceRequestPojo params) {
		try {
			if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
				if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findPuntCoverageWithTurnover().applyReturnYards(0);
				} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findKickCoverageWithTurnover().applyReturnYards(0);
				} else if (Objects.isNull(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
						.findDefenseWithTurnover()) && PbpParsingUtils.evalMatch(params.getPlay().getPlayText(), "Touchback.$")) {
					params.getPlay().getPlayResult().setPlayResultYardLine(80);
				} else {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().findDefenseWithTurnover()
							.applyReturnYards(0);
					if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseWithTurnover().getInterception() == 1) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
								.get(0).setPassingInterceptionYard(0);
					}
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseSafety(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("SAFETY")) {
				String matchStr = "([A-Z]*?-?[aA-zZ]{2,3}).? (?i)safety( |,|$)";
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					params.getPlay().getPlayResult().setPlayResultPoints(-2);
					String safetyString = PbpParsingUtils.extractCustom(params.getPlayRawText(), matchStr, 2);
					String[] safetyStringArray = safetyString.split("\\|")[0].split("\\~");

					if (Objects.isNull(params.getPlayTackles())) {
						String safetyName = PbpParsingUtils.formatName(String.format("TEAM, %s", safetyStringArray[0]));
						params.setPlayTackles(new String[] { safetyName });
						parseTackles(params);
					}
					parseSafetyHelper(params, 1);
				} else if (PbpParsingUtils.evalMatch(params.getPlayRawText(), "(?i)safety\\)")) {
					parseSafetyHelper(params, 0);
				} else {
					String logInfo = String.format(PLAY_TEXT_S_REGEX_S, params.getPlayRawText(), matchStr);
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Safety did not evaluate");
				}
			} else {
				parseSafetyHelper(params, 0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseSafetyHelper(PbpServiceRequestPojo params, Integer safety) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				parseSafetyHelperPunt(params, safety);
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				parseSafetyHelperKickoff(params, safety);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				parseSafetyHelperOffense(params, safety);
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				parseSafetyHelperPatFieldGoal(params, safety);
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

	private static void parseSafetyHelperPunt(PbpServiceRequestPojo params, Integer safety) {
		try {
			if (safety == 1 && params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
					.getPunting().get(0).getPuntBlocked() == 1) {
				String logInfo = String.format(PLAY_TEXT_S, params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Punt safety handling not supported");
			}
			for (PbpPlayerStatDefenseProductionPojo puntCov : params.getPlay().getPlayerStat()
					.get(params.getDefenseTeam()).getSpecialTeam().getPuntCoverage()) {
				parseSafetyDefenseHelper(params, puntCov, safety);
			}
			for (PbpPlayerStatPuntReturnPojo puntReturner : params.getPlay().getPlayerStat()
					.get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()) {
				puntReturner.setPuntReturnSafety(safety);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseSafetyHelperKickoff(PbpServiceRequestPojo params, Integer safety) {
		try {
			for (PbpPlayerStatDefenseProductionPojo kickCov : params.getPlay().getPlayerStat()
					.get(params.getDefenseTeam()).getSpecialTeam().getKickCoverage()) {
				parseSafetyDefenseHelper(params, kickCov, safety);
			}
			if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
					.isEmpty()) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
						.setKickReturnSafety(safety);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseSafetyHelperOffense(PbpServiceRequestPojo params, Integer safety) {
		try {
			for (PbpPlayerStatDefenseProductionPojo def : params.getPlay().getPlayerStat().get(params.getDefenseTeam())
					.getDefense().getDefenseProduction()) {
				parseSafetyDefenseHelper(params, def, safety);
			}
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingSafety(safety);
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingSafety(safety);
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).setReceivingSafety(safety);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseSafetyHelperPatFieldGoal(PbpServiceRequestPojo params, Integer safety) {
		try {
			if (safety == 1) {
				String logInfo = String.format(PLAY_TEXT_S, params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("FG/PAT safety is not supported");
			} else {
				for (PbpPlayerStatDefenseProductionPojo def : params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getDefense().getDefenseProduction()) {
					parseSafetyDefenseHelper(params, def, safety);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void parseSafetyDefenseHelper(PbpServiceRequestPojo params, PbpPlayerStatDefenseProductionPojo def,
			Integer safety) {
		try {
			if (safety == 1) {
				if (def.getTackleTotal() == 1) {
					def.setSafety(1);
				} else {
					def.setSafety(0);
				}
			} else {
				def.setSafety(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private static void removeTurnoverTackles(PbpServiceRequestPojo params) {
		try {
			if (Boolean.TRUE.equals(params.getPlay().getPlayResult().isPlayResultTurnover())) {
				List<PbpPlayerStatDefenseProductionPojo> trueDefenseTackles = params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getDefense().getDefenseProduction().stream()
						.filter(d -> !(d.getTackleTotal() > 0 && d.getFumbleForced() == 0))
						.collect(Collectors.toList());
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
						.setDefenseProduction(trueDefenseTackles);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());

		}
	}

}
