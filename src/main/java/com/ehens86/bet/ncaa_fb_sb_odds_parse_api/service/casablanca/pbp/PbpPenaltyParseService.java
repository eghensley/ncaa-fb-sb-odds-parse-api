package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PenaltyEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

public final class PbpPenaltyParseService {
	private static final String MULTIPLE_PENALTIES_FOUND_NEED_TO_HANDLE_THIS = "Multiple penalties found, need to handle this.";

	private static final String PENALTY_COUNT_S_PLAY_TEXT_S = "Penalty Count: %s | Play Text: %s";

	private static final String FIRST_DOWN = "1ST DOWN";

	private static final String INELIGIBLE_RECEIVER_DOWNFIELD = "ineligible receiver downfield";

	private static final String PENALTY = "PENALTY";

	private static final String I_PENALTY = ",?;?\\.? ?(?i)Penalty(.*)";

	// private static static constructor to prevent instantiation
	private PbpPenaltyParseService() {
		throw new UnsupportedOperationException();
	}

	public static boolean parsePenalty(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains(PENALTY)
					|| (PbpParsingUtils.evalMatch(params.getPlayRawText(), "kickoff.+out of bounds")
							&& !params.getPlayRawText().toUpperCase().contains("RETURN"))) {

				params.setPlayRawText(
						params.getPlayRawText().replace("UNS: Unsportsmanlike Conduct", "Unsportsmanlike Conduct"));
				params.setPlayRawText(params.getPlayRawText().replace("Offsides", "Offside"));
				params.setPlayRawText(params.getPlayRawText().replace("Block Below Waist", "Block Below the Waist"));

				params.setPlayRawText(params.getPlayRawText().replace("( ", "("));
				params.setPlayRawText(params.getPlayRawText().replace(" )", ")"));

				params.setPlayRawText(
						params.getPlayRawText().replace("UNR: Unnecessary Roughness", "Unnecessary Roughness"));
				params.setPlayRawText(
						params.getPlayRawText().replace(" PENALTY MAI Ineligible Receiver Downfield on Pass  enforced",
								" PENALTY MAI Ineligible Receiver Downfield on TEAM, MAI enforced"));
				params.setPlayRawText(
						params.getPlayRawText().replace("12 men on the field", "too many men on the field"));
				params.setPlayRawText(
						params.getPlayRawText().replace("Kick Catch Interference", "Fair Catch Interference"));
				params.setPlayRawText(
						params.getPlayRawText().replace("ineligible downfield on pass", INELIGIBLE_RECEIVER_DOWNFIELD));
				params.setPlayRawText(
						params.getPlayRawText().replace("Ineligible Downfield on Pass", INELIGIBLE_RECEIVER_DOWNFIELD));
				params.setPlayRawText(params.getPlayRawText().replace("  ", " "));
				params.setPlayRawText(params.getPlayRawText().replace("roughing passer", "roughing the passer"));

				if (params.getPlayRawText().toUpperCase().contains("AFTER THE PLAY")) {
					throw new IllegalArgumentException("HANDLE AFTER THE PLAY PENALTY");
				}
				boolean declined = handleDeclined(params);
				boolean offsetting = handleOffsetting(params);
				handleKickoffOutOfBounds(params);

				dispatchPenaltyProcessing(params, declined, offsetting);

				updatePlayTextString(params);

				params.setPlayTackles(PbpParsingUtils.extractTackle(params.getPlayRawText()));

				if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()
						&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()
						&& !declined && !offsetting) {
					String logInfo = String.format("No penalty matched.  Play text: %s", params.getPlayRawText());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Missing penalty");
				}

				return (Boolean.TRUE.equals(params.getPlay().getNoPlayPenalty()) && (declined || offsetting));

			} else {
				params.getPlay().setNoPlayPenalty(false);
				return false;
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void dispatchPenaltyProcessing(PbpServiceRequestPojo params, boolean declined, boolean offsetting) {
		try {
			if (!declined) {
				String[] penaltyArray = new String[] { "kick catching interference", "player disqualification", "hd", "do", "running into the kicker",
						"hl", "illegal snap", "encroachment", "illegal procedure", "fair catch interference",
						"illegal forward pass", "illegal motion", "roughing the kicker", "targeting",
						"block below the waist", "illegal block", "too many men on the field",
						"substitution infraction", "face mask", "illegal block in back", INELIGIBLE_RECEIVER_DOWNFIELD,
						"illegal use of the hands", "unnecessary roughness", "illegal substitution", "chop block",
						"illegal shift", "roughing the passer", "illegal formation", "unsportsmanlike conduct",
						"delay of game", "intentional grounding", "illegal participation", "us", "offside",
						"pass interference", "false start", "personal foul", "holding", "of" };
				for (String pen : penaltyArray) {
					if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()
							&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()
							&& !offsetting) {
						handleStandardPenalty(params, String.format(" %s ", pen));
					}

				}
			}
			if (Objects.isNull(params.getPlay().getNoPlayPenalty())) {
				params.getPlay().setNoPlayPenalty(false);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void updatePlayTextString(PbpServiceRequestPojo params) {
		try {
			if (Boolean.TRUE.equals(params.getPlay().getNoPlayPenalty())) {
				params.setPlayRawText(params.getPlayRawText().replaceAll("(.*)(?i)Penalty", PENALTY));
			} else {
				if (params.getPlayRawText().replaceAll(I_PENALTY, "").isEmpty()) {
					params.getPlay().setNoPlayPenalty(true);
					params.getPlay().setPlayType(PlayTypeEnum.PENALTY);
				} else {
					params.setPlayRawText(params.getPlayRawText().replaceAll(I_PENALTY, ""));
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static boolean handleNoPlay(PbpServiceRequestPojo params, boolean noPlay) {
		try {
			if (PbpParsingUtils.evalMatch(params.getPlayRawText(), "TOUCHDOWN.+nullified by penalty")) {
				noPlay = true;
			} else if (PbpParsingUtils.evalMatch(params.getPlayRawText(),
					"((( NO PLAY)( |$))|(replay the down)|(nullified))")) {
				noPlay = true;
			}
			return noPlay;
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void handleFirstDown(PbpServiceRequestPojo params, PlayerStatPenaltyPojo penaltyStat,
			String penTeam) {
		try {
			if (params.getPlayRawText().toUpperCase().contains(FIRST_DOWN)
					|| params.getPlayRawText().toUpperCase().contains("FIRST DOWN")) {
				String matchStr = "((NO PLAY)|((1ST DOWN) ([A-Z]*?-?[aA-zZ]{2,6}).?)|((FIRST DOWN) ([A-Z]*?-?[aA-zZ]{2,6}).?)|(automatic 1ST DOWN)|(results in a 1ST DOWN))";
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					String penaltyStr = PbpParsingUtils.extractCustom(params.getPlayRawText(), matchStr, 10);
					String[] penaltyArray = penaltyStr.split("\\|")[0].split("\\~");
					handleFirstDownHelper(params, penaltyStat, penTeam, penaltyArray);
				} else if (params.getPlayRawText().indexOf(FIRST_DOWN) > params.getPlayRawText().indexOf(PENALTY)) {
					penaltyStat.setPenaltyFirstDown(1);
					params.getPlay().getPlayResult().setPlayResultFirstDown(true);
				} else {
					penaltyStat.setPenaltyFirstDown(0);
					params.getPlay().getPlayResult().setPlayResultFirstDown(false);
				}
			} else {
				penaltyStat.setPenaltyFirstDown(0);
				params.getPlay().getPlayResult().setPlayResultFirstDown(false);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void handleFirstDownHelper(PbpServiceRequestPojo params, PlayerStatPenaltyPojo penaltyStat,
			String penTeam, String[] penaltyArray) {
		try {
			if (FIRST_DOWN.equals(penaltyArray[3]) || "FIRST DOWN".equals(penaltyArray[6])) {
				if (penTeam.equals(penaltyArray[4]) || penTeam.equals(penaltyArray[7])) {
					penaltyStat.setPenaltyFirstDown(0);
					params.getPlay().getPlayResult().setPlayResultFirstDown(false);
				} else {
					penaltyStat.setPenaltyFirstDown(1);
					params.getPlay().getPlayResult().setPlayResultFirstDown(true);
				}
			} else if ("automatic 1ST DOWN".equals(penaltyArray[8])
					|| "results in a 1ST DOWN".equals(penaltyArray[9])) {
				if (params.getPlay().getPlayType() == PlayTypeEnum.PUNT && PbpParsingUtils.resolvePossesionTeam(penTeam,
						params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict())) {
					penaltyStat.setPenaltyFirstDown(0);
					params.getPlay().getPlayResult().setPlayResultFirstDown(false);
				} else {
					penaltyStat.setPenaltyFirstDown(1);
					params.getPlay().getPlayResult().setPlayResultFirstDown(true);
				}
			} else if (params.getPlayRawText().lastIndexOf(FIRST_DOWN) < params.getPlayRawText().indexOf(PENALTY)) {
				penaltyStat.setPenaltyFirstDown(0);
				params.getPlay().getPlayResult().setPlayResultFirstDown(false);
			} else if (params.getPlayRawText().lastIndexOf(FIRST_DOWN) > params.getPlayRawText().indexOf(PENALTY)) {
				penaltyStat.setPenaltyFirstDown(1);
				params.getPlay().getPlayResult().setPlayResultFirstDown(true);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static boolean handleDeclined(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("DECLINED")) {
				params.setPlayRawText(params.getPlayRawText().replaceAll(I_PENALTY, ""));
				return true;
			} else {
				return false;
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static boolean handleOffsetting(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("OFFSETTING")) {
				params.setPlayRawText(params.getPlayRawText().replaceAll(".*", ""));
				params.getPlay().setNoPlayPenalty(true);
				params.getPlay().getPlayResult().setPlayResultPoints(0);
				params.getPlay().getPlayResult().setPlayResultYard(0);
				params.getPlay().getPlayResult().setPlayResultFirstDown(false);
				params.getPlay().setPlayType(PlayTypeEnum.PENALTY);
				params.getPlay().getPlayResult().setPlayResultYardLine(params.getPlay().getPlayStartYard());
				return true;
			} else {
				return false;
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void handleKickoffOutOfBounds(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("KICKOFF")
					&& (params.getPlayRawText().toUpperCase().contains(" IP ")
							|| PbpParsingUtils.evalMatch(params.getPlayRawText(), "kickoff.+out of bounds"))) {
				PlayerStatPenaltyPojo penaltyStat;
				String firstMatchStr = String.format("%s kickoff .+ PENALTY ([A-Z]*?-?[aA-zZ]{2,3}).? IP",
						NcaaConstants.PLAYER_NAME_REGEX);
				String secondMatchStr = String.format("%s kickoff.+out of bounds", NcaaConstants.PLAYER_NAME_REGEX);
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {
					String passInterferenceString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
							firstMatchStr, 8);
					String[] passInterferenceStringArray = passInterferenceString.split("\\|")[0].split("\\~");
					String penaltyPlayer = PbpParsingUtils.formatName(passInterferenceStringArray[0]);
					penaltyStat = new PlayerStatPenaltyPojo(penaltyPlayer);
					penaltyStat.setPenaltyName(PenaltyEnum.KICKOFF_OUT_OF_BOUNDS);
					penaltyStat.setPenaltyYards(35);
					penaltyStat.setPenaltyFirstDown(0);
					params.getPlay().getPlayResult().setPlayResultYardLine(35);
					addPenalty(params, penaltyStat, params.getDefenseTeam(), false, false);
				} else if (PbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr)) {
					String passInterferenceString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
							secondMatchStr, 7);
					String[] passInterferenceStringArray = passInterferenceString.split("\\|")[0].split("\\~");
					String penaltyPlayer = PbpParsingUtils.formatName(passInterferenceStringArray[0]);
					penaltyStat = new PlayerStatPenaltyPojo(penaltyPlayer);
					penaltyStat.setPenaltyName(PenaltyEnum.KICKOFF_OUT_OF_BOUNDS);
					penaltyStat.setPenaltyYards(35);
					penaltyStat.setPenaltyFirstDown(0);
					params.getPlay().getPlayResult().setPlayResultYardLine(35);
					addPenalty(params, penaltyStat, params.getDefenseTeam(), false, false);
				} else {
					String logInfo = String.format("Regex1: %s | Regex2: %s | Play Text: %s", firstMatchStr,
							secondMatchStr, params.getPlayRawText());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Out of bounds parsing did not match regex");
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void handleStandardPenalty(PbpServiceRequestPojo params, String penalty) {
		try {
			if (PbpParsingUtils.evalMatch(params.getPlayRawText(), String.format("PENALTY.+(?i)%s(?i)", penalty))) {
				boolean noPlay;

				params.setPlayRawText(params.getPlayRawText().replaceAll(
						String.format("(PENALTY ([A-Z]*?-?[aA-zZ]{2,6})(?i)%s(?-i))((defense )|(offense ))", penalty),
						"$1"));

				String enforcedOnCleanUpRegex = "([A-Z]*?-?[aA-zZ]{2,6})(?i)%s(?-i)(on )?()(enforced )?(\\d{1,2})";
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format(enforcedOnCleanUpRegex, penalty))) {
					params.setPlayRawText(
							params.getPlayRawText().replaceAll(String.format(enforcedOnCleanUpRegex, penalty),
									String.format("$1%son TEAM,$1 $4$5", penalty)));
				}

				PlayerStatPenaltyPojo penaltyStat = new PlayerStatPenaltyPojo();
				penaltyStat.setPenalty(1);
				String penaltyTeam = handleStandardPenaltyHelper(params, penalty, penaltyStat);
				penaltyStat.setPenaltyName(PenaltyEnum.valueOf(penalty.toUpperCase().strip().replace(" ", "_")));

				noPlay = penaltyStat.getPenaltyName().getNoPlay();

				if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
					noPlay = false;
				}

				noPlay = handleNoPlay(params, noPlay);
				handleFirstDown(params, penaltyStat, penaltyTeam);

				noPlay = handleStandardPenaltyNoPlayHelper(params, penalty, noPlay);

				boolean defensePenalty = !PbpParsingUtils.resolvePossesionTeam(penaltyTeam, params.getPossessionTeam(),
						params.getDefenseTeam(), params.getTeamAbbrevDict());
				if (defensePenalty) {
					addPenalty(params, penaltyStat, params.getDefenseTeam(), defensePenalty, noPlay);
				} else {
					addPenalty(params, penaltyStat, params.getPossessionTeam(), defensePenalty, noPlay);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static String handleStandardPenaltyHelper(PbpServiceRequestPojo params, String penalty,
			PlayerStatPenaltyPojo penaltyStat) {
		try {
			String[] penaltyArrays;
			String[] penaltyArray;
			String penaltyTeam;

			String firstMatchStr = String.format(
					"PENALTY (?:Before the snap, )?([A-Z]*?-?[aA-zZ]{2,6}).?(?i)%s(?-i)(?:on )?(\\(?%s\\)? )?(?:enforced (?:half the distance from the goal,? )?)?(?:at the spot of the foul for )?(\\d{1,2}) yards?(?: from( the end of the play at)?(?: the)?(?:%s))? to(?: the)?%s?",
					penalty, NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX,
					NcaaConstants.TEAM_YARD_REGEX);
			String secondMatchStr = String.format(
					"PENALTY (?:Before the snap, )?([A-Z]*?-?[aA-zZ]{2,6}).?(?i)%s(?-i)(?:on )?(\\(?%s\\)?\\.? ?)",
					penalty, NcaaConstants.PLAYER_NAME_REGEX);
			if (PbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {
				String penaltyStr = PbpParsingUtils.extractCustom(params.getPlayRawText(), firstMatchStr, 13);
				penaltyArrays = penaltyStr.split("\\|");
				if (penaltyArrays.length > 1) {
					String logInfo = String.format(PENALTY_COUNT_S_PLAY_TEXT_S, penaltyArrays.length,
							params.getPlayRawText());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException(MULTIPLE_PENALTIES_FOUND_NEED_TO_HANDLE_THIS);
				}
				penaltyArray = penaltyArrays[0].split("\\~");
				penaltyTeam = penaltyArray[0];
				if ("null".equals(penaltyArray[2])) {
					penaltyArray[2] = String.format("TEAM,%s", penaltyArray[0]);
				}
				penaltyStat.setPlayerName(PbpParsingUtils.formatName(penaltyArray[2]));
				penaltyStat.setPenaltyYards(Integer.valueOf(penaltyArray[9]));

			} else if (PbpParsingUtils.evalMatch(params.getPlayRawText(), secondMatchStr)) {
				String penaltyStr = PbpParsingUtils.extractCustom(params.getPlayRawText(), secondMatchStr, 8);
				penaltyArrays = penaltyStr.split("\\|");
				if (penaltyArrays.length > 1) {
					String logInfo = String.format(PENALTY_COUNT_S_PLAY_TEXT_S, penaltyArrays.length,
							params.getPlayRawText());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException(MULTIPLE_PENALTIES_FOUND_NEED_TO_HANDLE_THIS);
				}
				penaltyArray = penaltyArrays[0].split("\\~");
				penaltyTeam = penaltyArray[0];
				if ("null".equals(penaltyArray[2])) {
					penaltyArray[2] = String.format("TEAM,%s", penaltyArray[0]);
				}
				penaltyStat.setPlayerName(PbpParsingUtils.formatName(penaltyArray[2]));
				penaltyStat.setPenaltyYards(
						PenaltyEnum.valueOf(penalty.toUpperCase().strip().replace(" ", "_")).getYards());
			} else {
				String logInfo = String.format("Regex1: %s | Regex2: %s | Play Text: %s", firstMatchStr, secondMatchStr,
						params.getPlayRawText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Standard penalty parsing did not match regex");
			}
			return penaltyTeam;
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static boolean handleStandardPenaltyNoPlayHelper(PbpServiceRequestPojo params, String penalty,
			boolean noPlay) {
		try {
			String[] penaltyArrays;
			String[] penaltyArray;

			String firstMatchStr = String.format(
					"PENALTY (?:Before the snap, )?([A-Z]*?-?[aA-zZ]{2,6}).?(?i)%s(?-i)(?:on )?(\\(?%s\\)? )?(?:enforced (?:half the distance from the goal,? )?)?(?:at the spot of the foul for )?(\\d{1,2}) yards?(?: from( the end of the play at)?(?: the)?(?:%s))? to(?: the)?%s?",
					penalty, NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX,
					NcaaConstants.TEAM_YARD_REGEX);
			if (PbpParsingUtils.evalMatch(params.getPlayRawText(), firstMatchStr)) {
				String penaltyStr = PbpParsingUtils.extractCustom(params.getPlayRawText(), firstMatchStr, 13);
				penaltyArrays = penaltyStr.split("\\|");
				if (penaltyArrays.length > 1) {
					String logInfo = String.format(PENALTY_COUNT_S_PLAY_TEXT_S, penaltyArrays.length,
							params.getPlayRawText());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException(MULTIPLE_PENALTIES_FOUND_NEED_TO_HANDLE_THIS);
				}
				penaltyArray = penaltyArrays[0].split("\\~");
				if (penaltyArray.length > 8 && "the end of the play at ".equals(penaltyArray[10])) {
					noPlay = true;
				}
			}
			return noPlay;
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void addPenalty(PbpServiceRequestPojo params, PlayerStatPenaltyPojo penaltyStat, String team,
			boolean defense, boolean noPlay) {
		try {
			Integer playResultYard;
			params.getPlay().getPlayerStat().get(team).getPenalty().add(penaltyStat);
			params.getPlay().setNoPlayPenalty(noPlay);
			if (noPlay) {
				if (params.getPlay().getPlayType() == PlayTypeEnum.PUNT) {
					if (defense) {
						defense = false;
					} else {
						defense = true;
					}
					params.setPuntReturnTeam(null);
					params.setPuntTeam(null);
				}
				params.getPlay().setPlayType(PlayTypeEnum.PENALTY);
			}
			if (defense) {
				playResultYard = penaltyStat.getPenaltyYards();
			} else {
				playResultYard = penaltyStat.getPenaltyYards() * -1;
			}
			if (params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
					&& params.getPlay().getPlayType() != PlayTypeEnum.PUNT) {
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(playResultYard);
			}
			params.getPlay().getPlayResult().setPlayResultPoints(0);
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}
}
