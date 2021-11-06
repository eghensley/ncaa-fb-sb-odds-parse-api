package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpDefenseParseService {
	private static final Logger LOG = Logger.getLogger(PbpDefenseParseService.class.toString());

	private final PbpParsingUtils pbpParsingUtils;

	public PbpDefenseParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public void parseDefense(PbpServiceRequestPojo params) {
		try {
			// TODO handle offense tackles on turnovers
			parseTackles(params);
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				parseSack(params);
				parseInterception(params);
				parseBreakup(params);
				parseQbHurry(params);
			}
			parseFumbleRecovery(params);
			parseFumble(params); // TODO tests
			parseFumbleForced(params);
			parseTurnoverReturn(params);
			parseSafety(params);
			removeTurnoverTackles(params);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTackles(PbpServiceRequestPojo params) {
		try {
			Integer yards;
			boolean solo = false;
			if (Objects.isNull(params.getPlayTackles())) {
				return;
			} else {

				if (params.getPlayTackles().length == 1) {
					solo = true;
				}
				for (String tackleName : params.getPlayTackles()) {
					if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
						yards = params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.findPuntReturner().getPuntReturnYard();
						if (solo) {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.findPuntCoverageByName(tackleName).applyTackleSolo(tackleName, yards);
						} else {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.findPuntCoverageByName(tackleName).applyTackleSolo(tackleName, yards);
						}
					} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
						yards = params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.findKickoffReturner().getKickReturnYard();
						if (solo) {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.findKickCoverageByName(tackleName).applyTackleSolo(tackleName, yards);
						} else {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.findKickCoverageByName(tackleName).applyTackleSolo(tackleName, yards);
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
					}
				}
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

	private void parseBreakup(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("DEFENDED")
					|| params.getPlayRawText().toUpperCase().contains("BROKEN UP")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("broken up by %s", NcaaConstants.playerNameRegex))) {
					// TODO add yards thrown to
					String pbuString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("broken up by %s", NcaaConstants.playerNameRegex), 7);
					String[] pbuStringArray = pbuString.split("\\|")[0].split("\\~");
					String pbuName = pbpParsingUtils.formatName(pbuStringArray[0]);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(pbuName).setPassBreakUp(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingBreakup(1);
				} else {
					throw new IllegalArgumentException("HANDLE PBU CASE");
				}
			} else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
					.get(0).getPassingCompletion() != 1
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).getPassingInterception() != 1
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).getPassingSack() != 1
					&& Objects.nonNull(params.getPlayTackles())) {
//				System.out.println("catch");
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingBreakup(1);
				for (PlayerStatDefenseProductionPojo def : params.getPlay().getPlayerStat().get(params.getDefenseTeam())
						.getDefense().getDefenseProduction()) {
//					System.out.println("catch");
					def.clearTackles();
					def.setPassBreakUp(1);
				}

			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingBreakup(0);
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

	private void parseQbHurry(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("HURRY")
					|| params.getPlayRawText().toUpperCase().contains("HURRIED")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("QB hurr(?:(?:y)|(?:ied)) by %s", NcaaConstants.playerNameRegex))) {
					String hurryString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("QB hurr(?:(?:y)|(?:ied)) by %s", NcaaConstants.playerNameRegex), 7);
					String[] hurryStringArray = hurryString.split("\\|")[0].split("\\~");
					String hurryName = pbpParsingUtils.formatName(hurryStringArray[0]);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(hurryName).setQuarterbackHurry(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingHurry(1);
				} else {
					throw new IllegalArgumentException("HANDLE QB HURRY CASE");
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingHurry(0);
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

	private void parseSack(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("SACK")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("%s sacked for (?:(?:(?:a )?loss of \\d{1,3} yards?)|(?:no gain))",
								NcaaConstants.playerNameRegex))) {
					for (PlayerStatDefenseProductionPojo tackle : params.getPlay().getPlayerStat()
							.get(params.getDefenseTeam()).getDefense().getDefenseProduction()) {
						if (tackle.getTackleTotal() == 0) {
							continue;
						}
						tackle.setSack(1.0);
					}
				} else {
					throw new IllegalArgumentException("HANDLE SACK CASE");
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat().get(0)
						.setPassingSack(0);
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

	private void parseInterception(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("INTERCEPT")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("intercepted by %s at(?: the)?(%s|( , Touchback))", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex))) {
					Integer formattedThrownYard;
					String interceptionString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("intercepted by %s at(?: the)?(%s|( , Touchback))",
									NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
							10);
					String[] interceptionStringArray = interceptionString.split("\\|")[0].split("\\~");
					String interceptionName = pbpParsingUtils.formatName(interceptionStringArray[0]);
					if (" , Touchback".equals(interceptionStringArray[7])) {
						formattedThrownYard = 100 - params.getPlay().getPlayStartYard();
					} else {
						Integer formattedYardline = pbpParsingUtils.formatYardLine(interceptionStringArray[7],
								params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
						formattedThrownYard = formattedYardline - params.getPlay().getPlayStartYard();
					}
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(interceptionName).setInterception(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingInterception(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingYardThrownTo(formattedThrownYard);
					params.getPlay().getPlayResult().setPlayResultTurnover(true);
				} else {
					throw new IllegalArgumentException("HANDLE INTERCEPTION CASE");
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
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
//			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseFumbleForced(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FORCED")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format(" (to the|at)%s,? (fumble )?forced by %s", NcaaConstants.teamYardRegex,
								NcaaConstants.playerNameRegex))) {
					String fumbleForceString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format(" (to the|at)%s,? (fumble )?forced by %s", NcaaConstants.teamYardRegex,
									NcaaConstants.playerNameRegex),
							10);
					String[] fumbleForceStringArray = fumbleForceString.split("\\|")[0].split("\\~");
					String formatedFumbleName = pbpParsingUtils.formatName(fumbleForceStringArray[3]);
					if (Objects.isNull(params.getPlayTackles())
							|| params.getPlay().getPlayResult().isPlayResultTurnover()) {
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
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("fumble on the sack \\(forced by %s\\)", NcaaConstants.playerNameRegex))) {
					String fumbleForceString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("fumble on the sack \\(forced by %s\\)", NcaaConstants.playerNameRegex), 7);
					String[] fumbleForceStringArray = fumbleForceString.split("\\|")[0].split("\\~");
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseProductionByName(pbpParsingUtils.formatName(fumbleForceStringArray[0]))
							.setFumbleForced(1);
				} else {
					throw new IllegalArgumentException("HANDLE FORCED FUMBLE");
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

	private void parseFumbleRecovery(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("RECOVER")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("recovered by ([A-Z]*?-?[aA-zZ]{2,3}).? %s(?: at)?(?: the)?(%s)?",
								NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex))) {

					String returnerFumbleRecoverString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("recovered by ([A-Z]*?-?[aA-zZ]{2,3}).? %s(?: at)?(?: the)?(%s)?",
									NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
							10);
					String[] returnerFumbleRecoverStringArray = returnerFumbleRecoverString.split("\\|");
					String[] returnerFumbleInfo = returnerFumbleRecoverStringArray[returnerFumbleRecoverStringArray.length
							- 1].split("\\~");
					String abbrev = returnerFumbleInfo[0];
					String recoverName = pbpParsingUtils.formatName(returnerFumbleInfo[1]);

					if (!"null".equals(returnerFumbleInfo[9]) && params.getPlay().getPlayType() != PlayTypeEnum.PUNT
							&& params.getPlay().getPlayType() != PlayTypeEnum.KICKOFF) {
						Integer recoveredYardLine = pbpParsingUtils.formatYardLine(returnerFumbleInfo[9],
								params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
						Integer yardDiff = recoveredYardLine - (params.getPlay().getPlayStartYard()
								+ params.getPlay().getPlayResult().getPlayResultYard());
						params.getPlay().getPlayResult().updatePenaltyPlayResultYard(yardDiff);
					}
					boolean turnover = !pbpParsingUtils.resolvePossesionTeam(abbrev, params.getPossessionTeam(),
							params.getDefenseTeam(), params.getTeamAbbrevDict());

					if (turnover) {
						if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.findPuntCoverageByName(recoverName).setFumbleRecovered(1);
						} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
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
					} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT
							&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.getPunting().get(0).getPuntBlocked() == 1) {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.findPuntReturnByName(recoverName).applyNoPuntReturnFumble();
						String blockRecoverString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
								String.format("recovered by ([A-Z]*?-?[aA-zZ]{2,3}).? %s at the%s",
										NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
								9);
						String[] blockRecoverStringArray = blockRecoverString.split("\\~");
						Integer puntReturnStartYard = pbpParsingUtils.formatYardLine(blockRecoverStringArray[8],
								params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.findPuntReturnByName(recoverName).setPuntReturnStartYard(puntReturnStartYard);
					} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {

					}
					params.getPlay().getPlayResult().setPlayResultTurnover(turnover);

				} else {
					throw new IllegalArgumentException("HANDLE NEW RECOVERY");

				}
			} else {

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

	private void parseFumble(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FUMBLE")
					|| params.getPlayRawText().toUpperCase().contains(" MUFFED")) {
				String formatFumbleName;

				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("((?:(?:fumbled? by %s ?)| %s fumbled at)|((fumble on the sack)|(fumbled snap)))",
								NcaaConstants.playerNameRegex, NcaaConstants.playerNameRegex))) {
					String fumbleString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format(
									"((?:(?:fumbled? by %s ?)| %s fumbled at)|((fumble on the sack)|(fumbled snap)))",
									NcaaConstants.playerNameRegex, NcaaConstants.playerNameRegex),
							18);
					String[] fumbleStringArray = fumbleString.split("\\|")[0].split("\\~");
					fumbleStringArray[1] = fumbleStringArray[1].replace(" recovered", "");
					fumbleStringArray[1] = fumbleStringArray[1].replace(" at", "");

//				if (fumbleStringArray[0].endsWith(" at")) {
//					fumbleStringArray[0] = fumbleStringArray[0].replace(" at", "");
//				}
					String fumbleName;
					if (!"null".equals(fumbleStringArray[1])) {
						fumbleName = fumbleStringArray[1];
						formatFumbleName = pbpParsingUtils.formatName(fumbleName);
					} else if (!"null".equals(fumbleStringArray[8])) {
						fumbleName = fumbleStringArray[8];
						formatFumbleName = pbpParsingUtils.formatName(fumbleName);
					} else if ("fumble on the sack".equals(fumbleStringArray[15])) {
						formatFumbleName = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
								.getPassingStat().get(0).getPlayerName();
					} else if ("fumbled snap".equals(fumbleStringArray[15])) {
						formatFumbleName = params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
								.getRushingStat().get(0).getPlayerName();
					} else {
						throw new IllegalArgumentException("Cannot resolve fumble player");
					}
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("(?:return)? muffed by %s at(?: the)?%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex))) {
					String returnerMuffString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("(?:return)? muffed by %s at(?: the)?%s", NcaaConstants.playerNameRegex,
									NcaaConstants.teamYardRegex),
							8);
					formatFumbleName = pbpParsingUtils.formatName(returnerMuffString.split("\\~")[0]);
				} else {
					throw new IllegalArgumentException("Handle Case");

				}

				parseFumbleHelper(params, formatFumbleName);
				if (pbpParsingUtils.evalMatch(params.getPlay().getPlayText(), "Touchback.$")) {
					params.getPlay().getPlayResult().setPlayResultTurnover(true);
				}

			} else {
				parseNoFumbleHelper(params);
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

	private void parseFumbleHelper(PbpServiceRequestPojo params, String formatFumbleName) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				if (formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getPossessionTeam())
						.getOffense().getPassingStat().get(0).getPlayerName())) {
					if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
							.getReceivingStat().isEmpty()) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
								.get(0).applyNoReceivingFumble();
					}
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).applyPassingFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
				} else if (formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getPossessionTeam())
						.getOffense().getReceivingStat().get(0).getPlayerName())) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).applyNoPassingFumble();
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
							.get(0).applyReceivingFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
				} else {
					throw new IllegalArgumentException("PASSING FUMBLE - Name Mismatch");
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
				if (!formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getPossessionTeam())
						.getOffense().getRushingStat().get(0).getPlayerName())) {
					throw new IllegalArgumentException("RUSHING FUMBLE - Name Mismatch");
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.applyRushingFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				if (!formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getDefenseTeam())
						.getSpecialTeam().findPuntReturner().getPlayerName())) {
					throw new IllegalArgumentException("PUNT RETURN FUMBLE - Name Mismatch");
				}
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findPuntReturner()
						.applyPuntReturnFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				if (!formatFumbleName.equals(params.getPlay().getPlayerStat().get(params.getDefenseTeam())
						.getSpecialTeam().findKickoffReturner().getPlayerName())) {
					throw new IllegalArgumentException("KICKOFF FUMBLE - Name Mismatch");
				}
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findKickoffReturner()
						.applyKickReturnFumble(params.getPlay().getPlayResult().isPlayResultTurnover());
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

	private void parseNoFumbleHelper(PbpServiceRequestPojo params) {
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
				for (PlayerStatPuntReturnPojo puntReturner : params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()) {
					puntReturner.applyNoPuntReturnFumble();
				}
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				for (PlayerStatKickReturnPojo kickReturner : params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()) {
					kickReturner.applyNoKickReturnFumble();
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

	private void parseTurnoverReturn(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("RETURN")) {
				// TODO resolve reason for return (fumble vs interception)
				String turnoverReturnString;
				Integer turnoverReturnYards;
				Integer returnStartYard;
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("at(?: the)?%s,?.+ return(?:ed)?(?: for)? (-?\\d+) yards? to the%s",
								NcaaConstants.teamYardRegex, NcaaConstants.teamYardRegex))) {

					turnoverReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("at(?: the)?%s,?.+ return(?:ed)?(?: for)? (-?\\d+) yards? to the%s",
									NcaaConstants.teamYardRegex, NcaaConstants.teamYardRegex),
							3);
					turnoverReturnYards = Integer.valueOf(turnoverReturnString.split("\\~")[1]);
					returnStartYard = pbpParsingUtils.formatYardLine(turnoverReturnString.split("\\~")[0],
							params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("at%s, return(?:ed)? by %s (\\d{1,3}) yards? to the ",
								NcaaConstants.teamYardRegex, NcaaConstants.playerNameRegex))) {
					turnoverReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("at%s, return(?:ed)? by %s (\\d{1,3}) yards? to the ",
									NcaaConstants.teamYardRegex, NcaaConstants.playerNameRegex),
							9);
					turnoverReturnYards = Integer.valueOf(turnoverReturnString.split("\\~")[8]);
					returnStartYard = pbpParsingUtils.formatYardLine(turnoverReturnString.split("\\~")[0],
							params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict());
				} else {
					turnoverReturnYards = 0;
					returnStartYard = 0;
				}

				if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
					if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findPuntCoverageWithTurnover().applyReturnYards(turnoverReturnYards);
					} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findKickCoverageWithTurnover().applyReturnYards(turnoverReturnYards);
					} else {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findDefenseWithTurnover().applyReturnYards(turnoverReturnYards);
						if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findDefenseWithTurnover().getInterception() == 1) {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
									.getPassingStat().get(0).setPassingInterceptionYard(returnStartYard);
						}
					}
				} else {

					if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
						if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.findPuntReturnWithFumble().isEmpty() && turnoverReturnYards > 0) {
							throw new IllegalArgumentException("Handle return fumble recover");
						} else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.getPunting().get(0).getPuntBlocked() == 1) {
							params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
									.findPuntReturner().applyBlockReturn(turnoverReturnYards, returnStartYard);
						}
					} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
						if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.findKickReturnWithFumble().isEmpty() && turnoverReturnYards > 0) {
							throw new IllegalArgumentException("Handle return fumble recover");
						}
					} else {
						if (turnoverReturnYards > 0) {
							LOG.log(Level.WARNING, "Validate offense fumble recovery yards");
//							throw new IllegalArgumentException("Handle offense fumble recover");
						}
					}
				}

			} else {
				if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
					if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findPuntCoverageWithTurnover().applyReturnYards(0);
					} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findKickCoverageWithTurnover().applyReturnYards(0);
					} else {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findDefenseWithTurnover().applyReturnYards(0);
						if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findDefenseWithTurnover().getInterception() == 1) {
							params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
									.getPassingStat().get(0).setPassingInterceptionYard(0);
						}
					}
				}
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

	private void parseSafety(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("SAFETY")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("([A-Z]*?-?[aA-zZ]{2,3}).? (?i)safety( |,|$)"))) {
					params.getPlay().getPlayResult().setPlayResultPoints(-2);
					String safetyString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("([A-Z]*?-?[aA-zZ]{2,3}).? (?i)safety( |,|$)"), 2);
					String[] safetyStringArray = safetyString.split("\\|")[0].split("\\~");

					if (Objects.isNull(params.getPlayTackles())) {
						String safetyName = pbpParsingUtils.formatName(String.format("TEAM, %s", safetyStringArray[0]));
						params.setPlayTackles(new String[] { safetyName });
						parseTackles(params);
					}
					parseSafetyHelper(params, 1);
//					throw new IllegalArgumentException("HANDLE");
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format("(?i)safety\\)"))) {
					parseSafetyHelper(params, 0);
				} else {
					throw new IllegalArgumentException("HANDLE");
				}
			} else {
				parseSafetyHelper(params, 0);
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

	private void parseSafetyHelper(PbpServiceRequestPojo params, Integer safety) {
		try {
			if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.PUNT) {
				if (safety == 1 && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.getPunting().get(0).getPuntBlocked() == 1) {
					throw new IllegalArgumentException("HANDLE THIS");
				}
				for (PlayerStatDefenseProductionPojo puntCov : params.getPlay().getPlayerStat()
						.get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()) {
					parseSafetyDefenseHelper(params, puntCov, safety);
				}
				for (PlayerStatPuntReturnPojo puntReturner : params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()) {
					puntReturner.setPuntReturnSafety(safety);
				}
			} else if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				for (PlayerStatDefenseProductionPojo kickCov : params.getPlay().getPlayerStat()
						.get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage()) {
					parseSafetyDefenseHelper(params, kickCov, safety);
				}
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnSafety(safety);
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PASS) {
				for (PlayerStatDefenseProductionPojo def : params.getPlay().getPlayerStat().get(params.getDefenseTeam())
						.getDefense().getDefenseProduction()) {
					parseSafetyDefenseHelper(params, def, safety);
				}
				if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.RUN) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
							.get(0).setRushingSafety(safety);
				} else {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getPassingStat()
							.get(0).setPassingSafety(safety);
					if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense()
							.getReceivingStat().isEmpty()) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getReceivingStat()
								.get(0).setReceivingSafety(safety);
					}
				}
			} else if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG
					|| params.getPlay().getPlayCallType() == PlayCallTypeEnum.PAT) {
				if (safety == 1) {
					throw new IllegalArgumentException("HANDLE THIS");
				} else {
					for (PlayerStatDefenseProductionPojo def : params.getPlay().getPlayerStat()
							.get(params.getDefenseTeam()).getDefense().getDefenseProduction()) {
						parseSafetyDefenseHelper(params, def, safety);
					}
				}
			} else {
				throw new IllegalArgumentException("HANDLE THIS");
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

	private void parseSafetyDefenseHelper(PbpServiceRequestPojo params, PlayerStatDefenseProductionPojo def,
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
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void removeTurnoverTackles(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
				List<PlayerStatDefenseProductionPojo> offenseTackles = params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getDefense().getDefenseProduction().stream()
						.filter(d -> d.getTackleTotal() > 0 && d.getFumbleForced() == 0).collect(Collectors.toList());
				if (!offenseTackles.isEmpty()) {
					System.out.println("CATCH");
				}
				List<PlayerStatDefenseProductionPojo> trueDefenseTackles = params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getDefense().getDefenseProduction().stream()
						.filter(d -> !(d.getTackleTotal() > 0 && d.getFumbleForced() == 0))
						.collect(Collectors.toList());
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
						.setDefenseProduction(trueDefenseTackles);
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
