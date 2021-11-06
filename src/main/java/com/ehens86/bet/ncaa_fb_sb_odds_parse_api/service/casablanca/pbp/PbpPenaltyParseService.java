package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PenaltyEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpPenaltyParseService {
	private static final Logger LOG = Logger.getLogger(PbpPenaltyParseService.class.toString());

	private final PbpParsingUtils pbpParsingUtils;

	public PbpPenaltyParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public boolean parsePenalty(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("PENALTY")
					|| (pbpParsingUtils.evalMatch(params.getPlayRawText(), "kickoff.+out of bounds")
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
				params.setPlayRawText(params.getPlayRawText().replace("ineligible downfield on pass",
						"ineligible receiver downfield"));
				params.setPlayRawText(params.getPlayRawText().replace("Ineligible Downfield on Pass",
						"ineligible receiver downfield"));
				params.setPlayRawText(params.getPlayRawText().replaceAll("  ", " "));
				params.setPlayRawText(params.getPlayRawText().replace("roughing passer", "roughing the passer"));


				if (params.getPlayRawText().equals("Gerardi,Jake punt 44 yards to the ASU25, out of bounds PENALTY ASU Roughing The Kicker (Johnson,Andre) 15 yards from SUU31 to SUU46, 1ST DOWN. NO PLAY")) {
					System.out.println("catch");
				}
//				params.setPlayRawText(params.getPlayRawText().replace(" Holding  ", " Holding "));
//				params.setPlayRawText(params.getPlayRawText().replace(" Personal Foul  ", " Personal Foul "));
//				params.setPlayRawText(
//						params.getPlayRawText().replace(" Illegal Substitution  ", " Illegal Substitution "));
//				params.setPlayRawText(
//						params.getPlayRawText().replace(" False Start  ", " False Start "));
//				params.setPlayRawText(params.getPlayRawText().replace(" Illegal Formation  ", " Illegal Formation "));
//				params.setPlayRawText(
//						params.getPlayRawText().replace(" Unsportsmanlike Conduct  ", " Unsportsmanlike Conduct "));
//				params.setPlayRawText(params.getPlayRawText().replace(" Illegal Shift  ", " Illegal Shift "));
//				params.setPlayRawText(params.getPlayRawText().replace(" Delay Of Game  ", " Delay Of Game "));
//				params.setPlayRawText(
//						params.getPlayRawText().replace(" Illegal use of the hands  ", " Illegal use of the hands "));

				System.out.println(params.getPlayRawText());
				if (params.getPlayRawText().toUpperCase().contains("AFTER THE PLAY")) {
					throw new IllegalArgumentException("HANDLE AFTER THE PLAY PENALTY");
				}
				if (params.getPlayRawText().equals(
						"Covey,Britain rush for 14 yards gain to the WSU17 (Tarlas,George), 1ST DOWN, PENALTY UTA Holding 10 yards from WSU17 to WSU27")) {
					System.out.println("CATCH");
				}
				boolean declined = handleDeclined(params);
				boolean offsetting = handleOffsetting(params);
				handleKickoffOutOfBounds(params);

				if (!declined) {
					String[] penaltyArray = new String[] { "player disqualification", "hd", "do", "running into the kicker", "hl", "illegal snap", "encroachment", "illegal procedure", "fair catch interference", "illegal forward pass",
							"illegal motion", "roughing the kicker", "targeting", "block below the waist",
							"illegal block", "too many men on the field", "substitution infraction", "face mask",
							"illegal block in back", "ineligible receiver downfield", "illegal use of the hands",
							"unnecessary roughness", "illegal substitution", "chop block", "illegal shift",
							"roughing the passer", "illegal formation", "unsportsmanlike conduct", "delay of game",
							"intentional grounding", "illegal participation", "us", "offside", "pass interference",
							"false start", "personal foul", "holding", "of" };
					for (String pen : penaltyArray) {
						if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()
								&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()
								&& !declined && !offsetting) {
							handleStandardPenalty(params, String.format(" %s ", pen));
						}
						
					}
				}

				if (Objects.isNull(params.getPlay().getNoPlayPenalty())) {
					params.getPlay().setNoPlayPenalty(false);
				}

				if (params.getPlay().getNoPlayPenalty() == true) {
					params.setPlayRawText(params.getPlayRawText().replaceAll("(.*)(?i)Penalty", "PENALTY"));
				} else {
					params.setPlayRawText(params.getPlayRawText().replaceAll(",?;?\\.? ?(?i)Penalty(.*)", ""));
					if (params.getPlayRawText().isEmpty()) {
						params.getPlay().setNoPlayPenalty(true);
						params.getPlay().setPlayType(PlayTypeEnum.PENALTY);
						params.getPlay().setPlayCallType(PlayCallTypeEnum.NA);
					}
				}

				params.setPlayTackles(pbpParsingUtils.extractTackle(params.getPlayRawText()));
				System.out.println(params.getPlayRawText());
				System.out.println(String.format("No play: %s", params.getPlay().getNoPlayPenalty()));

				if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()
						&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()
						&& !declined && !offsetting) {
					throw new IllegalArgumentException("Missing penalty");
				}
				if (!offsetting && !declined) {
					reconcileNoPlayHalfDistance(params);
				}
				if (params.getPlay().getNoPlayPenalty() && declined) {
					return true;
				} else {
					return false;
				}
			} else {
				params.getPlay().setNoPlayPenalty(false);
				return false;
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

	private boolean handleNoPlay(PbpServiceRequestPojo params, boolean noPlay) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					"TOUCHDOWN.+nullified by penalty")
					) {
//				&& params.getPlay().getPlayType() == PlayTypeEnum.PUNT
			} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					"((( NO PLAY)( |$))|(replay the down)|(nullified))")) {
				noPlay = true;
			}
			return noPlay;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void handleFirstDown(PbpServiceRequestPojo params, TeamStatPenaltyPojo penaltyStat, String penTeam) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("1ST DOWN")
					|| params.getPlayRawText().toUpperCase().contains("FIRST DOWN")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						"((NO PLAY)|((1ST DOWN) ([A-Z]*?-?[aA-zZ]{2,6}).?)|((FIRST DOWN) ([A-Z]*?-?[aA-zZ]{2,6}).?)|(automatic 1ST DOWN)|(results in a 1ST DOWN))")) {
					String penaltyStr = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							"((NO PLAY)|((1ST DOWN) ([A-Z]*?-?[aA-zZ]{2,6}).?)|((FIRST DOWN) ([A-Z]*?-?[aA-zZ]{2,6}).?)|(automatic 1ST DOWN)|(results in a 1ST DOWN))",
							10);
					String[] penaltyArray = penaltyStr.split("\\|")[0].split("\\~");

					if ("1ST DOWN".equals(penaltyArray[3]) || "FIRST DOWN".equals(penaltyArray[6])) {
						if (penTeam.equals(penaltyArray[4]) || penTeam.equals(penaltyArray[7])) {
							penaltyStat.setPenaltyFirstDown(0);
							params.getPlay().getPlayResult().setPlayResultFirstDown(false);
						} else {
							penaltyStat.setPenaltyFirstDown(1);
							params.getPlay().getPlayResult().setPlayResultFirstDown(true);
						}
					} else if ("automatic 1ST DOWN".equals(penaltyArray[8])
							|| "results in a 1ST DOWN".equals(penaltyArray[9])) {
						if (params.getPlay().getPlayType() == PlayTypeEnum.PUNT
								&& pbpParsingUtils.resolvePossesionTeam(penTeam, params.getPossessionTeam(),
										params.getDefenseTeam(), params.getTeamAbbrevDict())) {
							penaltyStat.setPenaltyFirstDown(0);
							params.getPlay().getPlayResult().setPlayResultFirstDown(false);
						} else {
							penaltyStat.setPenaltyFirstDown(1);
							params.getPlay().getPlayResult().setPlayResultFirstDown(true);
						}
					} else if (params.getPlayRawText().lastIndexOf("1ST DOWN") < params.getPlayRawText()
							.indexOf("PENALTY")) {
						penaltyStat.setPenaltyFirstDown(0);
						params.getPlay().getPlayResult().setPlayResultFirstDown(false);
					} else if (params.getPlayRawText().lastIndexOf("1ST DOWN") > params.getPlayRawText()
							.indexOf("PENALTY")) {
						penaltyStat.setPenaltyFirstDown(1);
						params.getPlay().getPlayResult().setPlayResultFirstDown(true);
					} else {
						throw new IllegalArgumentException("HANDLE FIRST DOWN FAILED");
					}
				} else if (params.getPlayRawText().indexOf("1ST DOWN") > params.getPlayRawText().indexOf("PENALTY")) {
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
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private boolean handleDeclined(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("DECLINED")) {
				params.setPlayRawText(params.getPlayRawText().replaceAll(",?;?\\.? ?(?i)Penalty(.*)", ""));
				return true;
			} else {
				return false;
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

	private boolean handleOffsetting(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("OFFSETTING")) {
				params.setPlayRawText(params.getPlayRawText().replaceAll(".*", ""));
				params.getPlay().setNoPlayPenalty(true);
				params.getPlay().getPlayResult().setPlayResultPoints(0);
				params.getPlay().getPlayResult().setPlayResultYard(0);
				params.getPlay().getPlayResult().setPlayResultFirstDown(false);
				params.getPlay().setPlayCallType(PlayCallTypeEnum.NA);
				params.getPlay().setPlayType(PlayTypeEnum.PENALTY);
				params.getPlay().getPlayResult().setPlayResultYardLine(params.getPlay().getPlayStartYard());
				return true;
			} else {
				return false;
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

	private void handleKickoffOutOfBounds(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("KICKOFF")
					&& (params.getPlayRawText().toUpperCase().contains(" IP ")
							|| pbpParsingUtils.evalMatch(params.getPlayRawText(), "kickoff.+out of bounds"))) {
				TeamStatPenaltyPojo penaltyStat;
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String
						.format("%s kickoff .+ PENALTY ([A-Z]*?-?[aA-zZ]{2,3}).? IP", NcaaConstants.playerNameRegex))) {
					String passInterferenceString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("%s kickoff .+ PENALTY ([A-Z]*?-?[aA-zZ]{2,3}).? IP",
									NcaaConstants.playerNameRegex),
							8);
					String[] passInterferenceStringArray = passInterferenceString.split("\\|")[0].split("\\~");
					String penaltyTeam = passInterferenceStringArray[7];
					String penaltyPlayer = pbpParsingUtils.formatName(passInterferenceStringArray[0]);
					penaltyStat = new TeamStatPenaltyPojo(penaltyPlayer);
					penaltyStat.setPenaltyName(PenaltyEnum.KICKOFF_OUT_OF_BOUNDS);
					penaltyStat.setPenaltyYards(35);
					penaltyStat.setPenaltyFirstDown(0);
					params.getPlay().getPlayResult().setPlayResultYardLine(35);
					addPenalty(params, penaltyStat, params.getPossessionTeam(), false, false);
				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("%s kickoff.+out of bounds", NcaaConstants.playerNameRegex))) {
					String passInterferenceString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("%s kickoff.+out of bounds", NcaaConstants.playerNameRegex), 7);
					String[] passInterferenceStringArray = passInterferenceString.split("\\|")[0].split("\\~");
					String penaltyPlayer = pbpParsingUtils.formatName(passInterferenceStringArray[0]);
					penaltyStat = new TeamStatPenaltyPojo(penaltyPlayer);
					penaltyStat.setPenaltyName(PenaltyEnum.KICKOFF_OUT_OF_BOUNDS);
					penaltyStat.setPenaltyYards(35);
					penaltyStat.setPenaltyFirstDown(0);
					params.getPlay().getPlayResult().setPlayResultYardLine(35);
					addPenalty(params, penaltyStat, params.getPossessionTeam(), false, false);
				} else {
					throw new IllegalArgumentException("HANDLE THIS");
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

	private void handleStandardPenalty(PbpServiceRequestPojo params, String penalty) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format("PENALTY.+(?i)%s(?i)", penalty))) {
				TeamStatPenaltyPojo penaltyStat;
				boolean noPlay;

				params.setPlayRawText(params.getPlayRawText().replaceAll(
						String.format("(PENALTY ([A-Z]*?-?[aA-zZ]{2,6})(?i)%s(?-i))((defense )|(offense ))", penalty),
						"$1"));

				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("([A-Z]*?-?[aA-zZ]{2,6})(?i)%s(?-i)(on )?()(enforced )?(\\d{1,2})", penalty))) {
					params.setPlayRawText(params.getPlayRawText().replaceAll(
							String.format("([A-Z]*?-?[aA-zZ]{2,6})(?i)%s(?-i)(on )?()(enforced )?(\\d{1,2})", penalty),
							String.format("$1%son TEAM,$1 $4$5", penalty)));
				}

				String[] penaltyArrays;
				String[] penaltyArray;
				String penaltyTeam;
				String penaltyPlayer;
				Integer penaltyYards;

				if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format(
						"PENALTY (?:Before the snap, )?([A-Z]*?-?[aA-zZ]{2,6}).?(?i)%s(?-i)(?:on )?(\\(?%s\\)? )?(?:enforced (?:half the distance from the goal,? )?)?(?:at the spot of the foul for )?(\\d{1,2}) yards?(?: from( the end of the play at)?(?: the)?(?:%s))? to(?: the)?%s?",
						penalty, NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex,
						NcaaConstants.teamYardRegex))) {
					String penaltyStr = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
							"PENALTY (?:Before the snap, )?([A-Z]*?-?[aA-zZ]{2,6}).?(?i)%s(?-i)(?:on )?(\\(?%s\\)? )?(?:enforced (?:half the distance from the goal,? )?)?(?:at the spot of the foul for )?(\\d{1,2}) yards?(?: from( the end of the play at)?(?: the)?(?:%s))? to(?: the)?%s",
							penalty, NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex,
							NcaaConstants.teamYardRegex), 13);
					penaltyArrays = penaltyStr.split("\\|");
					if (penaltyArrays.length > 1) {
						throw new IllegalArgumentException("HANDLE THIS");
					}
					penaltyArray = penaltyArrays[0].split("\\~");
					penaltyTeam = penaltyArray[0];
					if ("null".equals(penaltyArray[2])) {
						penaltyArray[2] = String.format("TEAM,%s", penaltyArray[0]);
					}
					penaltyPlayer = pbpParsingUtils.formatName(penaltyArray[2]);
					penaltyYards = Integer.valueOf(penaltyArray[9]);

				} else if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format(
						"PENALTY (?:Before the snap, )?([A-Z]*?-?[aA-zZ]{2,6}).?(?i)%s(?-i)(?:on )?(\\(?%s\\)?\\.? ?)",
						penalty, NcaaConstants.playerNameRegex))) {
					String penaltyStr = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
							"PENALTY (?:Before the snap, )?([A-Z]*?-?[aA-zZ]{2,6}).?(?i)%s(?-i)(?:on )?(\\(?%s\\)?\\.? ?)",
							penalty, NcaaConstants.playerNameRegex), 8);
					penaltyArrays = penaltyStr.split("\\|");
					if (penaltyArrays.length > 1) {
						throw new IllegalArgumentException("HANDLE THIS");
					}
					penaltyArray = penaltyArrays[0].split("\\~");
					penaltyTeam = penaltyArray[0];
					if ("null".equals(penaltyArray[2])) {
						penaltyArray[2] = String.format("TEAM,%s", penaltyArray[0]);
					}
					penaltyPlayer = pbpParsingUtils.formatName(penaltyArray[2]);
					penaltyYards = PenaltyEnum.valueOf(penalty.toUpperCase().strip().replace(" ", "_")).getYards();
				} else {
					throw new IllegalArgumentException("HANDLE THIS");
				}

				penaltyStat = new TeamStatPenaltyPojo(penaltyPlayer);
				penaltyStat.setPenaltyName(PenaltyEnum.valueOf(penalty.toUpperCase().strip().replace(" ", "_")));
				penaltyStat.setPenaltyYards(penaltyYards);

				noPlay = penaltyStat.getPenaltyName().getNoPlay();

				if (params.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
					noPlay = false;
				}

				noPlay = handleNoPlay(params, noPlay);
				handleFirstDown(params, penaltyStat, penaltyTeam);

				if (penaltyArray.length > 8 && "the end of the play at ".equals(penaltyArray[10])) {
					noPlay = true;
				}

				boolean defensePenalty = !pbpParsingUtils.resolvePossesionTeam(penaltyTeam, params.getPossessionTeam(),
						params.getDefenseTeam(), params.getTeamAbbrevDict());
				if (defensePenalty) {
					addPenalty(params, penaltyStat, params.getDefenseTeam(), defensePenalty, noPlay);
				} else {
					addPenalty(params, penaltyStat, params.getPossessionTeam(), defensePenalty, noPlay);
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

	private void reconcileNoPlayHalfDistance(PbpServiceRequestPojo params) {
		try {
			if (params.getPlay().getNoPlayPenalty()
					&& Objects.isNull(params.getPlay().getPlayResult().getPlayResultYardLine())) {
				List<TeamStatPenaltyPojo> offensePenalty = params.getPlay().getPlayerStat()
						.get(params.getPossessionTeam()).getPenalty();
				List<TeamStatPenaltyPojo> defensePenalty = params.getPlay().getPlayerStat().get(params.getDefenseTeam())
						.getPenalty();

				Integer startYard = params.getPlay().getPlayStartYard();
				Integer halfDistance;
				if (!offensePenalty.isEmpty()) {
					halfDistance = (int) Math.ceil(startYard / 2.0);
					if (offensePenalty.get(0).getPenaltyYards() > halfDistance) {
						offensePenalty.get(0).setPenaltyYards(halfDistance);
						params.getPlay().getPlayResult().setPlayResultYard(halfDistance * -1);
						params.getPlay().getPlayResult().setPlayResultYardLine(halfDistance);
					}
				} else {
					halfDistance = (int) Math.floor((100 - startYard) / 2);
					if (defensePenalty.get(0).getPenaltyYards() > halfDistance) {
						defensePenalty.get(0).setPenaltyYards(halfDistance);
						params.getPlay().getPlayResult().setPlayResultYard(halfDistance);
						params.getPlay().getPlayResult().setPlayResultYardLine(startYard + halfDistance);
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

	private void addPenalty(PbpServiceRequestPojo params, TeamStatPenaltyPojo penaltyStat, String team, boolean defense,
			boolean noPlay) {
		try {
			Integer playResultYard;
			params.getPlay().getPlayerStat().get(team).getPenalty().add(penaltyStat);
			params.getPlay().setNoPlayPenalty(noPlay);
			if (noPlay) {
				if (params.getPlay().getPlayType() == PlayTypeEnum.PUNT ) { // && !defense && penaltyStat.getPenaltyFirstDown() == 1
					if (defense) {
						defense = false;
					} else {
						defense = true;
					}
					params.setPuntReturnTeam(null);
					params.setPuntTeam(null);
				}
				params.getPlay().setPlayType(PlayTypeEnum.PENALTY);
				params.getPlay().setPlayCallType(PlayCallTypeEnum.NA);
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
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
}
