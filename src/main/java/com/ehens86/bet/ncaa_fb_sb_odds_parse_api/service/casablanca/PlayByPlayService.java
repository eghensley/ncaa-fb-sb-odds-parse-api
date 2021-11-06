package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PenaltyEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.TempoTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PbpPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayResultPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PlayerStatPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayPeriodPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayPossessionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayTeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpDefenseParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpFieldGoalParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpKickoffParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpOffenseParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPassParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPenaltyParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPuntParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpRushParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpValidateService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PlayByPlayService {
	private static final Logger LOG = Logger.getLogger(PlayByPlayService.class.toString());

	private final PbpKickoffParseService pbpKickoffParse;
	private final PbpParsingUtils pbpParsingUtils;
	private final PbpPassParseService pbpPassParse;
	private final PbpRushParseService pbpRushParse;
	private final PbpPuntParseService pbpPuntParse;
	private final PbpDefenseParseService pbpDefenseParse;
	private final PbpOffenseParseService pbpOffenseParse;
	private final PbpFieldGoalParseService pbpFieldGoalParse;
	private final PbpValidateService pbpValidateService;
	private final PbpPenaltyParseService pbpPenaltyParse;

	public PlayByPlayService(PbpKickoffParseService pbpKickoffParse, PbpParsingUtils pbpParsingUtils,
			PbpPassParseService pbpPassParse, PbpRushParseService pbpRushParse, PbpPuntParseService pbpPuntParse,
			PbpDefenseParseService pbpDefenseParse, PbpOffenseParseService pbpOffenseParse,
			PbpValidateService pbpValidateService, PbpFieldGoalParseService pbpFieldGoalParse,
			PbpPenaltyParseService pbpPenaltyParse) {
		this.pbpKickoffParse = pbpKickoffParse;
		this.pbpPuntParse = pbpPuntParse;
		this.pbpParsingUtils = pbpParsingUtils;
		this.pbpPassParse = pbpPassParse;
		this.pbpRushParse = pbpRushParse;
		this.pbpDefenseParse = pbpDefenseParse;
		this.pbpOffenseParse = pbpOffenseParse;
		this.pbpValidateService = pbpValidateService;
		this.pbpFieldGoalParse = pbpFieldGoalParse;
		this.pbpPenaltyParse = pbpPenaltyParse;
	}

	public void parsePbP(PlayByPlayPojo playByPlayRaw, GamePojo game) {
		try {

			if ("5851600".equals(game.getStatsUrl()) || "5851699".equals(game.getStatsUrl())) {
				return;
			}

			DrivePojo drive;
			PbpPojo gamePlays = new PbpPojo();
			HashMap<String, HomeAwayEnum> teamDict = new HashMap<String, HomeAwayEnum>();
			HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<String, PlayByPlayTeamPojo>();

			for (PlayByPlayTeamPojo gameTeam : playByPlayRaw.getMeta().getTeams()) {
				teamDict.put(gameTeam.getId(), gameTeam.pullHomeAwayEnum());
				teamAbbrevDict.put(gameTeam.getId(), gameTeam);
			}

			for (PlayByPlayPeriodPojo period : playByPlayRaw.getPeriods()) {
				String splitStart = "";
				Integer splitIndex;
				PlayPeriodEnum playPeriod = period.getTitleEnum();
				Integer periodSeconds = pbpParsingUtils.convertMinSecToSec("15:00");
				for (PlayByPlayPossessionPojo possession : period.getPossessions()) {
					String possessionTeam;
					if ("5851706".equals(game.getStatsUrl()) && "".equals(possession.getTeamId())) {
						possessionTeam = "1195";
					} else {
						possessionTeam = possession.getTeamId();
					}
					String defenseTeam = playByPlayRaw.pullOpponent(possessionTeam);
					drive = new DrivePojo();
					drive.setPossessionTeamId(possessionTeam);
					drive.setDriveStartPeriod(playPeriod);
					drive.setDriveStartTime(pbpParsingUtils.convertMinSecToSec(possession.getTime()));
					Integer playsInPossession = possession.getPlays().size();
					for (int i = 0; i < playsInPossession; i++) {
						if (i == 0) {
							// drive.setDriveStartPeriod(driveStartPeriod);
						}
						PlayByPlayPlayPojo playRaw = possession.getPlays().get(i);
						PlayPojo play;

						String playRawText = playRaw.getScoreText().replace("  ", " ").replaceAll("#\\d{1,2} ", "")
								.replace(" III,", ",").replace(" Jr ", " ");
						if (pbpParsingUtils.evalMatch(playRawText, String.format("[A-Z]\\.[A-Z][a-z]+ [A-Z] "))) {
							String suffixReplaceFull = pbpParsingUtils
									.extractCustom(playRawText, String.format("([A-Z]\\.[A-Z][a-z]+ [A-Z]) "), 1)
									.split("\\~")[0];
							String suffixReplaceNew = suffixReplaceFull.split(" ")[0];
							playRawText = playRawText.replace(suffixReplaceFull, suffixReplaceNew);
						}

						playRawText = playRawText.replaceAll("^\\(\\d{1,2}:\\d{1,2}\\) ", "");

						if (playRawText.contains("Start of ") && playRawText.contains("quarter")) {
							continue;
						} else if (playRawText.contains("won the toss") || playRawText.contains("wins toss")) {
							continue;
						} else if (playRawText.contains("Timeout Official")) {
							continue;
						} else if (playRawText.contains(" drive start at ")) {
							if (i > 0) {
								splitIndex = i;
								splitStart = pbpParsingUtils
										.extractCustom(playRawText, "drive start at (\\d{1,2}:\\d{1,2})", 1)
										.split("\\~")[0];
							}
							continue;
						} else if (playRawText.contains("End of game")) {
							continue;
						} else if (playRawText.contains("will receive")) {
							continue;
						} else if (playRawText.contains("wins coin toss")) {
							continue;
						} else if (playRawText.contains("captains")) {
							continue;
						} else if (playRawText.contains("free kick")) {
							continue;
						} else if (playRawText.startsWith("End of half")) {
							continue;
						} else if (playRawText.startsWith("End of ")) {
							continue;
						} else if (playRawText.startsWith("Timeout")) {
							continue;
						} else if (playRawText.contains(" ball on ")) {
							continue;
						} else if (playRawText.equals("QB hurry by Nick Jackson, QB hurry by Nelson Jordan.")) {
							continue;
						} else if (playRawText.toUpperCase().contains(" KICK ATTEMPT ")) {
							// TODO remove, need to first split up TD+PAT combined
							continue;
						} else if (playRawText.equals(
								"PENALTY Before the snap, REI False Start  enforced 5 yards from the KSU3 to the KSU8.")) {
							// TODO split into two plays
							continue;
						} else if (pbpParsingUtils.evalMatch(playRawText, "^(\\d[a-z]{2}) and (\\d+)\\.$")) {
							continue;
						} else if (pbpParsingUtils.evalMatch(playRawText, "^Clock \\d{2}:\\d{2}.?$")) {
							continue;
						} else if (pbpParsingUtils.evalMatch(playRawText, "^[A-Z]{3} Capt. [\\d,]+$")) {
							continue;
						} else {
							play = new PlayPojo();
							play.setPeriod(playPeriod);
							play.setPlayText(playRawText);
							play.setDriveText(playRaw.getDriveText().replace("AMP;", ""));
							for (String teamId : teamDict.keySet()) {
								play.getPlayerStat().put(teamId, new PlayerStatPojo());
							}
						}

						System.out.println(playRawText);
						System.out.println("``````");

						playRawText = cleanUpPlayText(playRawText, teamAbbrevDict, possessionTeam, defenseTeam);
						String[] playTackles = pbpParsingUtils.extractTackle(playRawText);

						if (playRawText.startsWith(" ")) {
							playRawText = playRawText.stripLeading();
							play.setPlayText(playRawText);
						}

						PbpServiceRequestPojo serviceRequest = new PbpServiceRequestPojo(drive, play, playRaw,
								playRawText, playTackles, teamDict, teamAbbrevDict, possessionTeam, defenseTeam);

						System.out.println(playRawText);
						System.out.println("``````");

						if (playRawText
								.contains("PENALTY NCAT personal foul (ROBERTS,Jacob) 15 yards to the 50 yardline")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.contains(
								"PENALTY ALB Block Below Waist (Duffy,Isaac) 15 yards from NDS35 to NDS50. NO PLAY")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.contains(
								", score nullified by penalty. PENALTY Before the snap, PRE False Start on C. Caldwell enforced 5 yards from the SAU3 to the SAU8")) {
							continue; // TODO handle split into two plays, TWO POINT CONVERSION
						}
						if (playRawText.contains(
								"S. Chambers pass to the right incomplete intended for X. Valladay thrown to the MSU11, score nullified by penalty. PENALTY MSU Pass Interference")) {
							continue; // TODO handle split into multiple plays; penalty, nullified touchdown, after
										// the play penalty as well
						}
						if (playRawText.contains("PENALTY SLU roughing passer 15 yards to the 50 yardline")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.contains(
								"PENALTY SLU unsportsmanlike conduct (Rhodes, Tyree) 15 yards to the SLU20, PENALTY SLU unsportsmanlike conduct (Jones, Drew) 10 yards to the SLU10, NO PLAY")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.startsWith(", score nullified by penalty. PENALTY")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.startsWith(
								"B. Zepeda kickoff 58 yards to the SMU7 out of bounds. Receiving team has elected to spot the ball at its 35-yard line PENALTY ACU Offsides on R. McKnight declined;")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText
								.startsWith("PENALTY UCA personal foul (Gray, Jeremiah) 15 yards to the 50 yardline")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.contains("elected to spot the ball at its 35-yard line")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.contains(
								"PENALTY AT Roughing The Kicker (Howard,Kyin) 15 yards from DUK35 to DUK50. NO PLAY")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.contains(
								"Stage,Adam kickoff 0 yards to the UND35, PENALTY USU personal foul 15 yards to the 50 yardline, NO PLAY")) {
							continue; // TODO handle split into two plays
						}
						if (playRawText.contains(
								"PENALTY ASU Offsides on CLARK, Christian enforced half the distance from the goal, 1 yards from the ASU3 to the ASU2")) {
							continue; // TODO PAT PENALTY, no drive test for down and distance
						}
						if (playRawText.equals("PENALTY MOR personal foul 15 yards to the MOR20")) {
							continue; // TODO PAT PENALTY, no drive test for down and distance
						}
						if (playRawText.equals(
								"Ford,Jerome rush for 3 yards gain to the MUR15 (Daulton,Austin) PENALTY MUR Illegal Substitution 5 yards from MUR18 to MUR13. NO PLAY. The previous play is under review. The ruling on the field has been overturned. There were too many men on the field (OVERTURNED PLAY: J. Ford rush for 3 yards gain to the MUR15 (A. Daulton))")) {
							continue; // TODO complicated play, overturned
						}
						if (playRawText.equals(
								"Tyler Keltner kickoff 9 yards to the ETSU44, on-side kick, recovered by ETSU TEAM on ETSU44, PENALTY ETSU OF (Jawan Martin) 9 yards to the ETSU44, NO PLAY")) {
							continue; // TODO onside kick recovering team penalty
						}
						if (playRawText.equals(
								"Earle, Stone rush for 9 yards to the ACU34, out-of-bounds (Andre Reed), PENALTY ACU holding (Lutz, Remington) 10 yards to the ACU15, NO PLAY, PENALTY LC personal foul (Andre Reed) 15 yards to the ACU30, NO PLAY")) {
							continue; // TODO no play offense first down but then penalty, still get first down
						}						
						if (playRawText.equals(
								"MURPHY, Nate punt 42 yards to the SAC20, fair catch by FULCHER, Marcus. PENALTY SAC Roughing the Kicker on FILER, Greg enforced 15 yards from the UNI38 to the SAC47 and results in automatic 1ST DOWN. NO PLAY (replay the down)")) {
							continue; // TODO roughing the kicker not registered as first down for punting team
						}							
						if (playRawText.equals(
								"OHARA, Asher pass to the right complete for 12 yards to FULCHER, Marcus caught at the SAC29 and advanced to the SAC40 (SANDER, Korby), 1ST DOWN. PENALTY SAC Tripping on WILLIAMS, Pierre enforced 15 yards from the SAC40 to the SAC25, from the the spot of the ball when the foul occurred, 12 yards credited to passer and receiver on the play")) {
							continue; // TODO complicated string, weird
						}	
						if (playRawText.equals(
								"Wolff, Trey kickoff 0 yards to the TTU35, PENALTY TTU unsportsmanlike conduct (Ezukanma, Erik) 15 yards to the TTU20, NO PLAY")) {
							continue; // TODO kickoff unsportmanslike conduct after the play, needs to be split out
						}	
						if (playRawText.equals(
								"PENALTY Before the snap, MER False Start  enforced 5 yards from the MAI3 to the MAI8")) {
							continue; // TODO penalty on two pt conversion, no yardline given
						}
						if (playRawText.equals(
								"PENALTY TTU Block Below Waist (Shanley,Davis) 15 yards from TEN35 to TEN50. NO PLAY")) {
							continue; // TODO penalty after touchdown enforced on kickoff
						}	
						if (playRawText.equals(
								"CHANG, Ethan kickoff 62 yards to the COL3, SPENCER, Jake return 24 yards to the COL27 (JENNETTE, James). PENALTY COL Holding on SWEENEY, Christian enforced 0 yards WIL Personal Foul  enforced 0 yards after the change of possession from the end of the play at the COL27 to the COL27")) {
							continue; // TODO penalty after kickoff, 0 yards? weird
						}
						if (playRawText.equals(
								"S. Anderson rush for loss of 1 yard to the NCCU43 (Stephen Stokes)")) {
							continue; // TODO turnover on downs but not stated in play, needs context of next play in case of penalty
						}	
						if (playRawText.equals(
								"Braucht,Logan rush for 4 yards gain to the CIT43 (Shelton,Mason; Porter,EJ)")) {
							continue; // TODO turnover on downs but not stated in play, needs context of next play in case of penalty
						}							
						
						boolean updated = false;
						addTempoType(serviceRequest);
						cleanUpPlay(serviceRequest);
						cleanUpYards(serviceRequest);

						if (playRawText.contains(
								"Punt by at the DEL15 is blocked by M. Monios, recovered by MAI M. Moss at the DEL4, returned 0 yards to the DEL4, TOUCHDOWN MAI (Scoring play confirmed)")) {
							System.out.println(playRawText);
						}

						if (serviceRequest.getPlay().getDriveText().isEmpty()
								&& serviceRequest.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
								&& serviceRequest.getPlay().getPlayType() != PlayTypeEnum.PAT) {
							continue;
						}
						addEndYard(serviceRequest);
						boolean declinedNoPlay = pbpPenaltyParse.parsePenalty(serviceRequest);

						if (declinedNoPlay) {
							continue;
						}

						if (playRaw.getScoreText().contains("overturned")) {
							// TODO
//							System.out.println(playRawText);
							continue;
						}

						if (!serviceRequest.getPlay().getNoPlayPenalty() && serviceRequest.evalIfHasPenalty()) {
							continue;
						}

						if (!serviceRequest.getPlay().getNoPlayPenalty()) {
							updated = pbpKickoffParse.parseKickoff(serviceRequest, updated);
							updated = pbpPuntParse.parsePunt(serviceRequest, updated);
							updated = pbpFieldGoalParse.parseFieldGoal(serviceRequest, updated);
							updated = pbpPassParse.parsePass(serviceRequest, updated);
							updated = pbpRushParse.parseRush(serviceRequest, updated);
							pbpDefenseParse.parseDefense(serviceRequest);
							pbpOffenseParse.parseOffense(serviceRequest);
						}

						addStartYard(serviceRequest); // NOPLAY PENALTY
						addResultYardLine(serviceRequest); // NOPLAY PENALTY

						if (serviceRequest.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT
								&& serviceRequest.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
								&& serviceRequest.getPlay().getPlayCallType() != PlayCallTypeEnum.FG
								&& serviceRequest.getPlay().getPlayCallType() != PlayCallTypeEnum.PAT
								&& serviceRequest.getPlay().getPlayResult().getPlayResultYard() == null) {
							throw new IllegalArgumentException("HANDLE THIS");
						}
//						if (serviceRequest.getPlay().getPlayResult().getPlayResultYardLine() == null) {
//							throw new IllegalArgumentException("HANDLE THIS");
//						}
//						pbpValidateService.validate(serviceRequest);

						if (!updated && !serviceRequest.getPlay().getNoPlayPenalty()) {
							throw new IllegalArgumentException("HANDLE THIS");
						}

						drive.getDrivePlays().add(serviceRequest.getPlay());

					}

					gamePlays.getDrives().add(drive);
//					List<DrivePojo> reconciledDrives = reconcileDriveOnsideKick(drive, splitStart);
//					for (DrivePojo reconciledDrive : reconciledDrives) {
//						List<DrivePojo> tailedDrives = reconcileDriveTailKickoff(reconciledDrive);
//						for (DrivePojo tailedDrive : tailedDrives) {
//							validateDrive(tailedDrive);
//							gamePlays.getDrives().add(tailedDrive);
//						}
//					}

				}
			}
			game.setPlays(gamePlays);
		} catch (Exception e) {
			String errorStr = String.format("ERROR: PBP parse failed for %s vs %s with %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), e.toString());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}

	}

	private void addTempoType(PbpServiceRequestPojo serviceRequest) {
		try {
			String playRawText = serviceRequest.getPlayRawText();
			if (playRawText.contains("[NHSG")) {
				serviceRequest.getPlay().setPlayTempo(TempoTypeEnum.NHSG);
			} else if (playRawText.contains("[SG]")) {
				serviceRequest.getPlay().setPlayTempo(TempoTypeEnum.SG);
			} else if (playRawText.contains("[NH]")) {
				serviceRequest.getPlay().setPlayTempo(TempoTypeEnum.NH);
			} else if (playRawText.contains("[NHPUNT]")) {
				serviceRequest.getPlay().setPlayTempo(TempoTypeEnum.NHPUNT);
			} else {
				serviceRequest.getPlay().setPlayTempo(TempoTypeEnum.NONE);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), serviceRequest.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private String cleanUpPlayText(String playRawText, HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict,
			String possessionTeam, String defenseTeam) {
		try {
			playRawText = playRawText.replace("'", "");
			playRawText = playRawText.replace("Jr,", "Jr");
			playRawText = playRawText.replace("([^,])( Jr\\.) ", "$1 ");
			playRawText = playRawText.replaceAll("(([A-Z])\\.([A-Z][a-z]*))", "$2. $3");
			playRawText = playRawText.replaceAll("([A-Z])\\. ([A-Z])\\.", "$1$2");
			playRawText = playRawText.replaceAll("((, [A-Z])\\. )", "$2 ");
			playRawText = playRawText.replace(" . ", " ");

			playRawText = playRawText.replaceAll("( ?\\[NHSG\\] ?)", "");
			playRawText = playRawText.replaceAll("( ?\\[SG\\] ?)", "");
			playRawText = playRawText.replaceAll("( ?\\[NH\\] ?)", "");
			playRawText = playRawText.replaceAll("( ?\\[NHPUNT\\] ?)", "");

			playRawText = playRawText.replace("Vander Waal", "VanderWaal");

			playRawText = playRawText.replaceAll("[\\.]+$", "");
			playRawText = playRawText.replaceAll(String.format(" (to the|at)%s,? fumbled? by %s at forced",
					NcaaConstants.teamYardRegex, NcaaConstants.playerNameRegex), " $1 $2 fumbled by $3 at $2 forced");

			playRawText = playRawText.replace("Albany (NY)", "Albany");
			playRawText = playRawText.replace("HUNTER, Dae Dae", "HUNTER, DaeDae");

			playRawText = playRawText.replaceAll("([A-Z]\\. [A-Z][a-z]+),?( Jr\\.?)", "$1");
			playRawText = playRawText.replaceAll("([A-Z]\\. [A-Z][a-z]+),?( III)", "$1");
			playRawText = playRawText.replaceAll("([A-Z]{1,2} [aA-zZ]+) III", "$1");
			playRawText = playRawText.replaceAll("([A-Z]\\. [A-Z][a-z]+),?( II)", "$1");
			playRawText = playRawText.replaceAll("(, [A-Z] [A-Z][a-z]+),?( II) ", "$1 ");

			playRawText = playRawText.replaceAll("((, [A-Z])\\. )", "$2 ");
			playRawText = playRawText.replaceAll("([aA-zZ]+?) Jr.,([aA-zZ]*+)", "$1,$2");
			playRawText = playRawText.replace("C. La Chapelle", "C. LaChapelle");
			playRawText = playRawText.replace("A. Junior Ellis", "A. JuniorEllis");
			playRawText = playRawText.replace("A Junior Ellis", "A. JuniorEllis");
			playRawText = playRawText.replace("[NHPUNT]", "");

			playRawText = playRawText.replaceAll("([A-Z]\\.) ([aA-zZ]*+) IV ", "$1 $2 ");
			playRawText = playRawText.replaceAll("([A-Z][a-z]{0,3}\\.) ([aA-zZ]*+) IV ", "$1 $2 ");

			playRawText = playRawText.replaceAll("([aA-zZ]+?)-([aA-zZ]+?),([aA-zZ]*+)", "$1$2,$3");
			playRawText = playRawText.replace("Vander Esch,Caleb", "VanderEsch,Caleb");
			playRawText = playRawText.replace("Holmes,T Quele", "Holmes,TQuele");
			playRawText = playRawText.replace("Baker,Justin Richard", "Baker,JustinRichard");
			playRawText = playRawText.replaceAll(String.format("%s( Jr\\.)", NcaaConstants.playerNameRegex), "$1");
			playRawText = playRawText.replaceAll("([A-Z][aA-zZ']+ St)\\.", "$1");
			playRawText = playRawText.replaceAll("St\\. ([A-Z][a-z]+,[A-Z][a-z]+)", "St$1");

			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Rainbow-Douglas)((?:\\))|(?:;))", "$1Noah $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(STELLINI-SPLAN)((?:\\))|(?:;))", "$1Dominic $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Satterfield)((?:\\))|(?:;))", "$1Christopher $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Egbo)((?:\\))|(?:;))", "$1Anthony $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Kasteckas)((?:\\))|(?:;))", "$1Paulie $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Gallagher)((?:\\))|(?:;))", "$1Malcolm $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Taylor-Demerson)((?:\\))|(?:;))", "$1Dadrion $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Horton)((?:\\))|(?:;))", "$1Jadarius $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(HORTON)((?:\\))|(?:;))", "$1Jadarius $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(PSOTA)((?:\\))|(?:;))", "$1Trey $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(PURTY)((?:\\))|(?:;))", "$1Trejuan $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(ORLANDINI)((?:\\))|(?:;))", "$1Nathan $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(MARTINS)((?:\\))|(?:;))", "$1Austin $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(CHILTON)((?:\\))|(?:;))", "$1Austin $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(PEIFER)((?:\\))|(?:;))", "$1Jaxon $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(BITTNER)((?:\\))|(?:;))", "$1Brett $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(KESSEN)((?:\\))|(?:;))", "$1Kurt $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(SCIARRONI)((?:\\))|(?:;))", "$1Anthony $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(ANNIS)((?:\\))|(?:;))", "$1Evan $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(PRICE)((?:\\))|(?:;))", "$1Nicholas $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(EARLES)((?:\\))|(?:;))", "$1Donovan $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(FAIRBANK)((?:\\))|(?:;))", "$1Johnny $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(SHERMAN)((?:\\))|(?:;))", "$1Kohlton $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(REESE)((?:\\))|(?:;))", "$1Ollie $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(WASHINGTON)((?:\\))|(?:;))", "$1Robert $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(TWIGG)((?:\\))|(?:;))", "$1Gavin $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Smith-Bejgrowic)((?:\\))|(?:;))", "$1Kaejin $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Rogers-Anderson)((?:\\))|(?:;))", "$1Latheron $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(McCutcheon)((?:\\))|(?:;))", "$1Cameron $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Robertson)((?:\\))|(?:;))", "$1Paxton $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Acosta)((?:\\))|(?:;))", "$1Aaron $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Foglia)((?:\\))|(?:;))", "$1Nick $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Fleitman)((?:\\))|(?:;))", "$1Kyle $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Arrington)((?:\\))|(?:;))", "$1Mike $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Trimble)((?:\\))|(?:;))", "$1Austin $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(LABUS)((?:\\))|(?:;))", "$1Barret $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Abill)((?:\\))|(?:;))", "$1Jacob $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Wright)((?:\\))|(?:;))", "$1Teddy $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Quest)((?:\\))|(?:;))", "$1Jhonny $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Gecaj)((?:\\))|(?:;))", "$1Halil $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Jackson)((?:\\))|(?:;))", "$1Max $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Pinckney)((?:\\))|(?:;))", "$1Arthur $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(PARRISH)((?:\\))|(?:;))", "$1Ethan $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Coffindaffer)((?:\\))|(?:;))", "$1Kyle $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Harley)((?:\\))|(?:;))", "$1Tyhir $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Kim)((?:\\))|(?:;))", "$1Wookdong $2$3");
			playRawText = playRawText.replaceAll("((?:\\()?(?:;)?)(Hawkins-William)((?:\\))|(?:;))", "$1Darrel $2$3");

			playRawText = playRawText.replace(
					"D. Longman kickoff 64 yards to the COL1 out of bounds. Receiving team has elected to spot the ball at its 35-yard line",
					"D. Longman kickoff 64 yards to the COL1, out of bounds. PENALTY BC IP 0 yards to the COL35");

			playRawText = playRawText.replace(
					"C. Carrick kickoff 54 yards to the BRY11 out of bounds. Receiving team has elected to spot the ball at its 35-yard line",
					"C. Carrick kickoff 54 yards to the BRY11, out of bounds. PENALTY URI IP 0 yards to the BRY35");

			playRawText = playRawText.replaceAll(
					String.format("\\(%s\\)", teamAbbrevDict.get(possessionTeam).getShortname()), String.format(
							"\\(TEAM, %s\\)", teamAbbrevDict.get(possessionTeam).getShortname().replace(" ", "")));
			playRawText = playRawText.replaceAll(
					String.format("\\(%s\\)", teamAbbrevDict.get(defenseTeam).getShortname()),
					String.format("\\(TEAM, %s\\)", teamAbbrevDict.get(defenseTeam).getShortname().replace(" ", "")));
			playRawText = playRawText.replace(
					"Trayanum,DeaMonte rush for 11 yards gain to the SUU23, out of bounds PENALTY ASU Holding (Hodges,Curtis) ASU Holding declined 10 yards from SUU34 to SUU44. NO PLAY",
					"Trayanum,DeaMonte rush for 11 yards gain to the SUU23, out of bounds PENALTY ASU Holding (Hodges,Curtis) 10 yards from SUU34 to SUU44. NO PLAY");

			// TODO split this text into 2 plays
			playRawText = playRawText.replace(
					"PENALTY UTM personal foul (ANOMA, Eyabi) 15 yards to the 50 yardline, MUNSON, C kickoff 35 yards to the UTM15, RUCKER, Zion return 0 yards to the UTM15",
					"MUNSON, C kickoff 35 yards to the UTM15, RUCKER, Zion return 0 yards to the UTM15");

			playRawText = playRawText.replace(
					"Christiansen,Dayne rush for 18 yards gain to the SUU19 (Fields,Evan) PENALTY SUU Holding (Yarro,Canaan) 5 yards from SUU10 to SUU05",
					"Christiansen,Dayne rush for 18 yards gain to the SUU19 (Fields,Evan) PENALTY SUU Holding (Yarro,Canaan) 5 yards from SUU10 to SUU05. NO PLAY");

			playRawText = playRawText.replace(
					"PENALTY FUR unsportsmanlike conduct (Chase Abshier) 10 yards to the FUR10, 1ST DOWN FUR",
					"PENALTY FUR unsportsmanlike conduct (Chase Abshier) 10 yards to the FUR10");

//			playRawText = playRawText.replace("A. Rodriguez punt 42 yards to the HBU36, T Thompson return 0 yards to the HBU36 (T. Combs). PENALTY HBU Block below the waist on P. Kennedy enforced half the distance from the goal, 13 yards from the HBU26 to the HBU13 and results in automatic 1ST DOWN. NO PLAY (replay the down)", "A. Rodriguez punt 42 yards to the HBU36, T Thompson return 0 yards to the HBU36 (T. Combs). PENALTY HBU Block below the waist on P. Kennedy enforced half the distance from the goal, 13 yards from the HBU26 to the HBU13 and results in automatic 1ST DOWN");

			playRawText = playRawText.replaceAll("(punt.+return.+?1ST DOWN).? NO PLAY \\(replay the down\\)", "$1");
			playRawText = playRawText
					.replaceAll("(punt.+TOUCHDOWN.+nullified by penalty.+?).? NO PLAY \\(replay the down\\)", "$1");
			playRawText = playRawText
					.replaceAll("(rush.+TOUCHDOWN.+nullified by penalty.+?).? NO PLAY \\(replay the down\\)", "$1");

			playRawText = playRawText.replaceAll("\\(\\d{1,2}\\)", "");

			playRawText = playRawText.replace(
					"Punt by at the DEL15 is blocked by M. Monios, recovered by MAI M. Moss at the DEL4, returned 0 yards to the DEL4, TOUCHDOWN MAI (Scoring play confirmed)",
					"Punt by T. Pastula at the DEL15 is blocked by M. Monios, recovered by MAI M. Moss at the DEL4, returned 4 yards to the DEL0, TOUCHDOWN MAI (Scoring play confirmed)");

			return playRawText;
		} catch (Exception e) {
			String errorStr = String.format("ERROR: Text Cleanup Failed with %s - %s", e.toString(), playRawText);
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}

	}

	private void cleanUpYards(PbpServiceRequestPojo serviceRequest) {
		try {
			if (serviceRequest.getPlay().getPlayType() == PlayTypeEnum.PAT) {
				if ("".equals(serviceRequest.getPlay().getDriveText())
						|| serviceRequest.getPlay().getDriveText().endsWith("at -")) {
					serviceRequest.getPlay().setPlayStartYard(97);
					serviceRequest.getPlay().setPlayYardToGain(3);
				} else {
					String[] downAndDistance = pbpParsingUtils
							.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
					serviceRequest.getPlay()
							.setPlayStartYard(pbpParsingUtils.formatYardLine(downAndDistance[2],
									serviceRequest.getPossessionTeam(), serviceRequest.getDefenseTeam(),
									serviceRequest.getTeamAbbrevDict()));
					serviceRequest.getPlay().setPlayYardToGain(100 - serviceRequest.getPlay().getPlayStartYard());
				}
			} else if (serviceRequest.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {

			} else if (serviceRequest.getPlay().getPlayType() == PlayTypeEnum.PUNT) {
				String[] downAndDistance = pbpParsingUtils
						.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
				serviceRequest.getPlay().setPlayStartYard(
						pbpParsingUtils.formatYardLine(downAndDistance[2], serviceRequest.getDefenseTeam(),
								serviceRequest.getPossessionTeam(), serviceRequest.getTeamAbbrevDict()));
				serviceRequest.getPlay().setPlayStartDown(PlayDownEnum.valueOf(downAndDistance[0]));
				serviceRequest.getPlay().setPlayYardToGain(Integer.valueOf(downAndDistance[1]));

				if (serviceRequest.getPlay().getPlayYardToGain() == 0) {
					if (serviceRequest.getPlay().getPlayStartYard() > 80) {
						serviceRequest.getPlay().setPlayYardToGain(100 - serviceRequest.getPlay().getPlayStartYard());
					} else {
						throw new IllegalArgumentException("CATCH yard to gain 0 but not AND GOAL");
					}
				}
			} else {
				String[] downAndDistance = pbpParsingUtils
						.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
				serviceRequest.getPlay().setPlayStartYard(
						pbpParsingUtils.formatYardLine(downAndDistance[2], serviceRequest.getPossessionTeam(),
								serviceRequest.getDefenseTeam(), serviceRequest.getTeamAbbrevDict()));
				serviceRequest.getPlay().setPlayStartDown(PlayDownEnum.valueOf(downAndDistance[0]));
				serviceRequest.getPlay().setPlayYardToGain(Integer.valueOf(downAndDistance[1]));

				if (serviceRequest.getPlay().getPlayYardToGain() == 0) {
					if (serviceRequest.getPlay().getPlayStartYard() > 70) {
						serviceRequest.getPlay().setPlayYardToGain(100 - serviceRequest.getPlay().getPlayStartYard());
					} else {
						throw new IllegalArgumentException("CATCH yard to gain 0 but not AND GOAL");
					}
				}

			}
		} catch (Exception e) {
			String errorStr = String.format("ERROR: Text Cleanup Failed with %s - %s", e.toString(),
					serviceRequest.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
	
	private void cleanUpPlay(PbpServiceRequestPojo serviceRequest) {
		try {
			serviceRequest.getPlay().setPlayText(serviceRequest.getPlay().getPlayText().replace(
					"D.Longman kickoff 64 yards to the COL1 out of bounds. Receiving team has elected to spot the ball at its 35-yard line .",
					"D. Longman kickoff 64 yards to the COL1, out of bounds. PENALTY BC IP 0 yards to the COL35"));
			serviceRequest.getPlay().setPlayText(serviceRequest.getPlay().getPlayText().replace(
					"C.Carrick kickoff 54 yards to the BRY11 out of bounds. Receiving team has elected to spot the ball at its 35-yard line .",
					"C. Carrick kickoff 54 yards to the BRY11, out of bounds. PENALTY URI IP 0 yards to the BRY35"));
			serviceRequest.getPlay().setPlayText(serviceRequest.getPlay().getPlayText().replace(
					"Punt by at the DEL15 is blocked by M.Monios, recovered by MAI M.Moss at the DEL4, returned 0 yards to the DEL4, TOUCHDOWN MAI (Scoring play confirmed).",
					"Punt by T. Pastula at the DEL15 is blocked by M. Monios, recovered by MAI M. Moss at the DEL4, returned 4 yards to the DEL0, TOUCHDOWN MAI (Scoring play confirmed)"));

			
			
			serviceRequest.getPlay().getPlayResult().setPlayResultTurnover(false);
			if (serviceRequest.getPlayRawText().toUpperCase().contains("KICKOFF")) {
				String kickoffTeam = serviceRequest.getPossessionTeam();
				String kickoffReturnTeam = serviceRequest.getDefenseTeam();
				serviceRequest.setKickoffReturnTeam(kickoffReturnTeam);
				serviceRequest.setKickoffTeam(kickoffTeam);
				serviceRequest.getDrive().setKickoff(true);
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.KICKOFF);
				serviceRequest.getPlay().setPlayStartDown(PlayDownEnum.NA);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.NA);
				return;
			} else {
				serviceRequest.getDrive().setKickoff(false);
			}

			if (serviceRequest.getPlayRawText().toUpperCase().contains("PUNT")) {
				String puntTeam = serviceRequest.getPossessionTeam();
				String returnTeam = serviceRequest.getDefenseTeam();
				serviceRequest.setPuntReturnTeam(returnTeam);
				serviceRequest.setPuntTeam(puntTeam);
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.PUNT);
				serviceRequest.getPlay().getPlayResult().setPlayResultFirstDown(false);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.PUNT);
			} else if (serviceRequest.getPlayRawText().toUpperCase().contains(" FIELD GOAL ATTEMPT ")) {
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.OFFENSE);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.FG);
			} else if (serviceRequest.getPlayRawText().toUpperCase().contains(" PASSING ATTEMPT ")
					|| serviceRequest.getPlayRawText().toUpperCase().contains(" PASS ATTEMPT ")) {
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.PAT);
				serviceRequest.getPlay().setPlayStartDown(PlayDownEnum.NA);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.PASS);
			} else if (serviceRequest.getPlayRawText().toUpperCase().contains(" PASS ")
					|| serviceRequest.getPlayRawText().toUpperCase().contains(" SACKED ")
					|| serviceRequest.getPlayRawText().toUpperCase().contains(" SPIKE ")) {
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.OFFENSE);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.PASS);
			} else if (serviceRequest.getPlayRawText().toUpperCase().contains(" RUSHING ATTEMPT ")
					|| serviceRequest.getPlayRawText().toUpperCase().contains(" RUSH ATTEMPT ")) {
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.PAT);
				serviceRequest.getPlay().setPlayStartDown(PlayDownEnum.NA);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.RUN);
			} else if (serviceRequest.getPlayRawText().toUpperCase().contains(" RUSH ")
					|| serviceRequest.getPlayRawText().toUpperCase().contains(" RUSH FOR ")
					|| serviceRequest.getPlayRawText().toUpperCase().startsWith("RUSH FOR")
					|| serviceRequest.getPlayRawText().toUpperCase().toUpperCase().contains("KNEEL DOWN")
					|| serviceRequest.getPlayRawText().toUpperCase().toUpperCase().contains(" SCRAMBLES ")) {
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.OFFENSE);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.RUN);
			} else if (serviceRequest.getPlayRawText().toUpperCase().contains(" PAT ")
					|| serviceRequest.getPlayRawText().toUpperCase().contains(" KICK ATTEMPT ")) {
				serviceRequest.getPlay().setPlayType(PlayTypeEnum.PAT);
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.PAT);
				serviceRequest.getPlay().setPlayStartDown(PlayDownEnum.NA);
			} else if (serviceRequest.getPlayRawText().toUpperCase().startsWith("PENALTY")){
				
			} else {
				throw new IllegalArgumentException("Missing play type and play call type");
			}


		} catch (Exception e) {
			String errorStr = String.format("ERROR: Text Cleanup Failed with %s - %s", e.toString(),
					serviceRequest.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void reconcileDefPenaltyYards(PbpServiceRequestPojo serviceRequest) {
		try {
		} catch (Exception e) {
			String errorStr = String.format("ERROR: Text Cleanup Failed with %s - %s", e.toString(),
					serviceRequest.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}

	}

	private void addResultYardLine(PbpServiceRequestPojo serviceRequest) {
		try {
			boolean noPlay = serviceRequest.getPlay().getNoPlayPenalty();
			if (Objects.isNull(serviceRequest.getPlay().getPlayResult().getPlayResultYardLine())) {
				throw new IllegalArgumentException("asdfasdfasdf");
			}
		} catch (Exception e) {
			String errorStr = String.format("ERROR: Text Cleanup Failed with %s - %s", e.toString(),
					serviceRequest.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}

	}

	private void addStartYard(PbpServiceRequestPojo serviceRequest) {
		try {
			boolean noPlay = serviceRequest.getPlay().getNoPlayPenalty();
			if (Objects.isNull(serviceRequest.getPlay().getPlayStartYard())) {
				if (noPlay) {
					if (pbpParsingUtils.evalMatch(serviceRequest.getPlay().getPlayText(),
							String.format("%s (?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.teamYardRegex,
									NcaaConstants.teamYardRegex))) {
						String startYardString = pbpParsingUtils.extractCustom(serviceRequest.getPlay().getPlayText(),
								String.format("%s (?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.teamYardRegex,
										NcaaConstants.teamYardRegex),
								2);
						String[] startYardStringArrays = startYardString.split("\\|");
						int startYardMatches = startYardStringArrays.length;
						if (startYardMatches == 0) {
							throw new IllegalArgumentException("HANDLE THIS");
						}
						String[] startYardStringArray = startYardStringArrays[startYardMatches - 1].split("\\~");
						Integer startYard = pbpParsingUtils.formatYardLine(startYardStringArray[0],
								serviceRequest.getPossessionTeam(), serviceRequest.getDefenseTeam(),
								serviceRequest.getTeamAbbrevDict());
						serviceRequest.getPlay().setPlayStartYard(startYard);
					} else {
						throw new IllegalArgumentException("HANDLE THIS");
					}
				} else {
					throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
				}
			}
		} catch (Exception e) {
			String errorStr = String.format("ERROR: Text Cleanup Failed with %s - %s", e.toString(),
					serviceRequest.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}

	}

	private void addEndYard(PbpServiceRequestPojo serviceRequest) {
		try {
			if (pbpParsingUtils.evalMatch(serviceRequest.getPlay().getPlayText(), "Touchback.$")) {
				if (serviceRequest.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
					serviceRequest.getPlay().getPlayResult().setPlayResultYardLine(25);
				} else {
					serviceRequest.getPlay().getPlayResult().setPlayResultYardLine(20);
				}
			} else if (Objects.isNull(serviceRequest.getPlay().getPlayResult().getPlayResultYardLine())
					&& pbpParsingUtils.evalMatch(serviceRequest.getPlay().getPlayText(),
							String.format("(?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.teamYardRegex))) {
				String endYardString = pbpParsingUtils.extractCustom(serviceRequest.getPlay().getPlayText(),
						String.format("(?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.teamYardRegex), 1);
				String[] endYardStringArrays = endYardString.split("\\|");
				int endYardMatches = endYardStringArrays.length;
				if (endYardMatches == 0) {
					throw new IllegalArgumentException("HANDLE THIS");
				}
				String[] endYardStringArray = endYardStringArrays[endYardMatches - 1].split("\\~");
				Integer endYard = pbpParsingUtils.formatYardLine(endYardStringArray[0],
						serviceRequest.getPossessionTeam(), serviceRequest.getDefenseTeam(),
						serviceRequest.getTeamAbbrevDict());
				serviceRequest.getPlay().getPlayResult().setPlayResultYardLine(endYard);
			}
		} catch (Exception e) {
			String errorStr = String.format("ERROR: Text Cleanup Failed with %s - %s", e.toString(),
					serviceRequest.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}

	}

	private void validateDrive(DrivePojo drive) {
		LOG.log(Level.INFO, "-- Drive validation");

		if (StringUtils.isBlank(drive.getPossessionTeamId())) {
			throw new IllegalArgumentException("StringUtils.isBlank(drive.getPossessionTeamId())");
		}
		if (drive.getDriveStartTime() == null) {
			throw new IllegalArgumentException("drive.getDriveStartTime() == null");
		}
		if (StringUtils.isBlank(drive.getDriveEndTime())) {
			LOG.log(Level.WARNING, "StringUtils.isBlank(drive.getDriveEndTime())");
			// throw new
			// IllegalArgumentException("StringUtils.isBlank(drive.getDriveEndTime())");
		}
		if (StringUtils.isBlank(drive.getDriveTotalTime())) {
			LOG.log(Level.WARNING, "StringUtils.isBlank(drive.getDriveTotalTime())");
			// throw new
			// IllegalArgumentException("StringUtils.isBlank(drive.getDriveTotalTime())");
		}
		if (drive.getDriveStartPeriod() == null) {
			throw new IllegalArgumentException("drive.getDriveStartPeriod() == null");
		}
		if (drive.getDriveEndPeriod() == null) {
			LOG.log(Level.WARNING, "StringUtils.isBlank(drive.getDriveEndPeriod())");
			// throw new IllegalArgumentException("drive.getDriveEndPeriod() == null");
		}
		if (drive.getDrivePlays().size() == 0) {
			LOG.log(Level.WARNING, "drive.getDrivePlays().size() == 0");
			// throw new IllegalArgumentException("drive.getDrivePlays().size() == 0");
		}

		if (drive.isKickoff()) {
			if (drive.getDrivePlays().size() != 1) {
				LOG.log(Level.WARNING,
						String.format("drive.getDrivePlays().size() = %s", drive.getDrivePlays().size()));
//				throw new IllegalArgumentException(
//						String.format("drive.getDrivePlays().size() = %s", drive.getDrivePlays().size()));
			}
		}

		for (PlayPojo play : drive.getDrivePlays()) {
			if (StringUtils.isBlank(play.getPlayText())) {
				throw new IllegalArgumentException("StringUtils.isBlank(play.getPlayText())");
			}
			if (!drive.isKickoff()) {
				if (StringUtils.isBlank(play.getDriveText())) {
					throw new IllegalArgumentException("StringUtils.isBlank(play.getDriveText())");
				}
			}
			if (play.getPeriod() == null) {
				throw new IllegalArgumentException("play.getPeriod() == null");
			}
			if (play.getPlayType() == null) {
				throw new IllegalArgumentException("play.getPlayType() == null");
			}
			if (!PlayTypeEnum.KICKOFF.equals(play.getPlayType())) {
				if (play.getPlayStartDown() == null) {
					throw new IllegalArgumentException("play.getPlayStartDown() == null");
				}
				if (play.getPlayStartYard() == null) {
					throw new IllegalArgumentException("play.getPlayStartYard() == null");
				}
				if (play.getPlayYardToGain() == null) {
					throw new IllegalArgumentException("play.getPlayYardToGain() == null");
				}
				if (play.getPlayCallType() == null) {
					throw new IllegalArgumentException("play.getPlayCallType() == null");
				}
			}

			if (play.getPlayExpectedPoints() == null) {
				LOG.log(Level.WARNING, "play.getPlayExpectedPoints() == null");
			}

			if (play.getPlayResult() == null) {
				throw new IllegalArgumentException("play.getPlayResult() == null");
			}

			if (!PlayTypeEnum.KICKOFF.equals(play.getPlayType())) {
				if (play.getPlayResult().getPlayResultYard() == null) {
					throw new IllegalArgumentException("play.getPlayResult().getPlayResultYard() == null");
				}
				if (play.getPlayResult().isPlayResultFirstDown() == null) {
					throw new IllegalArgumentException("play.getPlayResult().isPlayResultFirstDown() == null");
				}
			}
			if (play.getPlayResult().isPlayResultTurnover() == null) {
				throw new IllegalArgumentException("play.getPlayResult().isPlayResultTurnover() == null");
			}
			if (play.getPlayResult().getPlayResultPoints() == null) {
				throw new IllegalArgumentException("play.getPlayResult().getPlayResultPoints() == null");
			}
			if (play.getPlayResult().getPlayResultExpectedPointsAdded() == null) {
				LOG.log(Level.WARNING, "play.getPlayResult().getPlayResultExpectedPointsAdded() == null");
			}

			if (play.getPlayerStat() == null) {
				throw new IllegalArgumentException("play.getPlayerStat() == null");
			}
			if (play.getPlayerStat().keySet().size() != 2) {
				throw new IllegalArgumentException("play.getPlayerStat().keySet().size() != 2");
			}
			if (!play.getPlayerStat().containsKey(drive.getPossessionTeamId())) {
				throw new IllegalArgumentException("!play.getPlayerStat().containsKey(drive.getPossessionTeamId())");
			}

			for (PlayerStatPojo stat : play.getPlayerStat().values()) {
				if (stat.getOffense() == null) {
					throw new IllegalArgumentException("stat.getOffense() == null");
				}

				if (stat.getDefense() == null) {
					throw new IllegalArgumentException("stat.getDefense() == null");
				}

				if (stat.getSpecialTeam() == null) {
					throw new IllegalArgumentException("stat.getSpecialTeam() == null");
				}
			}
		}

	}

	private List<DrivePojo> reconcileDriveOnsideKick(DrivePojo drive, String splitStart) {
		List<DrivePojo> returnDriveList = new ArrayList<DrivePojo>();
		Integer splitIndexOnside = drive.requireSplitOnside();
		if (splitIndexOnside != null) {
			DrivePojo splitDrive = new DrivePojo();
			splitDrive.setPossessionTeamId(drive.getPossessionTeamId());
			splitDrive.setDrivePlays(drive.getDrivePlays().subList(splitIndexOnside, drive.getDrivePlays().size()));
			drive.setDrivePlays(drive.getDrivePlays().subList(0, splitIndexOnside));
			drive.setKickoff(false);
			splitDrive.setKickoff(true);
			splitDrive.setDriveStartTime(pbpParsingUtils.convertMinSecToSec(splitStart));
			splitDrive.setDriveStartPeriod(drive.getDriveStartPeriod());
			returnDriveList.add(drive);
			returnDriveList.add(splitDrive);
			return returnDriveList;
		} else {
			returnDriveList.add(drive);
			return returnDriveList;
		}
	}

	private List<DrivePojo> reconcileDriveTailKickoff(DrivePojo drive) {
		List<DrivePojo> returnDriveList = new ArrayList<DrivePojo>();
		if (drive.requireSplitTailKickoff()) {
			DrivePojo tailKickoffDrive = new DrivePojo();
			tailKickoffDrive.setPossessionTeamId(drive.getPossessionTeamId());
			tailKickoffDrive.setDrivePlays(
					drive.getDrivePlays().subList(drive.getDrivePlays().size() - 1, drive.getDrivePlays().size()));
			drive.setDrivePlays(drive.getDrivePlays().subList(0, drive.getDrivePlays().size() - 1));
			tailKickoffDrive.setKickoff(true);
			tailKickoffDrive.setDriveStartPeriod(drive.getDriveStartPeriod());
			returnDriveList.add(drive);
			returnDriveList.add(tailKickoffDrive);
			return returnDriveList;
		} else {
			returnDriveList.add(drive);
			return returnDriveList;
		}
	}
}
