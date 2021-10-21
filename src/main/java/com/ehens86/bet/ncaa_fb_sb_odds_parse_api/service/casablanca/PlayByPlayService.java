package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
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
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpKickoffParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPassParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPuntParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpRushParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PlayByPlayService {
	private static final Logger LOG = Logger.getLogger(PlayByPlayService.class.toString());

	private final PbpKickoffParseService pbpKickoffParse;
	private final PbpParsingUtils pbpParsingUtils;
	private final PbpPassParseService pbpPassParse;
	private final PbpRushParseService pbpRushParse;
	private final PbpPuntParseService pbpPuntParse;

	public PlayByPlayService(PbpKickoffParseService pbpKickoffParse, PbpParsingUtils pbpParsingUtils,
			PbpPassParseService pbpPassParse, PbpRushParseService pbpRushParse, PbpPuntParseService pbpPuntParse) {
		this.pbpKickoffParse = pbpKickoffParse;
		this.pbpPuntParse = pbpPuntParse;
		this.pbpParsingUtils = pbpParsingUtils;
		this.pbpPassParse = pbpPassParse;
		this.pbpRushParse = pbpRushParse;
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
					// String defenseTeam = teamDict.keySet().stream().filter(id ->
					// !possessionTeam.equals(id)).collect(Collectors.toList()).get(0);

					//
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
//						if (playRawText.startsWith("(")) {
//							System.out.println("catch");
//						}
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
						} else if (playRawText.contains(" conversion ")) {
							continue;
						} else if (playRawText.contains("pass attempt Failed.")) {
							continue;
						} else if (playRawText.contains("pass attempt ")) {
							continue;
						} else if (playRawText.contains(" spiked")) {
							continue;
						} else if (playRawText.contains(" Spike")) {
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
						
//						System.out.println(playRawText);
						String[] playTackles = pbpParsingUtils.extractTackle(playRawText);
						
						playRawText=playRawText.replace("'", "");
						playRawText=playRawText.replace("Jr,", "Jr");
						playRawText=playRawText.replace("([^,])( Jr\\.) ", "$1 ");
						playRawText=playRawText.replaceAll("(([A-Z])\\.([A-Z][a-z]*))", "$2. $3");
						playRawText=playRawText.replaceAll("([A-Z])\\. ([A-Z])\\.", "$1$2");
						playRawText=playRawText.replaceAll("((, [A-Z])\\. )", "$2 ");
						playRawText=playRawText.replace(" . ", " ");

						playRawText=playRawText.replaceAll("[\\.]+$", "");
						playRawText=playRawText
								.replaceAll(String.format(" (to the|at)%s,? fumbled? by %s at forced", NcaaConstants.teamYardRegex,
										NcaaConstants.playerNameRegex), " $1 $2 fumbled by $3 at $2 forced");
						playRawText=playRawText.replace("Albany (NY)", "Albany");
						playRawText=playRawText.replaceAll("([A-Z]\\. [A-Z][a-z]+),?( Jr\\.?)", "$1");
						playRawText=playRawText.replaceAll("([A-Z]\\. [A-Z][a-z]+),?( III)", "$1");
						playRawText=playRawText.replaceAll("([A-Z]\\. [A-Z][a-z]+),?( II)", "$1");

						playRawText=playRawText.replace("C. La Chapelle", "C. LaChapelle");
						
						if (playRawText.startsWith(" ")) {
							playRawText=playRawText.stripLeading();
							play.setPlayText(playRawText);
						}

						PbpServiceRequestPojo serviceRequest = new PbpServiceRequestPojo(drive, play, playRaw,
								playRawText, playTackles, teamDict, teamAbbrevDict, possessionTeam, defenseTeam);

						if (playRaw.getScoreText().contains("NO PLAY")) {
//							System.out.println(playRawText);
//							pbpKickoffParse.parseKickoff(drive, play, playRaw, playRawText, playTackles, teamDict,
//									teamAbbrevDict, possessionTeam, defenseTeam);
						} else if (playRaw.getScoreText().contains("The ruling on the field has been overturned")) {
//							System.out.println(playRawText);
							continue;
						} else if (playRaw.getScoreText().contains("PENALTY")) {
//							System.out.println(playRawText);
							continue;
						} else if (playRaw.getScoreText().contains("kickoff")) {
							// System.out.println(playRawText);
							// pbpKickoffParse.parseKickoff(serviceRequest);
							continue;
						} else if (playRaw.getScoreText().contains("pass")) {
							System.out.println(playRawText);
							pbpPassParse.parsePass(serviceRequest);
							continue;
						} else if (playRaw.getScoreText().contains("rush for")) {
							//pbpRushParse.parseRush(serviceRequest);
							continue;
						} else if (playRaw.getScoreText().contains("punt")) {
							// System.out.println(playRawText);
							// pbpPuntParse.parsePunt(serviceRequest);
							continue;
						} else {
//							System.out.println(playRawText);
							continue;
						}
						drive.getDrivePlays().add(play);

					}

//					gamePlays.getDrives().add(drive);
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
