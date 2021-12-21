package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DriveResultEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.FieldZoneEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.TempoTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PbpPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PbpPlayerStatPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive.DriveTimesPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive.HomeAwayScoreMeta;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive.PlayParseStartTimeEndTime;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive.PlayResultMeta;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive.RollforwardLastPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPeriodPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPossessionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayTeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpDefenseParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpFieldGoalParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpKickoffParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpOffenseParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPassParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPenaltyParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpPuntParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpRushParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.PbpValidateService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PlayByPlayService {
	private static final String DRIVE_END_TIME_S = "Drive End Time: %s";

	private static final String LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S = "Last drive end yardline: %s | Drive start yardline: %s";

	private static final String DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE = "!drives.get(d-1).getDriveEndYardline().equals(drives.get(d).getDriveStartYardline())";

	private static final String STARTING_S = " -- Starting [%s]";

	private static final String PLAY_NUMBER_S = "Play Number: %s";

	private static final String TEXT_S = "Text: %s";

	private static final String DRIVE_NUMBER_S = "Drive Number: %s";

	private static final String QUARTER_START = "15:00";

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
	private final LoggingUtils loggingUtils;

	public PlayByPlayService(PbpKickoffParseService pbpKickoffParse, PbpParsingUtils pbpParsingUtils,
			PbpPassParseService pbpPassParse, PbpRushParseService pbpRushParse, PbpPuntParseService pbpPuntParse,
			PbpDefenseParseService pbpDefenseParse, PbpOffenseParseService pbpOffenseParse,
			PbpValidateService pbpValidateService, PbpFieldGoalParseService pbpFieldGoalParse,
			PbpPenaltyParseService pbpPenaltyParse, LoggingUtils loggingUtils) {
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
		this.loggingUtils = loggingUtils;
	}

	public void parsePbP(PlayByPlayPojo playByPlayRaw, GamePojo game) {
		try {

			if ("5851600".equals(game.getNcaaGameId()) || "5851699".equals(game.getNcaaGameId())) {
				return;
			}

			HashMap<String, HomeAwayEnum> teamDict = new HashMap<>();
			HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();

			for (PlayByPlayTeamPojo gameTeam : playByPlayRaw.getMeta().getTeams()) {
				teamDict.put(gameTeam.getId(), gameTeam.pullHomeAwayEnum());
				teamAbbrevDict.put(gameTeam.getId(), gameTeam);
			}
			List<List<PbpServiceRequestPojo>> gameInfo = driveFirstPassParse(playByPlayRaw, game, teamDict,
					teamAbbrevDict);

			/**
			 * Split TD/Extra Point into 2 plays where needed
			 */
			splitTdExtraPointPlays(gameInfo, teamDict, teamAbbrevDict);

			/**
			 * Set kickoff indexes
			 */
			/**
			 * Split kickoffs to seperate drives
			 */
			splitKickoffToNewDrive(gameInfo);

			/**
			 * Rollforward end yardline to fill in start yard where missing. addTempoType()
			 * cleanUpYards()
			 */
			rollforwardEndYardLine(gameInfo);

			/**
			 * Send for processing
			 */
			sendPlaysForProcessing(gameInfo);

			/**
			 * Combine drives split across quarters
			 */
			combineCarryOverQuarterDrives(gameInfo);

			/**
			 * Clean Up Scoring Plays, Extra Point, Penalties before kickoff
			 */
			scoringPlayCleanup(gameInfo);

			/**
			 * Add Turnover on Downs
			 */
			addTurnoverOnDowns(gameInfo);

			/**
			 * Validate Plays
			 */
			validatePlays(gameInfo);

			/**
			 * Add Time
			 */
			/**
			 * Add End Period for Drives
			 */
			/**
			 * Add End Time for Drives
			 */
			/**
			 * Add Total Time for Drives
			 */
			/**
			 * Roll Up to Drives
			 */
			List<DrivePojo> drives = applyTime(gameInfo);

			/**
			 * Validate Drives
			 */
			validateDrives(drives);

			PbpPojo gamePlays = new PbpPojo();
			gamePlays.setDrives(drives);
			game.setPlays(gamePlays);
		} catch (Exception e) {
			String errorStr = String.format("ERROR: PBP parse failed for %s vs %s with %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), e.toString());
			loggingUtils.logException(e, errorStr);
			throw new IllegalArgumentException(errorStr);
		}
	}

	/**
	 * Split TD/Extra Point into 2 plays where needed
	 */
	private void splitTdExtraPointPlays(List<List<PbpServiceRequestPojo>> gameInfo,
			HashMap<String, HomeAwayEnum> teamDict, HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {

			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			for (int d = 0; d < gameInfo.size(); d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				for (int p = 0; p < drive.size(); p++) {
					PbpServiceRequestPojo play = drive.get(p);
					if (play.getPlayRawText().contains("TOUCHDOWN")) {
						String newRawText = play.getPlayRawText().split("TOUCHDOWN")[1];
						newRawText = newRawText.replaceAll("^,", "");
						newRawText = newRawText.replace(" clock ", "");
						newRawText = newRawText.strip();
						newRawText = newRawText.replaceAll("^,", "");
						newRawText = newRawText.strip();
						if (!newRawText.isEmpty()) {
							newRawText = cleanUpPlayText(newRawText, teamAbbrevDict, play.getPossessionTeam(),
									play.getDefenseTeam());
							String[] playTackles = pbpParsingUtils.extractTackle(newRawText);

							PlayPojo newPlay = initializePlay(play.getPlay().getPeriod(), newRawText,
									play.getPlay().getDriveText().replace("AMP;", ""), teamDict,
									play.getPlay().getPlayResult().getPlayResultPossessionTeamId());

							PbpServiceRequestPojo newServiceRequest = new PbpServiceRequestPojo(
									new DrivePojo(play.getDrive().getPossessionTeamId(),
											play.getDrive().getDriveStartPeriod(), play.getDrive().getDriveStartTime(),
											play.getDrive().getDriveEndTime()),
									newPlay, play.getPlayRaw(), newRawText, playTackles, teamDict, teamAbbrevDict,
									play.getPossessionTeam(), play.getDefenseTeam());
							cleanUpPlay(newServiceRequest);
							addEndYard(newServiceRequest);
							pbpPenaltyParse.parsePenalty(newServiceRequest);
							newServiceRequest.getPlay().getPlayResult().setPlayResultPossessionTeamId(
									newServiceRequest.getPlay().getPlayStartPossessionTeamId());
							gameInfo.get(d).add(p + 1, newServiceRequest);
							play.setPlayRawText(play.getPlayRawText().replaceAll("(.+TOUCHDOWN)(.+)", "$1"));
						}
					}
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Set kickoff indexes
	 */
	private void splitKickoffToNewDrive(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			HashMap<Integer, List<Integer>> kickoffIndexes = new HashMap<>();
			HashMap<Integer, Integer> driveLengths = new HashMap<>();
			EnumMap<PlayPeriodEnum, Integer> drivePeriods = new EnumMap<>(PlayPeriodEnum.class);
			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);

				Integer playsInPossession = drive.size();
				driveLengths.put(d, playsInPossession);
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					if (!drivePeriods.containsKey(play.getDrive().getDriveStartPeriod())) {
						drivePeriods.put(play.getDrive().getDriveStartPeriod(), d);
					}
					if (play.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
						if (!kickoffIndexes.containsKey(d)) {
							List<Integer> kickoffList = new ArrayList<>();
							kickoffList.add(p);
							kickoffIndexes.put(d, kickoffList);
						} else {
							kickoffIndexes.get(d).add(p);
						}
					}
				}
			}
			applyNewKickoffIndexes(gameInfo, driveLengths, kickoffIndexes);
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Split kickoffs to seperate drives
	 */
	private void applyNewKickoffIndexes(List<List<PbpServiceRequestPojo>> gameInfo,
			HashMap<Integer, Integer> driveLengths, HashMap<Integer, List<Integer>> kickoffIndexes) {
		try {
			List<Integer> kickoffDrives = new ArrayList<>(kickoffIndexes.keySet());
			Collections.sort(kickoffDrives);
			Collections.reverse(kickoffDrives);
			for (Integer kickoffDriveIndex : kickoffDrives) {
				if (kickoffIndexes.get(kickoffDriveIndex).size() > 1) {
					String kickoffDriveInfo = String.format("Kickoff Drive Number: %s", kickoffDriveIndex);
					loggingUtils.logInfo(kickoffDriveInfo);
					String logInfo = String.format("Number of kickoffs found in drive: %s",
							kickoffIndexes.get(kickoffDriveIndex).size());
					loggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("handle multiple kickoffs in a drive");
				}

				if (driveLengths.get(kickoffDriveIndex) > 1) {
					List<PbpServiceRequestPojo> newKickoffDrive = new ArrayList<>();
					newKickoffDrive
							.add(gameInfo.get(kickoffDriveIndex).get(kickoffIndexes.get(kickoffDriveIndex).get(0)));
					gameInfo.add(kickoffDriveIndex + 1, newKickoffDrive);
					List<PbpServiceRequestPojo> oldKickoffDrive = gameInfo.get(kickoffDriveIndex);
					oldKickoffDrive.remove((int) kickoffIndexes.get(kickoffDriveIndex).get(0));
					gameInfo.set((int) kickoffDriveIndex, oldKickoffDrive);
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Rollforward end yardline to fill in start yard where missing. addTempoType()
	 * cleanUpYards()
	 */
	private void rollforwardEndYardLine(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {

			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			Integer drivesInGame = gameInfo.size();
			RollforwardLastPlayPojo rollforwardInfo = new RollforwardLastPlayPojo();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					rollforwardEndYardLinePlayHelper(play, rollforwardInfo);
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Rollforward end yardline to fill in start yard where missing. addTempoType()
	 * cleanUpYards()
	 */
	private void rollforwardEndYardLinePlayHelper(PbpServiceRequestPojo play, RollforwardLastPlayPojo rollforwardInfo) {
		try {
			if (play.getPlayRawText().isEmpty()) {
				String logInfo = String.format("Play Down & Distance: %s", play.getPlay().getDriveText());
				loggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("play raw text is empty");
			}
			if (play.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
					&& Objects.nonNull(play.getPlay().getPlayStartYard())) {
				play.getPlay().setPlayStartYard(rollforwardInfo.getLastPlayEndYardLine());
			}
			if (play.getPlay().getPlayType() != PlayTypeEnum.PAT && play.getPlay().getDriveText().endsWith("at -")) {
				if (Objects.isNull(rollforwardInfo.getLastDriveText())) {
					String logInfo = String.format("Play text: %s", play.getPlayRawText());
					loggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("missing needed down & distance text from prior play");
				}
				play.getPlay().setDriveText(
						play.getPlay().getDriveText().replace("-", rollforwardInfo.getLastDriveText().split(" ")[4]));
				rollforwardInfo.setLastDriveText(play.getPlay().getDriveText());
			} else {
				rollforwardInfo.setLastDriveText(play.getPlay().getDriveText());
			}
			if (Objects.nonNull(play.getPlay().getPlayResult().getPlayResultYardLine())) {
				rollforwardInfo.setLastPlayEndYardLine(play.getPlay().getPlayResult().getPlayResultYardLine());
			}
			addTempoType(play);
			cleanUpYards(play);
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Send for processing
	 */
	private void sendPlaysForProcessing(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			Integer drivesInGame = gameInfo.size();

			HomeAwayScoreMeta homeAwayScoreMeta = new HomeAwayScoreMeta();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				for (int p = 0; p < playsInPossession; p++) {
					boolean updated = false;
					PbpServiceRequestPojo play = drive.get(p);

					play.getPlay().setPlayStartHomeScore(homeAwayScoreMeta.getHomeScore());
					play.getPlay().setPlayStartAwayScore(homeAwayScoreMeta.getAwayScore());

					if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE
							.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
						loggingUtils.logInfo("----------------------------");
						loggingUtils.logInfo(String.format("Play Text: %s", play.getPlayRawText()));
						loggingUtils.logInfo(String.format("Play Type: %s", play.getPlay().getPlayType()));
						loggingUtils.logInfo(String.format("Play Call Type: %s", play.getPlay().getPlayCallType()));
					}
					if (Boolean.FALSE.equals(play.getPlay().getNoPlayPenalty())) {
						updated = pbpKickoffParse.parseKickoff(play, updated);
						updated = pbpPuntParse.parsePunt(play, updated);
						updated = pbpFieldGoalParse.parseFieldGoal(play, updated);
						updated = pbpPassParse.parsePass(play, updated);
						updated = pbpRushParse.parseRush(play, updated);
						pbpDefenseParse.parseDefense(play);
						pbpOffenseParse.parseOffense(play);
					}

					postProcessingApplyPlayScore(play, homeAwayScoreMeta);
					Integer scoreDiff = homeAwayScoreMeta.calcAbsScoreDiff();
					if ((PlayPeriodEnum.SECOND.equals(play.getPlay().getPeriod()) && scoreDiff > 38)
							|| (PlayPeriodEnum.THIRD.equals(play.getPlay().getPeriod()) && scoreDiff > 28)
							|| (PlayPeriodEnum.FOURTH.equals(play.getPlay().getPeriod()) && scoreDiff > 22)) {
						play.getPlay().setGarbageTime(true);
					} else {
						play.getPlay().setGarbageTime(false);
					}
					addStartYard(play);

					if (!PlayTypeEnum.KICKOFF.equals(play.getPlay().getPlayType())
							&& !PlayTypeEnum.PAT.equals(play.getPlay().getPlayType())) {
						if (play.getPlay().getPlayStartYard() <= 20) {
							play.getPlay().setPlayFieldZone(FieldZoneEnum.DEEP);
						} else if (play.getPlay().getPlayStartYard() <= 39) {
							play.getPlay().setPlayFieldZone(FieldZoneEnum.BACK);
						} else if (play.getPlay().getPlayStartYard() <= 60) {
							play.getPlay().setPlayFieldZone(FieldZoneEnum.MID);
						} else if (play.getPlay().getPlayStartYard() <= 81) {
							play.getPlay().setPlayFieldZone(FieldZoneEnum.FRONT);
						} else {
							play.getPlay().setPlayFieldZone(FieldZoneEnum.RED);
						}

						if (!PlayTypeEnum.PUNT.equals(play.getPlay().getPlayType())) {
							Integer yardToGain = play.getPlay().getPlayYardToGain();
							PlayDownEnum playDown = play.getPlay().getPlayStartDown();
							Integer yardGained = play.getPlay().getPlayResult().getPlayResultYard();
							boolean firstDown = play.getPlay().getPlayResult().isPlayResultFirstDown();
							Integer playPoints = play.getPlay().getPlayResult().getPlayResultPoints();

							if (PlayDownEnum.SECOND.equals(playDown) && yardToGain >= 7) {
								play.getPlay().setPassingDown(true);
							} else if (PlayDownEnum.THIRD.equals(playDown) && yardToGain >= 5) {
								play.getPlay().setPassingDown(true);
							} else if (PlayDownEnum.FOURTH.equals(playDown) && yardToGain >= 5) {
								play.getPlay().setPassingDown(true);
							} else {
								play.getPlay().setPassingDown(false);
							}

							if (playPoints == 6) {
								play.getPlay().getPlayResult().setPlayResultSuccess(true);
							} else if (firstDown) {
								play.getPlay().getPlayResult().setPlayResultSuccess(true);
							} else if (PlayDownEnum.FIRST.equals(playDown) && yardGained >= yardToGain / 2) {
								play.getPlay().getPlayResult().setPlayResultSuccess(true);
							} else if (PlayDownEnum.SECOND.equals(playDown) && yardGained >= yardToGain * .7) {
								play.getPlay().getPlayResult().setPlayResultSuccess(true);
							} else {
								play.getPlay().getPlayResult().setPlayResultSuccess(false);
							}

							if (PlayCallTypeEnum.RUN.equals(play.getPlay().getPlayCallType())) {
								boolean rushingPower = false;
								Boolean rushingPowerSuccess = null;

								if (play.getPlay().getPlayStartYard() >= 98) {
									rushingPower = true;
								} else if (play.getPlay().getPlayYardToGain() <= 2
										&& (PlayDownEnum.THIRD.equals(play.getPlay().getPlayStartDown())
												|| PlayDownEnum.FOURTH.equals(play.getPlay().getPlayStartDown()))) {
									rushingPower = true;
								}

								if (Boolean.TRUE.equals(rushingPower)) {
									if (play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense()
											.getRushingStat().get(0).getRushingFirstDown() == 1
											|| play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense()
													.getRushingStat().get(0).getRushingTouchdown() == 1) {
										rushingPowerSuccess = true;
									} else {
										rushingPowerSuccess = false;
									}
								}

								play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense()
										.getRushingStat().get(0).setRushingPower(rushingPower);
								play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense()
										.getRushingStat().get(0).setRushingPowerSuccess(rushingPowerSuccess);
								play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense()
										.getRushingStat().get(0)
										.setRushingSuccess(play.getPlay().getPlayResult().getPlayResultSuccess());
							}

							if (yardGained < 0) {
								play.getPlay().setDefeat(true);
							} else if (Boolean.TRUE.equals(play.getPlay().getPlayResult().isPlayResultTurnover())) {
								play.getPlay().setDefeat(true);
							} else if (PlayDownEnum.THIRD.equals(playDown) && !firstDown) {
								play.getPlay().setDefeat(true);
							} else if (PlayDownEnum.FOURTH.equals(playDown) && !firstDown) {
								play.getPlay().setDefeat(true);
							} else {
								play.getPlay().setDefeat(false);
							}

						}
					}

					if (!updated && Boolean.FALSE.equals(play.getPlay().getNoPlayPenalty())) {
						String logInfo = String.format("Play Text: %s", play.getPlayRawText());
						loggingUtils.logInfo(logInfo);
						throw new IllegalArgumentException("Play matched no conditions to enter processing.");
					}
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Add play result scores: home/away
	 */
	private void postProcessingApplyPlayScore(PbpServiceRequestPojo play, HomeAwayScoreMeta homeAwayScoreMeta) {
		try {
			if (HomeAwayEnum.HOME
					.equals(play.getTeamDict().get(play.getPlay().getPlayResult().getPlayResultPossessionTeamId()))) {
				if (play.getPlay().getPlayResult().getPlayResultPoints() >= 0) {
					homeAwayScoreMeta.setHomeScore(
							homeAwayScoreMeta.getHomeScore() + play.getPlay().getPlayResult().getPlayResultPoints());
				} else {
					homeAwayScoreMeta.setAwayScore(homeAwayScoreMeta.getAwayScore()
							+ -1 * play.getPlay().getPlayResult().getPlayResultPoints());
				}
			} else {
				if (play.getPlay().getPlayResult().getPlayResultPoints() >= 0) {
					homeAwayScoreMeta.setAwayScore(
							homeAwayScoreMeta.getAwayScore() + play.getPlay().getPlayResult().getPlayResultPoints());
				} else {
					homeAwayScoreMeta.setHomeScore(homeAwayScoreMeta.getHomeScore()
							+ -1 * play.getPlay().getPlayResult().getPlayResultPoints());
				}
			}

			play.getPlay().getPlayResult().setPlayResultHomeScore(homeAwayScoreMeta.getHomeScore());
			play.getPlay().getPlayResult().setPlayResultAwayScore(homeAwayScoreMeta.getAwayScore());
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Combine drives split across quarters
	 */
	private void combineCarryOverQuarterDrives(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			EnumMap<PlayPeriodEnum, DriveTimesPojo> periodDrives = pullPeriodStartEndDrive(gameInfo);
			List<Integer> drivesToCombine = new ArrayList<>();
			Integer drivesInGame = gameInfo.size();
			String lastPossessionTeam = null;
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				lastPossessionTeam = combineCarryOverQuarterDrivesHelper(drive, d, lastPossessionTeam, periodDrives,
						drivesToCombine, gameInfo);
			}

			Collections.sort(drivesToCombine);
			Collections.reverse(drivesToCombine);
			for (Integer dd : drivesToCombine) {
				gameInfo.get(dd - 1).addAll(gameInfo.get(dd));
				gameInfo.remove((int) dd);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Helper function per drive to split across quarters
	 */
	private String combineCarryOverQuarterDrivesHelper(List<PbpServiceRequestPojo> drive, Integer d,
			String lastPossessionTeam, EnumMap<PlayPeriodEnum, DriveTimesPojo> periodDrives,
			List<Integer> drivesToCombine, List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			Integer playsInPossession = drive.size();
			String possessionTeam = null;
			for (int p = 0; p < playsInPossession; p++) {
				PbpServiceRequestPojo play = drive.get(p);
				if (p == 0) {
					possessionTeam = play.getPossessionTeam();
				}
			}
			if (Objects.isNull(possessionTeam)) {
				String logInfo = String.format(DRIVE_NUMBER_S, d);
				loggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("missing needed possession team from prior drive");
			}
			if (d != 0 && possessionTeam.equals(lastPossessionTeam)) {
				if (periodDrives.get(PlayPeriodEnum.SECOND).getStartDrive().equals(d)
						|| periodDrives.get(PlayPeriodEnum.FOURTH).getStartDrive().equals(d)) {
					drivesToCombine.add(d);
				} else {
					String logInfo = String.format(DRIVE_NUMBER_S, d);
					loggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Same possession team across consecutive drives");
				}
			}
			if (gameInfo.get(d).get(0).getPlay().getPlayType() != PlayTypeEnum.KICKOFF) {
				lastPossessionTeam = gameInfo.get(d).get(0).getPossessionTeam();
			} else {
				lastPossessionTeam = null;
			}
			return lastPossessionTeam;
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	/**
	 * Clean Up Scoring Plays, Extra Point, Penalties before kickoff
	 */
	private void scoringPlayCleanup(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			List<PlayResultMeta> playResults = new ArrayList<>();
			Integer drivesInGame = gameInfo.size();

			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);

				Integer playsInPossession = drive.size();
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					if (play.getPlay().getPlayResult().getPlayResultPoints() != 0) {
						playResults.add(new PlayResultMeta(d, p, play.getPlay().getPlayResult().getPlayResultPoints()));
					}
				}
			}

			for (PlayResultMeta pp : playResults) {
				if (pp.getPoints() == 6) {
					scoringPlayCleanupTdHelper(gameInfo.get(pp.getDrive()), pp);
				} else if (pp.getPoints() == 1) {
					scoringPlayCleanupPatHelper(gameInfo.get(pp.getDrive()), pp);
				} else if (pp.getPoints() == 3) {
					scoringPlayCleanupFgHelper(gameInfo.get(pp.getDrive()), pp);
				} else {
					logInfoPlayText = String.format(TEXT_S,
							gameInfo.get(pp.getDrive()).get(pp.getPlay() + 1).getPlayRawText());
					loggingUtils.logInfo(logInfoPlayText);
					logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
					loggingUtils.logInfo(logInfoPlayNumber);
					logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
					loggingUtils.logInfo(logInfoDriveNumber);
					throw new IllegalArgumentException("Handle uncaught scoring logic");
				}

			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void scoringPlayCleanupTdHelper(List<PbpServiceRequestPojo> driveInfo, PlayResultMeta pp) {
		try {
			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			Integer tdDriveLength = driveInfo.size();
			if (tdDriveLength == pp.getPlay() + 1
					&& driveInfo.get(pp.getPlay()).getPlay().getPlayType() != PlayTypeEnum.PUNT) {
				logInfoPlayText = String.format(TEXT_S, driveInfo.get(pp.getPlay() + 1).getPlayRawText());
				loggingUtils.logInfo(logInfoPlayText);
				logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
				loggingUtils.logInfo(logInfoPlayNumber);
				logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
				loggingUtils.logInfo(logInfoDriveNumber);
				throw new IllegalArgumentException("What to do for TD as last play?");
			}
			for (PbpServiceRequestPojo ppp : driveInfo.subList(pp.getPlay() + 1, tdDriveLength)) {
				if (ppp.getPlay().getPlayType() == PlayTypeEnum.PAT) {
					break; // Stop search at PAT
				} else {
					logInfoPlayText = String.format(TEXT_S, ppp.getPlayRawText());
					loggingUtils.logInfo(logInfoPlayText);
					logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
					loggingUtils.logInfo(logInfoPlayNumber);
					logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
					loggingUtils.logInfo(logInfoDriveNumber);
					throw new IllegalArgumentException("What to do for non PAT play after TD in drive?");
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void scoringPlayCleanupPatHelper(List<PbpServiceRequestPojo> driveInfo, PlayResultMeta pp) {
		try {
			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			if (driveInfo.size() > pp.getPlay() + 1) {
				logInfoPlayText = String.format(TEXT_S, driveInfo.get(pp.getPlay() + 1).getPlayRawText());
				loggingUtils.logInfo(logInfoPlayText);
				logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
				loggingUtils.logInfo(logInfoPlayNumber);
				logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
				loggingUtils.logInfo(logInfoDriveNumber);
				throw new IllegalArgumentException("Need to handle play after PAT in a drive");
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void scoringPlayCleanupFgHelper(List<PbpServiceRequestPojo> driveInfo, PlayResultMeta pp) {
		try {
			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			if (driveInfo.size() > pp.getPlay() + 1) {
				logInfoPlayText = String.format(TEXT_S, driveInfo.get(pp.getPlay() + 1).getPlayRawText());
				loggingUtils.logInfo(logInfoPlayText);
				logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
				loggingUtils.logInfo(logInfoPlayNumber);
				logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
				loggingUtils.logInfo(logInfoDriveNumber);
				throw new IllegalArgumentException("What to do for play after FG in drive?");
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Validate Plays
	 */
	private void addTurnoverOnDowns(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				PbpServiceRequestPojo play = drive.get(playsInPossession - 1);
				if (play.getPlay().getPlayType() == PlayTypeEnum.OFFENSE
						&& play.getPlay().getPlayCallType() != PlayCallTypeEnum.FG
						&& play.getPlay().getPlayStartDown() == PlayDownEnum.FOURTH
						&& play.getPlay().getPlayYardToGain() > play.getPlay().getPlayResult().getPlayResultYard()) {
					play.getPlay().getPlayResult().setPlayResultTurnover(true);
					play.getPlay().getPlayResult().setPlayResultPossessionTeamId(play.getDefenseTeam());

				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Validate Plays
	 */
	private void validatePlays(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					pbpValidateService.validate(play);
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Add Time
	 */
	private List<DrivePojo> applyTime(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			EnumMap<PlayPeriodEnum, DriveTimesPojo> periodDrives = pullPeriodStartEndDrive(gameInfo);
			HashMap<Integer, DrivePojo> driveMap = new HashMap<>();

			List<DrivePojo> drives = new ArrayList<>();
			Integer drivesInGame = gameInfo.size();

			HomeAwayScoreMeta homeAwayScoreMeta = new HomeAwayScoreMeta();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				applyTimeDriveHelper(drive, d, driveMap, homeAwayScoreMeta);
			}

			driveMap.get(periodDrives.get(PlayPeriodEnum.SECOND).getEndDrive())
					.setDriveResult(DriveResultEnum.END_HALF);
			driveMap.get(periodDrives.get(PlayPeriodEnum.FOURTH).getEndDrive())
					.setDriveResult(DriveResultEnum.END_HALF);

			driveMap.get(periodDrives.get(PlayPeriodEnum.SECOND).getEndDrive()).setDriveEndTime(0);
			driveMap.get(periodDrives.get(PlayPeriodEnum.FOURTH).getEndDrive()).setDriveEndTime(0);

			for (int d = 0; d < drivesInGame; d++) {
				applyTimePerDriveHelper(driveMap, periodDrives, d);
			}

			loggingUtils.logInfo("- [aggregateDrives] -");
			for (int d = 0; d < drivesInGame; d++) {
				DrivePojo drive = driveMap.get(d);

				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					loggingUtils.logInfo(String.format(DRIVE_NUMBER_S, d));
					loggingUtils.logInfo(String.format("Drive Start Time: %s", drive.getDriveStartTime()));
					loggingUtils.logInfo(String.format(DRIVE_END_TIME_S, drive.getDriveEndTime()));
					loggingUtils.logInfo(String.format("Drive Start Yardline: %s", drive.getDriveStartYardline()));
					loggingUtils.logInfo(String.format("Drive End Yardline: %s", drive.getDriveEndYardline()));
				}

				drive.setDriveTotalTime(drive.getDriveStartTime() - drive.getDriveEndTime());
				drive.setDriveTotalYard(drive.getDriveEndYardline() - drive.getDriveStartYardline());

				List<PlayPojo> drivePlays = new ArrayList<>();
				for (PbpServiceRequestPojo p : gameInfo.get(d)) {
					drivePlays.add(p.getPlay());
				}
				drive.setDrivePlays(drivePlays);
				drive.setDriveOffensePlays(drive.getDrivePlays().stream()
						.filter(p -> PlayTypeEnum.OFFENSE.equals(p.getPlayType())).collect(Collectors.toList()).size());
				drives.add(drive);
			}

			return drives;
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	/**
	 * Apply time logic on second pass over drives
	 */
	private void applyTimePerDriveHelper(HashMap<Integer, DrivePojo> driveMap,
			EnumMap<PlayPeriodEnum, DriveTimesPojo> periodDrives, Integer d) {
		try {
			if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
				loggingUtils.logInfo(String.format("-- Drive Number: %s", d));
			}
			// Skip for first drive of the second half
			// Set next drive start time based on drive end time
			if (driveMap.containsKey(d + 1) && periodDrives.get(PlayPeriodEnum.THIRD).getStartDrive() != d + 1
					&& driveMap.get(d + 1).getDriveStartTime() > driveMap.get(d).getDriveEndTime()) {
				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					loggingUtils.logInfo(String.format(DRIVE_END_TIME_S, driveMap.get(d).getDriveEndTime()));
					loggingUtils.logInfo(
							String.format("Next Drive Start Time: %s", driveMap.get(d + 1).getDriveStartTime()));
				}
				driveMap.get(d + 1).setDriveStartTime(driveMap.get(d).getDriveEndTime());
			}
			// Skip for last drive of the first half
			// Set drive time based on next drive end time
			if (driveMap.containsKey(d + 1) && periodDrives.get(PlayPeriodEnum.SECOND).getEndDrive() != d
					&& driveMap.get(d + 1).getDriveStartTime() < driveMap.get(d).getDriveEndTime()) {
				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					loggingUtils.logInfo(String.format(DRIVE_END_TIME_S, driveMap.get(d).getDriveEndTime()));
					loggingUtils.logInfo(
							String.format("Next Drive Start Time: %s", driveMap.get(d + 1).getDriveStartTime()));
				}
				driveMap.get(d).setDriveEndTime(driveMap.get(d + 1).getDriveStartTime());
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Add Time - Per Drive
	 */
	private void applyTimeDriveHelper(List<PbpServiceRequestPojo> drive, Integer d,
			HashMap<Integer, DrivePojo> driveMap, HomeAwayScoreMeta homeAwayScoreMeta) {
		try {

			Map<String, HomeAwayEnum> teamDict = drive.get(0).getTeamDict();

			Integer playsInPossession = drive.size();
			DrivePojo standardDrive = new DrivePojo();
			standardDrive.setDriveResultPoint(0);
			standardDrive.setDriveOffenseYard(0);
			standardDrive.setDriveStartHomeScore(homeAwayScoreMeta.getHomeScore());
			standardDrive.setDriveStartAwayScore(homeAwayScoreMeta.getAwayScore());
			standardDrive.setDriveStartPeriod(drive.get(0).getDrive().getDriveStartPeriod());
			standardDrive.setDriveEndPeriod(drive.get(playsInPossession - 1).getDrive().getDriveStartPeriod());

			for (int p = 0; p < playsInPossession; p++) {
				PbpServiceRequestPojo play = drive.get(p);
				applyTimePlayHelper(play, p, standardDrive);
				if (p < playsInPossession - 1 && standardDrive.getDriveResult() == DriveResultEnum.TURNOVER) {
					String logInfo = String.format("Play text: %s", play.getPlayRawText());
					loggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Turnover is not the last play of drive");
				}
			}
			if (Objects.isNull(standardDrive.getKickoff())) {
				standardDrive.setKickoff(false);
			}
			if (Objects.isNull(standardDrive.getDriveResultScoringOpp())) {
				standardDrive.setDriveResultScoringOpp(false);
			}

			if (HomeAwayEnum.HOME.equals(teamDict.get(standardDrive.getPossessionTeamId()))) {
				if (standardDrive.getDriveResultPoint() >= 0) {
					homeAwayScoreMeta
							.setHomeScore(homeAwayScoreMeta.getHomeScore() + standardDrive.getDriveResultPoint());
				} else {
					homeAwayScoreMeta
							.setAwayScore(homeAwayScoreMeta.getAwayScore() + -1 * standardDrive.getDriveResultPoint());
				}
			} else {
				if (standardDrive.getDriveResultPoint() >= 0) {
					homeAwayScoreMeta
							.setAwayScore(homeAwayScoreMeta.getAwayScore() + standardDrive.getDriveResultPoint());
				} else {
					homeAwayScoreMeta
							.setHomeScore(homeAwayScoreMeta.getHomeScore() + -1 * standardDrive.getDriveResultPoint());
				}
			}

			standardDrive.setDriveResultHomeScore(homeAwayScoreMeta.getHomeScore());
			standardDrive.setDriveResultAwayScore(homeAwayScoreMeta.getAwayScore());

			driveMap.put(d, standardDrive);
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Add Time - Per Play
	 */
	private void applyTimePlayHelper(PbpServiceRequestPojo play, Integer p, DrivePojo standardDrive) {
		try {

			applyTimePlayHelperFirstPlay(play, standardDrive, p);

			if (Objects.nonNull(standardDrive.getDriveResult())
					&& standardDrive.getDriveResult() != DriveResultEnum.TD) {
				String logInfo = String.format("Play text: %s | Drive result: %s", play.getPlayRawText(),
						standardDrive.getDriveResult());
				loggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("Drive result already set before drive complete");
			}
			if (play.getDrive().getDriveEndTime() < standardDrive.getDriveEndTime()) {
				standardDrive.setDriveEndTime(play.getDrive().getDriveEndTime());
			}

			if (Boolean.TRUE.equals(play.getDrive().getKickoff())) {
				standardDrive.setKickoff(true);
			}

			applyTimePlayHelperApplyDriveResult(play, standardDrive);

			if (Objects.nonNull(play.getPlay().getPlayResult().getPlayResultYardLine())) {
				if (PlayCallTypeEnum.PUNT.equals(play.getPlay().getPlayCallType())) {
					standardDrive.setDriveEndYardline(100 - play.getPlay().getPlayResult().getPlayResultYardLine());
				} else {
					standardDrive.setDriveEndYardline(play.getPlay().getPlayResult().getPlayResultYardLine());
				}
			}

			if (play.getPlay().getPlayType() == PlayTypeEnum.OFFENSE) {
				if (play.getPlay().getPlayStartYard() >= 60) {
					standardDrive.setDriveResultScoringOpp(true);
				}
				if (Boolean.FALSE.equals(play.getPlay().getPlayResult().isPlayResultTurnover())) {
					standardDrive.addDriveOffenseYard(play.getPlay().getPlayResult().getPlayResultYard());
				}
			}

			if (PlayCallTypeEnum.PUNT.equals(play.getPlay().getPlayCallType())) {
				standardDrive.addDriveResultPoint(play.getPlay().getPlayResult().getPlayResultPoints() * -1);
			} else {
				standardDrive.addDriveResultPoint(play.getPlay().getPlayResult().getPlayResultPoints());
			}

			if (!standardDrive.getPossessionTeamId().equals(play.getPossessionTeam())
					&& play.getPlay().getPlayType() != PlayTypeEnum.PUNT) {
				throw new IllegalArgumentException("drive includes mulitple possession teams");
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Extra logic for first play in drive
	 */
	private void applyTimePlayHelperFirstPlay(PbpServiceRequestPojo play, DrivePojo standardDrive, Integer p) {
		try {
			if (p == 0) {
				standardDrive.setDriveStartTime(play.getDrive().getDriveStartTime());
				standardDrive.setDriveEndTime(play.getDrive().getDriveEndTime());
				standardDrive.setPossessionTeamId(play.getPossessionTeam());
				standardDrive.setDriveStartYardline(play.getPlay().getPlayStartYard());
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Add Play Result - Per Play
	 */
	private void applyTimePlayHelperApplyDriveResult(PbpServiceRequestPojo play, DrivePojo standardDrive) {
		try {
			if (play.getPlay().getPlayResult().getPlayResultPoints() == 6) {
				standardDrive.setDriveResult(DriveResultEnum.TD);
			} else if (play.getPlay().getPlayType() == PlayTypeEnum.PUNT) {
				standardDrive.setDriveResult(DriveResultEnum.PUNT);
			} else if (Boolean.TRUE.equals(play.getPlay().getPlayResult().isPlayResultTurnover())) {
				standardDrive.setDriveResult(DriveResultEnum.TURNOVER);
			} else if (play.getPlay().getPlayCallType() == PlayCallTypeEnum.FG) {
				standardDrive.setDriveResult(DriveResultEnum.FG);
			} else if (PlayTypeEnum.KICKOFF.equals(play.getPlay().getPlayType())) {
				standardDrive.setDriveResult(DriveResultEnum.KICKOFF);
			} else if (Objects.isNull(standardDrive.getDriveResult())
					&& PlayTypeEnum.PAT.equals(play.getPlay().getPlayType())) {
				standardDrive.setDriveResult(DriveResultEnum.PAT);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Generate MAP with the start drive and end drive indices for each quarter
	 */
	private EnumMap<PlayPeriodEnum, DriveTimesPojo> pullPeriodStartEndDrive(
			List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			EnumMap<PlayPeriodEnum, DriveTimesPojo> periodDrives = new EnumMap<>(PlayPeriodEnum.class);
			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					if (p == 0 && !periodDrives.containsKey(play.getDrive().getDriveStartPeriod())) {
						periodDrives.put(play.getDrive().getDriveStartPeriod(), new DriveTimesPojo(d));
					}
					if (periodDrives.containsKey(play.getDrive().getDriveStartPeriod())) {
						periodDrives.get(play.getDrive().getDriveStartPeriod()).setEndDrive(d);
					}
				}
			}
			return periodDrives;
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private PlayPojo initializePlay(PlayPeriodEnum playPeriod, String playRawText, String driveText,
			HashMap<String, HomeAwayEnum> teamDict, String possessionTeam) {
		PlayPojo play = new PlayPojo();
		play.setPeriod(playPeriod);
		play.setPlayText(playRawText);
		play.setDriveText(driveText.replace("AMP;", ""));
		play.setPlayStartPossessionTeamId(possessionTeam);
		for (String teamId : teamDict.keySet()) {
			play.getPlayerStat().put(teamId, new PbpPlayerStatPojo());
		}
		return play;
	}

	private List<List<PbpServiceRequestPojo>> driveFirstPassParse(PlayByPlayPojo playByPlayRaw, GamePojo game,
			HashMap<String, HomeAwayEnum> teamDict, HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			List<List<PbpServiceRequestPojo>> gameInfo = new ArrayList<>();
			Integer periodsInGame = playByPlayRaw.getPeriods().size();
			for (int q = 0; q < periodsInGame; q++) {
				PlayByPlayPeriodPojo period = playByPlayRaw.getPeriods().get(q);
				PlayPeriodEnum playPeriod = period.getTitleEnum();

				Integer drivesInPeriod = period.getPossessions().size();

				PlayParseStartTimeEndTime startTimeEndTime = new PlayParseStartTimeEndTime(
						pbpParsingUtils.convertMinSecToSec(QUARTER_START, playPeriod),
						pbpParsingUtils.convertMinSecToSec(QUARTER_START, playPeriod));
				for (int d = 0; d < drivesInPeriod; d++) {
					PlayByPlayPossessionPojo possession = period.getPossessions().get(d);
					driveFirstPassParseDriveHelper(game, possession, playByPlayRaw, startTimeEndTime, playPeriod,
							teamDict, teamAbbrevDict, gameInfo);
				}
			}
			return gameInfo;
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private void driveFirstPassParseDriveHelper(GamePojo game, PlayByPlayPossessionPojo possession,
			PlayByPlayPojo playByPlayRaw, PlayParseStartTimeEndTime startTimeEndTime, PlayPeriodEnum playPeriod,
			HashMap<String, HomeAwayEnum> teamDict, HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict,
			List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			List<PbpServiceRequestPojo> driveInfo = new ArrayList<>();
			String possessionTeam;
			if ("5851706".equals(game.getNcaaGameId()) && "".equals(possession.getTeamId())) {
				possessionTeam = "1195";
			} else {
				possessionTeam = possession.getTeamId();
			}
			String defenseTeam = playByPlayRaw.pullOpponent(possessionTeam);
			if (!QUARTER_START.equals(possession.getTime())) {
				startTimeEndTime.setStartTime(pbpParsingUtils.convertMinSecToSec(possession.getTime(), playPeriod));
				startTimeEndTime.setEndTime(pbpParsingUtils.convertMinSecToSec(possession.getTime(), playPeriod));
			}

			Integer playsInPossession = possession.getPlays().size();
			for (int p = 0; p < playsInPossession; p++) {
				PlayByPlayPlayPojo playRaw = possession.getPlays().get(p);
				driveFirstPassParsePlayHelper(playRaw, playPeriod, possessionTeam, defenseTeam, teamDict,
						teamAbbrevDict, startTimeEndTime, driveInfo);
			}
			if (!driveInfo.isEmpty()) {
				gameInfo.add(driveInfo);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void driveFirstPassParsePlayHelper(PlayByPlayPlayPojo playRaw, PlayPeriodEnum playPeriod,
			String possessionTeam, String defenseTeam, HashMap<String, HomeAwayEnum> teamDict,
			HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict, PlayParseStartTimeEndTime startTimeEndTime,
			List<PbpServiceRequestPojo> driveInfo) {
		try {

			PlayPojo play;
			String playRawText = playRaw.getScoreText().replace("  ", " ").replaceAll("#\\d{1,2} ", "")
					.replace(" III,", ",").replace(" Jr ", " ");

			String suffixReplacementRegexCondition = "[A-Z]\\.[A-Z][a-z]+ [A-Z] ";
			if (pbpParsingUtils.evalMatch(playRawText, suffixReplacementRegexCondition)) {
				String suffixReplaceFull = pbpParsingUtils
						.extractCustom(playRawText, suffixReplacementRegexCondition, 1).split("\\~")[0];
				String suffixReplaceNew = suffixReplaceFull.split(" ")[0];
				playRawText = playRawText.replace(suffixReplaceFull, suffixReplaceNew);
			}

			String endTimeRegexCondition = "\\(?(\\d{1,2}:\\d{1,2})\\)?";
			if (pbpParsingUtils.evalMatch(playRawText, endTimeRegexCondition)) {
				String timeString = pbpParsingUtils.extractCustom(playRawText, endTimeRegexCondition, 1).split("~")[0];
				startTimeEndTime.setEndTime(pbpParsingUtils.convertMinSecToSec(timeString, playPeriod));
			}
			playRawText = playRawText.replaceAll("\\(?\\d{1,2}:\\d{1,2}\\)? ?", "");

			if (filterPlaysByText(playRawText)) {
				play = initializePlay(playPeriod, playRawText, playRaw.getDriveText(), teamDict, possessionTeam);
			} else {
				return;
			}

			playRawText = cleanUpPlayText(playRawText, teamAbbrevDict, possessionTeam, defenseTeam);
			String[] playTackles = pbpParsingUtils.extractTackle(playRawText);

			if (playRawText.startsWith(" ")) {
				playRawText = playRawText.stripLeading();
				play.setPlayText(playRawText);
			}

			PbpServiceRequestPojo serviceRequest = new PbpServiceRequestPojo(
					new DrivePojo(possessionTeam, playPeriod, startTimeEndTime.getStartTime(),
							startTimeEndTime.getEndTime()),
					play, playRaw, playRawText, playTackles, teamDict, teamAbbrevDict, possessionTeam, defenseTeam);

			cleanUpPlay(serviceRequest);

			if (serviceRequest.getPlay().getDriveText().isEmpty()
					&& serviceRequest.getPlay().getPlayType() != PlayTypeEnum.KICKOFF
					&& serviceRequest.getPlay().getPlayType() != PlayTypeEnum.PAT) {
				return;
			}
			addEndYard(serviceRequest);
			serviceRequest.getPlay().getPlayResult().setPlayResultPossessionTeamId(serviceRequest.getPossessionTeam());
			boolean declinedNoPlay = pbpPenaltyParse.parsePenalty(serviceRequest);
			if (declinedNoPlay) {
				return;
			}

			driveInfo.add(serviceRequest);
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private boolean filterPlaysByText(String playRawText) {
		try {
			if (playRawText.contains("Start of ") && playRawText.contains("quarter")) {
				return false;
			} else if (pbpParsingUtils.evalMatch(playRawText, "w[io]ns?(?: the)?(?: coin)? toss")) {
				return false;
			} else if (playRawText.contains(" drive start at ")) {
				return false;
			} else if (playRawText.contains("End of game")) {
				return false;
			} else if (playRawText.contains("will receive")) {
				return false;
			} else if (playRawText.contains("captains")) {
				return false;
			} else if (playRawText.contains("free kick")) {
				return false; // TODO account for these
			} else if (playRawText.startsWith("End of ")) {
				return false;
			} else if (playRawText.startsWith("Timeout")) {
				return false; // TODO account for these
			} else if (pbpParsingUtils.evalMatch(playRawText, "^[aA-zZ]{4} ball on [aA-zZ]{4}\\d{2}\\.?$")) {
				return false;
			} else if (pbpParsingUtils.evalMatch(playRawText, "^(\\d[a-z]{2}) and (\\d+)\\.$")) {
				return false;
			} else if (pbpParsingUtils.evalMatch(playRawText, "^[A-Z]{3} Capt. [\\d,]+$")) {
				return false;
			} else {
				return true;
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
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
			loggingUtils.logException(e, e.toString());

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
					NcaaConstants.TEAM_YARD_REGEX, NcaaConstants.PLAYER_NAME_REGEX),
					" $1 $2 fumbled by $3 at $2 forced");

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
			playRawText = playRawText.replaceAll(String.format("%s( Jr\\.)", NcaaConstants.PLAYER_NAME_REGEX), "$1");
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

			playRawText = playRawText.replace(
					"Christiansen,Dayne rush for 18 yards gain to the SUU19 (Fields,Evan) PENALTY SUU Holding (Yarro,Canaan) 5 yards from SUU10 to SUU05",
					"Christiansen,Dayne rush for 18 yards gain to the SUU19 (Fields,Evan) PENALTY SUU Holding (Yarro,Canaan) 5 yards from SUU10 to SUU05. NO PLAY");

			playRawText = playRawText.replace(
					"PENALTY FUR unsportsmanlike conduct (Chase Abshier) 10 yards to the FUR10, 1ST DOWN FUR",
					"PENALTY FUR unsportsmanlike conduct (Chase Abshier) 10 yards to the FUR10");

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
			loggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}

	}

	private void cleanUpYards(PbpServiceRequestPojo serviceRequest) {
		try {
			if (serviceRequest.getPlay().getPlayType() == PlayTypeEnum.PAT) {
				cleanUpYardsPatHelper(serviceRequest);
			} else if (serviceRequest.getPlay().getPlayType() == PlayTypeEnum.KICKOFF) {
				// Nothing needed here for now
			} else if (serviceRequest.getPlay().getPlayType() == PlayTypeEnum.PUNT) {
				cleanUpYardsPuntHelper(serviceRequest);
			} else {
				cleanUpYardsOffenseHelper(serviceRequest);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void cleanUpYardsPatHelper(PbpServiceRequestPojo serviceRequest) {
		try {
			if ("".equals(serviceRequest.getPlay().getDriveText())
					|| serviceRequest.getPlay().getDriveText().endsWith("at -")) {
				serviceRequest.getPlay().setPlayStartYard(97);
				serviceRequest.getPlay().setPlayYardToGain(3);
			} else {
				String[] downAndDistance = pbpParsingUtils
						.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
				serviceRequest.getPlay().setPlayStartYard(
						pbpParsingUtils.formatYardLine(downAndDistance[2], serviceRequest.getPossessionTeam(),
								serviceRequest.getDefenseTeam(), serviceRequest.getTeamAbbrevDict()));
				serviceRequest.getPlay().setPlayYardToGain(100 - serviceRequest.getPlay().getPlayStartYard());
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void cleanUpYardsPuntHelper(PbpServiceRequestPojo serviceRequest) {
		try {
			String[] downAndDistance = pbpParsingUtils.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
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
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void cleanUpYardsOffenseHelper(PbpServiceRequestPojo serviceRequest) {
		try {
			String[] downAndDistance = pbpParsingUtils.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
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
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

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

			cleanUpPlayKickoffHelper(serviceRequest);
			if (Boolean.FALSE.equals(serviceRequest.getDrive().getKickoff())) {
				cleanUpPlayNonKickoffHelper(serviceRequest);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void cleanUpPlayKickoffHelper(PbpServiceRequestPojo serviceRequest) {
		try {
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
			} else {
				serviceRequest.getDrive().setKickoff(false);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void cleanUpPlayNonKickoffHelper(PbpServiceRequestPojo serviceRequest) {
		try {
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
			} else if (serviceRequest.getPlayRawText().toUpperCase().startsWith("PENALTY")) {
				// Nothing needed here for now
			} else {
				throw new IllegalArgumentException("Missing play type and play call type");
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

		}
	}

	private void addStartYard(PbpServiceRequestPojo serviceRequest) {
		try {
			boolean noPlay = serviceRequest.getPlay().getNoPlayPenalty();
			if (Objects.isNull(serviceRequest.getPlay().getPlayStartYard()) && noPlay) {
				String regexCondition = String.format("%s (?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.TEAM_YARD_REGEX,
						NcaaConstants.TEAM_YARD_REGEX);
				if (pbpParsingUtils.evalMatch(serviceRequest.getPlay().getPlayText(), regexCondition)) {
					String startYardString = pbpParsingUtils.extractCustom(serviceRequest.getPlay().getPlayText(),
							regexCondition, 2);
					String[] startYardStringArrays = startYardString.split("\\|");
					int startYardMatches = startYardStringArrays.length;
					if (startYardMatches == 0) {
						throw new IllegalArgumentException("Start regex extraction length = 0");
					}
					String[] startYardStringArray = startYardStringArrays[startYardMatches - 1].split("\\~");
					Integer startYard = pbpParsingUtils.formatYardLine(startYardStringArray[0],
							serviceRequest.getPossessionTeam(), serviceRequest.getDefenseTeam(),
							serviceRequest.getTeamAbbrevDict());
					serviceRequest.getPlay().setPlayStartYard(startYard);
				} else {
					throw new IllegalArgumentException(
							String.format("Regex failed to match.  Input String: %s | Regex Condition: %s",
									serviceRequest.getPlay().getPlayText(), regexCondition));
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());

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
							String.format("(?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.TEAM_YARD_REGEX))) {
				String endYardString = pbpParsingUtils.extractCustom(serviceRequest.getPlay().getPlayText(),
						String.format("(?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.TEAM_YARD_REGEX), 1);
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
			loggingUtils.logException(e, e.toString());

		}

	}

	private void validateDrives(List<DrivePojo> drives) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			loggingUtils.logInfo(methodStartInfo);

			HomeAwayScoreMeta homeAwayScoreMeta = new HomeAwayScoreMeta();
			Integer drivesInGame = drives.size();
			for (int d = 0; d < drivesInGame; d++) {
				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					loggingUtils.logInfo(String.format("-- Drive Number: %s", d));
					loggingUtils.logInfo(String.format("Drive Start Period: %s", drives.get(d).getDriveStartPeriod()));
					loggingUtils.logInfo(String.format("Drive End Period: %s", drives.get(d).getDriveEndPeriod()));
					loggingUtils.logInfo(String.format("Drive Start Time: %s", drives.get(d).getDriveStartTime()));
					loggingUtils.logInfo(String.format(DRIVE_END_TIME_S, drives.get(d).getDriveEndTime()));
					loggingUtils.logInfo(String.format("Drive Total Time: %s", drives.get(d).getDriveTotalTime()));
					loggingUtils
							.logInfo(String.format("Drive Start Yardline: %s", drives.get(d).getDriveStartYardline()));
					loggingUtils.logInfo(String.format("Drive End Yardline: %s", drives.get(d).getDriveEndYardline()));
					loggingUtils.logInfo(String.format("Drive Total Yard: %s", drives.get(d).getDriveTotalYard()));
					loggingUtils.logInfo(String.format("Drive Offense Yards: %s", drives.get(d).getDriveOffenseYard()));
					loggingUtils.logInfo(String.format("Drive Result: %s", drives.get(d).getDriveResult()));
					loggingUtils.logInfo(String.format("Drive Result Point: %s", drives.get(d).getDriveResultPoint()));
					loggingUtils.logInfo(String.format("Drive Kickoff: %s", drives.get(d).getKickoff()));
					loggingUtils
							.logInfo(String.format("Drive Offense Plays: %s", drives.get(d).getDriveOffensePlays()));
				}

				validateDrive(drives.get(d));
				validateDriveIncreasingScores(drives.get(d), homeAwayScoreMeta);

				if (d != 0) {
					if (!DriveResultEnum.END_HALF.equals(drives.get(d - 1).getDriveResult())) {
						if (!drives.get(d).getDriveStartTime().equals(drives.get(d - 1).getDriveEndTime())) {
							String logInfo = String.format("Last drive end time: %s | Drive start time: %s",
									drives.get(d - 1).getDriveEndTime(), drives.get(d).getDriveStartTime());
							loggingUtils.logInfo(logInfo);
							throw new IllegalArgumentException(
									"!drives.get(d).getDriveStartTime().equals(drives.get(d - 1).getDriveEndTime())");
						}

						if (DriveResultEnum.KICKOFF.equals(drives.get(d).getDriveResult())) {
							if (drives.get(d - 1).getDriveEndYardline().equals(drives.get(d).getDriveStartYardline())) {
								String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
										drives.get(d - 1).getDriveEndYardline(), drives.get(d).getDriveStartYardline());
								loggingUtils.logInfo(logInfo);
								throw new IllegalArgumentException(
										"drives.get(d-1).getDriveEndYardline().equals(drives.get(d).getDriveStartYardline())");
							}
							if (!DriveResultEnum.FG.equals(drives.get(d - 1).getDriveResult())
									&& !DriveResultEnum.PAT.equals(drives.get(d - 1).getDriveResult())
									&& !drives.get(d - 1).getDriveEndYardline().equals(100)) {
								throw new IllegalArgumentException(
										"!drives.get(d - 1).getDriveEndYardline().equals(100)");
							}
						} else if (DriveResultEnum.PAT.equals(drives.get(d).getDriveResult())) {
							if (!DriveResultEnum.TD.equals(drives.get(d - 1).getDriveResult())) {
								throw new IllegalArgumentException(
										"!DriveResultEnum.TD.equals(drives.get(d - 1).getDriveResult())");
							}
							if (!drives.get(d).getDriveOffensePlays().equals(0)) {
								throw new IllegalArgumentException("!drives.get(d).getDriveOffensePlays().equals(0)");
							}
						} else if (DriveResultEnum.PUNT.equals(drives.get(d - 1).getDriveResult())) {
							if (!drives.get(d - 1).getDriveEndYardline()
									.equals(100 - drives.get(d).getDriveStartYardline())) {
								String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
										100 - drives.get(d - 1).getDriveEndYardline(),
										drives.get(d).getDriveStartYardline());
								loggingUtils.logInfo(logInfo);
								throw new IllegalArgumentException(
										DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
							}
						} else if (DriveResultEnum.FG.equals(drives.get(d - 1).getDriveResult())) {
							if (!drives.get(d - 1).getDriveEndYardline()
									.equals(100 - drives.get(d).getDriveStartYardline())) {
								String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
										100 - drives.get(d - 1).getDriveEndYardline(),
										drives.get(d).getDriveStartYardline());
								loggingUtils.logInfo(logInfo);
								throw new IllegalArgumentException(
										DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
							}
						} else if (DriveResultEnum.TURNOVER.equals(drives.get(d - 1).getDriveResult())) {
							if (!drives.get(d - 1).getDriveEndYardline()
									.equals(100 - drives.get(d).getDriveStartYardline())) {
								String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
										100 - drives.get(d - 1).getDriveEndYardline(),
										drives.get(d).getDriveStartYardline());
								loggingUtils.logInfo(logInfo);
								throw new IllegalArgumentException(
										DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
							}
						} else {
							if (!drives.get(d - 1).getDriveEndYardline()
									.equals(drives.get(d).getDriveStartYardline())) {
								String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
										drives.get(d - 1).getDriveEndYardline(), drives.get(d).getDriveStartYardline());
								loggingUtils.logInfo(logInfo);
								throw new IllegalArgumentException(
										DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
							}
						}
					} else {
						if (!drives.get(d).getDriveStartTime().equals(1800)) {
							String logInfo = String.format("Last drive result: %s | Drive start time: %s",
									drives.get(d - 1).getDriveResult(), drives.get(d).getDriveStartTime());
							loggingUtils.logInfo(logInfo);
							throw new IllegalArgumentException("!drives.get(d).getDriveStartTime().equals(1800)");
						}
						if (!DriveResultEnum.KICKOFF.equals(drives.get(d).getDriveResult())) {
							String logInfo = String.format("Last drive result: %s | Drive result: %s",
									drives.get(d - 1).getDriveResult(), drives.get(d).getDriveResult());
							loggingUtils.logInfo(logInfo);
							throw new IllegalArgumentException(
									"!DriveResultEnum.KICKOFF.equals(drives.get(d).getDriveResult())");
						}
					}
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, e.toString());
		}
	}

	private void validateDriveIncreasingScores(DrivePojo drive, HomeAwayScoreMeta homeAwayScoreMeta) {
		if (drive.getDriveStartHomeScore() >= homeAwayScoreMeta.getHomeStartScore()) {
			homeAwayScoreMeta.setHomeStartScore(drive.getDriveStartHomeScore());
		} else {
			throw new IllegalArgumentException("drive.getDriveStartHomeScore() < last drive's homeScoreStart");
		}
		if (drive.getDriveStartAwayScore() >= homeAwayScoreMeta.getAwayStartScore()) {
			homeAwayScoreMeta.setAwayStartScore(drive.getDriveStartAwayScore());
		} else {
			throw new IllegalArgumentException("drive.getDriveStartAwayScore() < last drive's awayScoreStart");
		}
		if (drive.getDriveResultHomeScore() >= homeAwayScoreMeta.getHomeResultScore()) {
			homeAwayScoreMeta.setHomeResultScore(drive.getDriveResultHomeScore());
		} else {
			throw new IllegalArgumentException("drive.getDriveResultHomeScore() < last drive's homeScoreResult");
		}
		if (drive.getDriveResultAwayScore() >= homeAwayScoreMeta.getAwayResultScore()) {
			homeAwayScoreMeta.setAwayResultScore(drive.getDriveResultAwayScore());
		} else {
			throw new IllegalArgumentException("drive.getDriveResultAwayScore() < last drive's awayScoreResult");
		}
	}

	private void validateDrive(DrivePojo drive) {

		validateDriveNullCheckHelper(drive);
		validateDriveNullScoreHelper(drive);
		validateDriveScoreMatchLastPlay(drive);
		validatePlayIncreasingScores(drive);

		if (drive.getDriveEndYardline().equals(100) && drive.getDriveResultPoint() < 6) {
			throw new IllegalArgumentException(
					"drive.getDriveEndYardline().equals(100) && drive.getDriveResultPoint() < 6");
		}
		if (drive.getDriveResultPoint() > 5 && !drive.getDriveEndYardline().equals(100)) {
			throw new IllegalArgumentException(
					"drive.getDriveResultPoint() > 5 && !drive.getDriveEndYardline().equals(100)");
		}
		if (drive.getDriveResultPoint() < 0 && !drive.getDriveEndYardline().equals(0)) {
			throw new IllegalArgumentException(
					"drive.getDriveResultPoint() < 0 && !drive.getDriveEndYardline().equals(-100)");
		}

		if (drive.getDriveStartAwayScore() != drive.getDriveResultAwayScore()
				|| drive.getDriveStartHomeScore() != drive.getDriveResultHomeScore()) {
			if (drive.getDriveResultPoint() == 0) {
				throw new IllegalArgumentException("drive.getDriveResultPoint() == 0");
			}
		} else {
			if (drive.getDriveResultPoint() != 0) {
				throw new IllegalArgumentException("drive.getDriveResultPoint() != 0");
			}
		}

		if (!(drive.getDriveResultPoint().equals(0) || drive.getDriveResultPoint().equals(7)
				|| drive.getDriveResultPoint().equals(1) || drive.getDriveResultPoint().equals(8)
				|| drive.getDriveResultPoint().equals(6) || drive.getDriveResultPoint().equals(3)
				|| drive.getDriveResultPoint().equals(-6))) {
			throw new IllegalArgumentException("Invalid drive points");
		}
		validateKickoff(drive);

		if (drive.getDriveStartTime() < drive.getDriveEndTime()) {
			throw new IllegalArgumentException("drive.getDriveStartTime() < drive.getDriveEndTime()");
		}

	}

	private void validatePlayIncreasingScores(DrivePojo drive) {
		Integer homeScoreStart = 0;
		Integer awayScoreStart = 0;
		Integer homeScoreResult = 0;
		Integer awayScoreResult = 0;
		for (PlayPojo play : drive.getDrivePlays()) {
			if (play.getPlayStartHomeScore() >= homeScoreStart) {
				homeScoreStart = play.getPlayStartHomeScore();
			} else {
				throw new IllegalArgumentException("play.getPlayStartHomeScore() < last play's homeScoreStart");
			}
			if (play.getPlayStartAwayScore() >= awayScoreStart) {
				awayScoreStart = play.getPlayStartAwayScore();
			} else {
				throw new IllegalArgumentException("play.getPlayStartAwayScore() < last play's awayScoreStart");
			}
			if (play.getPlayResult().getPlayResultHomeScore() >= homeScoreResult) {
				homeScoreResult = play.getPlayResult().getPlayResultHomeScore();
			} else {
				throw new IllegalArgumentException(
						"play.getPlayResult().getPlayResultHomeScore() < last play's homeScoreResult");
			}
			if (play.getPlayResult().getPlayResultAwayScore() >= awayScoreResult) {
				awayScoreResult = play.getPlayResult().getPlayResultAwayScore();
			} else {
				throw new IllegalArgumentException(
						"play.getPlayResult().getPlayResultAwayScore() < last play's awayScoreResult");
			}
		}
	}

	private void validateDriveScoreMatchLastPlay(DrivePojo drive) {
		if (drive.getDriveStartHomeScore() != drive.getDrivePlays().get(0).getPlayStartHomeScore()) {

			String actualValueLogStr = String.format("HOME | DRIVE | START | score: %s",
					drive.getDriveStartHomeScore());
			String expectedValueLogStr = String.format("HOME | FIRST PLAY | START | score: %s",
					drive.getDrivePlays().get(0).getPlayStartHomeScore());
			loggingUtils.logInfo(actualValueLogStr);
			loggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveStartHomeScore() != drive.getDrivePlays().get(0).getPlayStartHomeScore()");
		}
		if (drive.getDriveStartAwayScore() != drive.getDrivePlays().get(0).getPlayStartAwayScore()) {

			String actualValueLogStr = String.format("AWAY | DRIVE | START | score: %s",
					drive.getDriveStartAwayScore());
			String expectedValueLogStr = String.format("AWAY | FIRST PLAY | START | score: %s",
					drive.getDrivePlays().get(0).getPlayStartAwayScore());
			loggingUtils.logInfo(actualValueLogStr);
			loggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveStartAwayScore() != drive.getDrivePlays().get(0).getPlayStartAwayScore()");
		}
		if (drive.getDriveResultHomeScore() != drive.getDrivePlays().get(drive.getDrivePlays().size() - 1)
				.getPlayResult().getPlayResultHomeScore()) {

			String actualValueLogStr = String.format("HOME | DRIVE | RESULT | score: %s",
					drive.getDriveResultHomeScore());
			String expectedValueLogStr = String.format("HOME | LAST PLAY | RESULT | score: %s", drive.getDrivePlays()
					.get(drive.getDrivePlays().size() - 1).getPlayResult().getPlayResultHomeScore());
			loggingUtils.logInfo(actualValueLogStr);
			loggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveResultHomeScore() != drive.getDrivePlays().get(drive.getDrivePlays().size()-1).getPlayResult().getPlayResultHomeScore()");
		}
		if (drive.getDriveResultAwayScore() != drive.getDrivePlays().get(drive.getDrivePlays().size() - 1)
				.getPlayResult().getPlayResultAwayScore()) {

			String actualValueLogStr = String.format("AWAY | DRIVE | RESULT | score: %s",
					drive.getDriveResultAwayScore());
			String expectedValueLogStr = String.format("AWAY | LAST PLAY | RESULT | score: %s", drive.getDrivePlays()
					.get(drive.getDrivePlays().size() - 1).getPlayResult().getPlayResultAwayScore());
			loggingUtils.logInfo(actualValueLogStr);
			loggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveResultAwayScore() != drive.getDrivePlays().get(drive.getDrivePlays().size()-1).getPlayResult().getPlayResultAwayScore()");
		}
	}

	private void validateDriveNullCheckHelper(DrivePojo drive) {
		if (StringUtils.isBlank(drive.getPossessionTeamId())) {
			throw new IllegalArgumentException("StringUtils.isBlank(drive.getPossessionTeamId())");
		}
		if (drive.getDriveStartTime() == null) {
			throw new IllegalArgumentException("drive.getDriveStartTime() == null");
		}
		if (drive.getDriveEndTime() == null) {
			throw new IllegalArgumentException("drive.getDriveEndTime() == null");
		}
		if (drive.getDriveTotalTime() == null) {
			throw new IllegalArgumentException("drive.getDriveTotalTime() == null");
		}
		if (drive.getDriveOffensePlays() == null) {
			throw new IllegalArgumentException("drive.getDriveOffensePlays() == null");
		}
		if (drive.getDriveStartPeriod() == null) {
			throw new IllegalArgumentException("drive.getDriveStartPeriod() == null");
		}
		if (drive.getDriveEndPeriod() == null) {
			throw new IllegalArgumentException("drive.getDriveEndPeriod() == null");
		}
		if (drive.getDrivePlays().isEmpty()) {
			throw new IllegalArgumentException("drive.getDrivePlays().isEmpty()");
		}
		if (drive.getDriveStartYardline() == null) {
			throw new IllegalArgumentException("drive.getDriveStartYardline() == null");
		}
		if (drive.getDriveEndYardline() == null) {
			throw new IllegalArgumentException("drive.getDriveEndYardline() == null");
		}
		if (drive.getDriveTotalYard() == null) {
			throw new IllegalArgumentException("drive.getDriveTotalYard() == null");
		}
		if (drive.getDriveOffenseYard() == null) {
			throw new IllegalArgumentException("drive.getDriveOffenseYard() == null");
		}
		if (drive.getDriveResultPoint() == null) {
			throw new IllegalArgumentException("drive.getDriveResultPoint() == null");
		}
		if (drive.getDriveResult() == null) {
			throw new IllegalArgumentException("drive.getDriveResult() == null");
		}
		if (drive.getDriveResultScoringOpp() == null) {
			throw new IllegalArgumentException("drive.getDriveResultScoringOpp() == null");
		}
	}

	private void validateDriveNullScoreHelper(DrivePojo drive) {
		if (drive.getDriveResultHomeScore() == null) {
			throw new IllegalArgumentException("drive.getDriveResultHomeScore() == null");
		}
		if (drive.getDriveResultAwayScore() == null) {
			throw new IllegalArgumentException("drive.getDriveResultAwayScore() == null");
		}
		if (drive.getDriveStartHomeScore() == null) {
			throw new IllegalArgumentException("drive.getDriveStartHomeScore() == null");
		}
		if (drive.getDriveStartAwayScore() == null) {
			throw new IllegalArgumentException("drive.getDriveStartAwayScore() == null");
		}
	}

	private void validateKickoff(DrivePojo drive) {
		if (Boolean.TRUE.equals(drive.getKickoff()) || DriveResultEnum.PAT.equals(drive.getDriveResult())) {
			if (drive.getDrivePlays().stream().filter(p -> !p.getNoPlayPenalty()).collect(Collectors.toList())
					.size() != 1) {
				throw new IllegalArgumentException("drive.getDrivePlays().size() != 1");
			}
			if (drive.getDriveOffensePlays() != 0) {
				throw new IllegalArgumentException("drive.getDriveOffensePlays() != 0");
			}
		} else {
			if (drive.getDriveOffensePlays() == 0) {
				throw new IllegalArgumentException("drive.getDriveOffensePlays() == 0");
			}
		}
	}

}
