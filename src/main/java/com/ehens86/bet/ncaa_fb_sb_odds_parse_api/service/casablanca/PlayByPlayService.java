package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.logging.log4j.ThreadContext;

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
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.GameMapPojo;
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
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate.DriveValidationService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class PlayByPlayService {
	private static final String KICKOFF_DRIVE_NUMBER_S = "Kickoff Drive Number: %s";

	private static final String DRIVE_END_TIME_S = "Drive End Time: %s";

	private static final String STARTING_S = " -- Starting [%s]";

	private static final String PLAY_NUMBER_S = "Play Number: %s";

	private static final String TEXT_S = "Text: %s";

	private static final String DRIVE_NUMBER_S = "Drive Number: %s";

	private static final String QUARTER_START = "15:00";

	// private static static constructor to prevent instantiation
	private PlayByPlayService() {
		throw new UnsupportedOperationException();
	}

	public static void parsePbP(PlayByPlayPojo playByPlayRaw, GamePojo game) {
		try {

			if ("5851600".equals(game.getNcaaGameId()) || "5851699".equals(game.getNcaaGameId())) {
				return;
			}

			ObjectMapper objectMapper = new ObjectMapper();
			String json = objectMapper.writeValueAsString(playByPlayRaw);
			System.out.println(json);
			String jsonGame = objectMapper.writeValueAsString(game);
			System.out.println(jsonGame);
			HashMap<String, HomeAwayEnum> teamDict = new HashMap<>();
			HashMap<String, PlayByPlayTeamPojo> teamAbbrevDict = new HashMap<>();

			for (PlayByPlayTeamPojo gameTeam : playByPlayRaw.getMeta().getTeams()) {
				teamDict.put(gameTeam.getId(), gameTeam.pullHomeAwayEnum());
				teamAbbrevDict.put(gameTeam.getId(), gameTeam);
			}
			GameMapPojo gameMaps = new GameMapPojo(teamDict, teamAbbrevDict);
			List<List<PbpServiceRequestPojo>> gameInfo = driveFirstPassParse(playByPlayRaw, game, gameMaps);

			/**
			 * Split TD/Extra Point into 2 plays where needed
			 */
			splitTdExtraPointPlays(gameInfo, gameMaps);

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
			 * Set punt turnover indexes
			 */
			splitPuntTurnoverToNewDrive(gameInfo);

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
			DriveValidationService.validateDrives(drives);

			PbpPojo gamePlays = new PbpPojo();
			gamePlays.setDrives(drives);
			game.setPlays(gamePlays);
		} catch (Exception e) {
			String errorStr = String.format("ERROR: PBP parse failed for %s vs %s with %s",
					game.getTeamHome().getTeamName(), game.getTeamAway().getTeamName(), e.toString());
			LoggingUtils.logException(e, errorStr);
			throw new IllegalArgumentException(errorStr);
		}
	}

	/**
	 * Split TD/Extra Point into 2 plays where needed
	 */
	private static void splitTdExtraPointPlays(List<List<PbpServiceRequestPojo>> gameInfo, GameMapPojo gameMaps) {
		try {

			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			for (int d = 0; d < gameInfo.size(); d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				for (int p = 0; p < drive.size(); p++) {
					PbpServiceRequestPojo play = drive.get(p);

					if (PbpParsingUtils.evalMatch(play.getPlayRawText(),
							"The previous play is under review. The ruling on the field (stands|is confirmed)")) {
						String newRawText = play.getPlayRawText().split("The previous play is under review")[0];
						newRawText = newRawText.strip();
						newRawText = newRawText.replaceAll(".$", "");
						play.setPlayRawText(newRawText);
					} else if (PbpParsingUtils.evalMatch(play.getPlayRawText(),
							"After review, the ruling on the field was overturned")) {
						String newRawText = play.getPlayRawText()
								.split("After review, the ruling on the field was overturned")[0];
						newRawText = newRawText.strip();
						newRawText = newRawText.replaceAll(".$", "");
						play.setPlayRawText(newRawText);
					} else if (PbpParsingUtils.evalMatch(play.getPlayRawText(),
							"\\(After review, play stands as called on the field\\)")) {
						String newRawText = play.getPlayRawText()
								.split("(After review, play stands as called on the field)")[0];
						newRawText = newRawText.strip();
						newRawText = newRawText.replaceAll(".$", "");
						play.setPlayRawText(newRawText);
					} else if (play.getPlayRawText().contains("TOUCHDOWN")
							&& !play.getPlayRawText().contains("score nullified")) {
						String newRawText = play.getPlayRawText().split("TOUCHDOWN")[1];
						newRawText = newRawText.replaceAll("^,", "");
						newRawText = newRawText.replace(" clock ", "");
						newRawText = newRawText.strip();
						newRawText = newRawText.replaceAll("^,", "");
						newRawText = newRawText.strip();
						newRawText = newRawText.replace("(Scoring play confirmed)", "");
						newRawText = newRawText.strip();
						newRawText = newRawText.replaceAll("1ST DOWN$", "");
						newRawText = newRawText.strip();
						Integer newRawTextWordCount = newRawText.split(" ").length;
						if (!newRawText.isEmpty() && newRawTextWordCount > 1) {
							newRawText = cleanUpPlayText(newRawText, play.getPossessionTeam(), play.getDefenseTeam(),
									gameMaps);
							String[] playTackles = PbpParsingUtils.extractTackle(newRawText);

							PlayPojo newPlay = initializePlay(play.getPlay().getPeriod(), newRawText,
									play.getPlay().getDriveText().replace("AMP;", ""),
									play.getPlay().getPlayResult().getPlayResultPossessionTeamId(), gameMaps);

							PbpServiceRequestPojo newServiceRequest = new PbpServiceRequestPojo(
									new DrivePojo(play.getDrive().getPossessionTeamId(),
											play.getDrive().getDriveStartPeriod(), play.getDrive().getDriveStartTime(),
											play.getDrive().getDriveEndTime()),
									newPlay, play.getPlayRaw(), newRawText, playTackles, gameMaps.getTeamDict(),
									gameMaps.getTeamAbbrevDict());
							newServiceRequest.setPossessionTeam(play.getPossessionTeam());
							newServiceRequest.setDefenseTeam(play.getDefenseTeam());
							cleanUpPlay(newServiceRequest);
							addEndYard(newServiceRequest);
							PbpPenaltyParseService.parsePenalty(newServiceRequest);
							newServiceRequest.getPlay().getPlayResult().setPlayResultPossessionTeamId(
									newServiceRequest.getPlay().getPlayStartPossessionTeamId());
							gameInfo.get(d).add(p + 1, newServiceRequest);
							play.setPlayRawText(play.getPlayRawText().replaceAll("(.+TOUCHDOWN)(.+)", "$1"));
						}
					}
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Set punt turnover indexes
	 */
	private static void splitPuntTurnoverToNewDrive(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			HashMap<Integer, List<Integer>> puntIndexes = new HashMap<>();
			HashMap<Integer, Integer> driveLengths = new HashMap<>();
			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);

				Integer playsInPossession = drive.size();
				driveLengths.put(d, playsInPossession);
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					if (PlayTypeEnum.PUNT.equals(play.getPlay().getPlayType())
							&& Boolean.TRUE.equals(play.getPlay().getPlayResult().isPlayResultTurnover())) {
						if (!puntIndexes.containsKey(d)) {
							List<Integer> puntList = new ArrayList<>();
							puntList.add(p);
							puntIndexes.put(d, puntList);
						} else {
							puntIndexes.get(d).add(p);
						}
					}
				}
			}
			applyNewPuntTurnoverIndexes(gameInfo, driveLengths, puntIndexes);
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Split punt turnovers to seperate drives
	 */
	private static void applyNewPuntTurnoverIndexes(List<List<PbpServiceRequestPojo>> gameInfo,
			HashMap<Integer, Integer> driveLengths, HashMap<Integer, List<Integer>> puntIndexes) {
		try {
			List<Integer> puntDrives = new ArrayList<>(puntIndexes.keySet());
			Collections.sort(puntDrives);
			Collections.reverse(puntDrives);
			for (Integer puntDriveIndex : puntDrives) {
				if (puntIndexes.get(puntDriveIndex).size() > 1) {
					String puntDriveInfo = String.format("Punt Drive Number: %s", puntDriveIndex);
					LoggingUtils.logInfo(puntDriveInfo);
					String logInfo = String.format("Number of punts found in drive: %s",
							puntIndexes.get(puntDriveIndex).size());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("handle multiple punts in a drive");
				}

				if (driveLengths.get(puntDriveIndex) > 3) {
					Integer puntTurnoverPlayInDrive = puntIndexes.get(puntDriveIndex).get(0);
					Integer lastPlayInDrive = driveLengths.get(puntDriveIndex);
					Integer splitTimeEnd = (int) gameInfo.get(puntDriveIndex).get(lastPlayInDrive - 1).getDrive()
							.getDriveEndTime();
					Integer splitTimeStart = (int) gameInfo.get(puntDriveIndex).get(0).getDrive().getDriveStartTime();

					List<PbpServiceRequestPojo> newPuntDrive = new ArrayList<>();
					for (int p = puntTurnoverPlayInDrive + 1; p < lastPlayInDrive; p++) {
						PbpServiceRequestPojo playNewDrive = gameInfo.get(puntDriveIndex).get(p);
						playNewDrive.getDrive().setDriveEndTime(splitTimeEnd);
						playNewDrive.getDrive().setDriveStartTime(splitTimeEnd);
						newPuntDrive.add(playNewDrive);
					}

					gameInfo.add(puntDriveIndex + 1, newPuntDrive);

					List<PbpServiceRequestPojo> oldPuntDrive = gameInfo.get(puntDriveIndex);
					for (int p = lastPlayInDrive - 1; p >= puntTurnoverPlayInDrive + 1; p--) {
						oldPuntDrive.remove(p);
					}

					for (PbpServiceRequestPojo oldPuntPlay : oldPuntDrive) {
						oldPuntPlay.getDrive().setDriveEndTime(splitTimeEnd);
						oldPuntPlay.getDrive().setDriveStartTime(splitTimeStart);
					}
					gameInfo.set(puntDriveIndex, oldPuntDrive);
				} else {
					String puntDriveInfo = String.format("Punt Drive Number: %s", puntDriveIndex);
					LoggingUtils.logInfo(puntDriveInfo);
					String logInfo = String.format("Number of plays found after punt turnover in drive: %s",
							driveLengths.get(puntDriveIndex));
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("handle punt turnover without additional play drives");
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Set kickoff indexes
	 */
	private static void splitKickoffToNewDrive(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			HashMap<Integer, List<Integer>> kickoffIndexes = new HashMap<>();
			HashMap<Integer, List<Integer>> penaltyIndexes = new HashMap<>();

			HashMap<Integer, Integer> driveLengths = new HashMap<>();
			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);

				Integer playsInPossession = drive.size();
				driveLengths.put(d, playsInPossession);
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					if (PlayCallTypeEnum.KICKOFF.equals(play.getPlay().getPlayCallType())) {
						if (!kickoffIndexes.containsKey(d)) {
							List<Integer> kickoffList = new ArrayList<>();
							kickoffList.add(p);
							kickoffIndexes.put(d, kickoffList);
						} else {
							kickoffIndexes.get(d).add(p);
						}
						if (p > 0 && PlayTypeEnum.PENALTY.equals(drive.get(p - 1).getPlay().getPlayType())) {
							if (!penaltyIndexes.containsKey(d)) {
								List<Integer> penaltyList = new ArrayList<>();
								penaltyList.add(p - 1);
								penaltyIndexes.put(d, penaltyList);
							} else {
								penaltyIndexes.get(d).add(p - 1);
							}
						}
					}
				}
			}
			applyNewKickoffIndexes(gameInfo, driveLengths, kickoffIndexes, penaltyIndexes);
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Split kickoffs to seperate drives
	 */
	private static void applyNewKickoffIndexes(List<List<PbpServiceRequestPojo>> gameInfo,
			HashMap<Integer, Integer> driveLengths, HashMap<Integer, List<Integer>> kickoffIndexes,
			HashMap<Integer, List<Integer>> penaltyIndexes) {
		try {
			List<Integer> kickoffDrives = new ArrayList<>(kickoffIndexes.keySet());
			Collections.sort(kickoffDrives);
			Collections.reverse(kickoffDrives);
			for (Integer kickoffDriveIndex : kickoffDrives) {
				if (kickoffIndexes.get(kickoffDriveIndex).size() > 1) {
					String kickoffDriveInfo = String.format(KICKOFF_DRIVE_NUMBER_S, kickoffDriveIndex);
					LoggingUtils.logInfo(kickoffDriveInfo);
					String logInfo = String.format("Number of kickoffs found in drive: %s",
							kickoffIndexes.get(kickoffDriveIndex).size());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("handle multiple kickoffs in a drive");
				}

				if (driveLengths.get(kickoffDriveIndex) > 1) {
					List<PbpServiceRequestPojo> newKickoffDrive = new ArrayList<>();
					newKickoffDrive
							.add(gameInfo.get(kickoffDriveIndex).get(kickoffIndexes.get(kickoffDriveIndex).get(0)));
					List<PbpServiceRequestPojo> oldKickoffDrive = gameInfo.get(kickoffDriveIndex);
					oldKickoffDrive.remove((int) kickoffIndexes.get(kickoffDriveIndex).get(0));
					if (penaltyIndexes.containsKey(kickoffDriveIndex)) {
						if (penaltyIndexes.get(kickoffDriveIndex).size() > 1) {
							String penaltyDriveInfo = String.format(KICKOFF_DRIVE_NUMBER_S, kickoffDriveIndex);
							LoggingUtils.logInfo(penaltyDriveInfo);
							String logInfo = String.format("Number of penalties found in drive after PAT: %s",
									penaltyIndexes.get(kickoffDriveIndex).size());
							LoggingUtils.logInfo(logInfo);
							throw new IllegalArgumentException("handle multiple penalties in a drive after PAT");
						}
						if (penaltyIndexes.get(kickoffDriveIndex).get(0) + 1 != kickoffIndexes.get(kickoffDriveIndex)
								.get(0)) {
							String penaltyDriveInfo = String.format(KICKOFF_DRIVE_NUMBER_S, kickoffDriveIndex);
							LoggingUtils.logInfo(penaltyDriveInfo);
							String logInfo = String.format(
									"Penalty is play %s in drive which is not directly before kickoff at play %s",
									penaltyIndexes.get(kickoffDriveIndex).get(0),
									kickoffIndexes.get(kickoffDriveIndex).get(0));
							LoggingUtils.logInfo(logInfo);
							throw new IllegalArgumentException(
									"Handle penalty after PAT but not directly before kickoff");
						}
						newKickoffDrive.add(0,
								gameInfo.get(kickoffDriveIndex).get(penaltyIndexes.get(kickoffDriveIndex).get(0)));
						newKickoffDrive.get(0).setKickoffTeam(newKickoffDrive.get(1).getDefenseTeam());
						newKickoffDrive.get(0).setKickoffReturnTeam(newKickoffDrive.get(1).getPossessionTeam());
						oldKickoffDrive.remove((int) penaltyIndexes.get(kickoffDriveIndex).get(0));
					}
					gameInfo.add(kickoffDriveIndex + 1, newKickoffDrive);
					gameInfo.set(kickoffDriveIndex, oldKickoffDrive);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Rollforward end yardline to fill in start yard where missing. addTempoType()
	 * cleanUpYards()
	 */
	private static void rollforwardEndYardLine(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {

			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

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
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Rollforward end yardline to fill in start yard where missing. addTempoType()
	 * cleanUpYards()
	 */
	private static void rollforwardEndYardLinePlayHelper(PbpServiceRequestPojo play,
			RollforwardLastPlayPojo rollforwardInfo) {
		try {
			if (play.getPlayRawText().isEmpty()) {
				String logInfo = String.format("Play Down & Distance: %s", play.getPlay().getDriveText());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("play raw text is empty");
			}
			if (!PlayTypeEnum.KICKOFF.equals(play.getPlay().getPlayType())
					&& Objects.nonNull(play.getPlay().getPlayStartYard())) {
				play.getPlay().setPlayStartYard(rollforwardInfo.getLastPlayEndYardLine());
			}
			if (!PlayTypeEnum.PAT.equals(play.getPlay().getPlayType())
					&& play.getPlay().getDriveText().endsWith("at -")) {
				if (Objects.isNull(rollforwardInfo.getLastDriveText())) {
					String logInfo = String.format("Play text: %s", play.getPlayRawText());
					LoggingUtils.logInfo(logInfo);
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
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Send for processing
	 */
	private static void sendPlaysForProcessing(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			Integer drivesInGame = gameInfo.size();

			HomeAwayScoreMeta homeAwayScoreMeta = new HomeAwayScoreMeta();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);

					play.getPlay().setPlayStartHomeScore(homeAwayScoreMeta.getHomeScore());
					play.getPlay().setPlayStartAwayScore(homeAwayScoreMeta.getAwayScore());

					if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE
							.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
						LoggingUtils.logInfo("----------------------------");
						LoggingUtils.logInfo(String.format("Play Text: %s", play.getPlayRawText()));
						LoggingUtils.logInfo(String.format("Play Type: %s", play.getPlay().getPlayType()));
						LoggingUtils.logInfo(String.format("Play Call Type: %s", play.getPlay().getPlayCallType()));
					}
					boolean updated = sendPlaysForProcessingHelperProcess(play);
					sendPlaysForProcessingPostProcessingHelper(play, homeAwayScoreMeta);
					if (!updated && Boolean.FALSE.equals(play.getPlay().getNoPlayPenalty())) {
						String logInfo = String.format("Play Text: %s", play.getPlayRawText());
						LoggingUtils.logInfo(logInfo);
						throw new IllegalArgumentException("Play matched no conditions to enter processing.");
					}

				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static boolean sendPlaysForProcessingHelperProcess(PbpServiceRequestPojo play) {
		boolean updated = false;
		try {
			if (Boolean.FALSE.equals(play.getPlay().getNoPlayPenalty())) {
				updated = PbpKickoffParseService.parseKickoff(play, updated);
				updated = PbpPuntParseService.parsePunt(play, updated);
				updated = PbpFieldGoalParseService.parseFieldGoal(play, updated);
				updated = PbpPassParseService.parsePass(play, updated);
				updated = PbpRushParseService.parseRush(play, updated);
				PbpDefenseParseService.parseDefense(play);
				PbpOffenseParseService.parseOffense(play);
			} else {
				reconcileNoPlayHalfDistance(play);
				if (Objects.isNull(play.getPlay().getPlayCallType())) {
					play.getPlay().setPlayCallType(PlayCallTypeEnum.NA);
				}
			}
			return updated;
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
			return updated;
		}
	}

	private static void sendPlaysForProcessingPostProcessingHelper(PbpServiceRequestPojo play,
			HomeAwayScoreMeta homeAwayScoreMeta) {
		try {
			postProcessingApplyPlayScore(play, homeAwayScoreMeta);
			sendPlaysForProcessingGarbageTimeHelper(play, homeAwayScoreMeta);
			addStartYardPenalty(play);

			if (!PlayTypeEnum.KICKOFF.equals(play.getPlay().getPlayType())
					&& !PlayTypeEnum.PAT.equals(play.getPlay().getPlayType())) {
				sendPlaysForProcessingFieldZoneHelper(play);

				if (!PlayCallTypeEnum.FG.equals(play.getPlay().getPlayCallType())
						&& !PlayTypeEnum.PUNT.equals(play.getPlay().getPlayType())) {
					Integer yardToGain = play.getPlay().getPlayYardToGain();
					PlayDownEnum playDown = play.getPlay().getPlayStartDown();
					Integer yardGained = play.getPlay().getPlayResult().getPlayResultYard();
					boolean firstDown = play.getPlay().getPlayResult().isPlayResultFirstDown();
					Integer playPoints = play.getPlay().getPlayResult().getPlayResultPoints();

					sendPlaysForProcessingPassingDownHelper(play, playDown, yardToGain);
					sendPlaysForProcessingPlaySuccessHelper(play, playDown, yardGained, firstDown, yardToGain,
							playPoints);
					if (PlayCallTypeEnum.RUN.equals(play.getPlay().getPlayCallType())
							&& Boolean.FALSE.equals(play.getPlay().getNoPlayPenalty())) {
						sendPlaysForProcessingRushingPowerHelper(play);
					}
					sendPlaysForProcessingDefeatHelper(play, playDown, yardGained, firstDown);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void sendPlaysForProcessingRushingPowerHelper(PbpServiceRequestPojo play) {
		try {
			boolean rushingPower = false;
			Boolean rushingPowerSuccess = null;

			if ((play.getPlay().getPlayStartYard() >= 98) || (play.getPlay().getPlayYardToGain() <= 2
					&& (PlayDownEnum.THIRD.equals(play.getPlay().getPlayStartDown())
							|| PlayDownEnum.FOURTH.equals(play.getPlay().getPlayStartDown())))) {
				rushingPower = true;
			}

			if (Boolean.TRUE.equals(rushingPower)) {
				if (play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.getRushingFirstDown() == 1
						|| play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense().getRushingStat()
								.get(0).getRushingTouchdown() == 1) {
					rushingPowerSuccess = true;
				} else {
					rushingPowerSuccess = false;
				}
			}

			play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense().getRushingStat().get(0)
					.setRushingPower(rushingPower);
			play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense().getRushingStat().get(0)
					.setRushingPowerSuccess(rushingPowerSuccess);
			play.getPlay().getPlayerStat().get(play.getPossessionTeam()).getOffense().getRushingStat().get(0)
					.setRushingSuccess(play.getPlay().getPlayResult().getPlayResultSuccess());
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void sendPlaysForProcessingPlaySuccessHelper(PbpServiceRequestPojo play, PlayDownEnum playDown,
			Integer yardGained, boolean firstDown, Integer yardToGain, Integer playPoints) {
		try {
			play.getPlay().getPlayResult().setPlayResultSuccess(false);
			if (playPoints == 6 || Boolean.TRUE.equals(firstDown)
					|| (PlayDownEnum.FIRST.equals(playDown) && yardGained >= yardToGain / 2)
					|| (PlayDownEnum.SECOND.equals(playDown) && yardGained >= yardToGain * .7)) {
				play.getPlay().getPlayResult().setPlayResultSuccess(true);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void sendPlaysForProcessingPassingDownHelper(PbpServiceRequestPojo play, PlayDownEnum playDown,
			Integer yardToGain) {
		try {
			play.getPlay().setPassingDown(false);
			if ((PlayDownEnum.SECOND.equals(playDown) && yardToGain >= 7)
					|| (PlayDownEnum.THIRD.equals(playDown) && yardToGain >= 5)
					|| (PlayDownEnum.FOURTH.equals(playDown) && yardToGain >= 5)) {
				play.getPlay().setPassingDown(true);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void sendPlaysForProcessingDefeatHelper(PbpServiceRequestPojo play, PlayDownEnum playDown,
			Integer yardGained, boolean firstDown) {
		try {
			play.getPlay().setDefeat(false);
			if (yardGained < 0 || Boolean.TRUE.equals(play.getPlay().getPlayResult().isPlayResultTurnover())
					|| ((PlayDownEnum.THIRD.equals(playDown) || PlayDownEnum.FOURTH.equals(playDown)) && !firstDown)) {
				play.getPlay().setDefeat(true);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void sendPlaysForProcessingFieldZoneHelper(PbpServiceRequestPojo play) {
		try {
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
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void sendPlaysForProcessingGarbageTimeHelper(PbpServiceRequestPojo play,
			HomeAwayScoreMeta homeAwayScoreMeta) {
		try {
			Integer scoreDiff = homeAwayScoreMeta.calcAbsScoreDiff();
			play.getPlay().setGarbageTime(false);
			if ((PlayPeriodEnum.SECOND.equals(play.getPlay().getPeriod()) && scoreDiff > 38)
					|| (PlayPeriodEnum.THIRD.equals(play.getPlay().getPeriod()) && scoreDiff > 28)
					|| (PlayPeriodEnum.FOURTH.equals(play.getPlay().getPeriod()) && scoreDiff > 22)) {
				play.getPlay().setGarbageTime(true);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Add play result scores: home/away
	 */
	private static void postProcessingApplyPlayScore(PbpServiceRequestPojo play, HomeAwayScoreMeta homeAwayScoreMeta) {
		try {
			if (HomeAwayEnum.HOME
					.equals(play.getTeamDict().get(play.getPlay().getPlayResult().getPlayResultPossessionTeamId()))) {
				if (play.getPlay().getPlayResult().getPlayResultPoints() >= 0) {
					homeAwayScoreMeta.setHomeScore(
							homeAwayScoreMeta.getHomeScore() + play.getPlay().getPlayResult().getPlayResultPoints());
				} else if (play.getPlay().getPlayResult().getPlayResultPoints() == -2) {
					homeAwayScoreMeta.setAwayScore(homeAwayScoreMeta.getAwayScore()
							+ -1 * play.getPlay().getPlayResult().getPlayResultPoints());
				} else if (play.getPlay().getPlayResult().getPlayResultPoints() == -6) {
					homeAwayScoreMeta.setHomeScore(homeAwayScoreMeta.getHomeScore()
							+ -1 * play.getPlay().getPlayResult().getPlayResultPoints());
				} else {
					throw new IllegalArgumentException(String.format("Play result not supported: %s",
							play.getPlay().getPlayResult().getPlayResultPoints()));
				}
			} else {
				if (play.getPlay().getPlayResult().getPlayResultPoints() >= 0) {
					homeAwayScoreMeta.setAwayScore(
							homeAwayScoreMeta.getAwayScore() + play.getPlay().getPlayResult().getPlayResultPoints());
				} else if (play.getPlay().getPlayResult().getPlayResultPoints() == -2) {
					homeAwayScoreMeta.setHomeScore(homeAwayScoreMeta.getHomeScore()
							+ -1 * play.getPlay().getPlayResult().getPlayResultPoints());
				} else if (play.getPlay().getPlayResult().getPlayResultPoints() == -6) {
					homeAwayScoreMeta.setAwayScore(homeAwayScoreMeta.getAwayScore()
							+ -1 * play.getPlay().getPlayResult().getPlayResultPoints());
				} else {
					throw new IllegalArgumentException(String.format("Play result not supported: %s",
							play.getPlay().getPlayResult().getPlayResultPoints()));
				}
			}

			play.getPlay().getPlayResult().setPlayResultHomeScore(homeAwayScoreMeta.getHomeScore());
			play.getPlay().getPlayResult().setPlayResultAwayScore(homeAwayScoreMeta.getAwayScore());
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Combine drives split across quarters
	 */
	private static void combineCarryOverQuarterDrives(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

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
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Helper function per drive to split across quarters
	 */
	private static String combineCarryOverQuarterDrivesHelper(List<PbpServiceRequestPojo> drive, Integer d,
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
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("missing needed possession team from prior drive");
			}
			combineCarryOverQuarterDrivesHelperCombine(d, lastPossessionTeam, periodDrives, drivesToCombine, gameInfo,
					possessionTeam);
			if (!PlayTypeEnum.KICKOFF.equals(gameInfo.get(d).get(gameInfo.get(d).size() - 1).getPlay().getPlayType())) {
				lastPossessionTeam = gameInfo.get(d).get(gameInfo.get(d).size() - 1).getPossessionTeam();
			} else {
				lastPossessionTeam = null;
			}
			return lastPossessionTeam;
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static List<Integer> combineCarryOverQuarterDrivesHelperCombine(Integer d, String lastPossessionTeam,
			EnumMap<PlayPeriodEnum, DriveTimesPojo> periodDrives, List<Integer> drivesToCombine,
			List<List<PbpServiceRequestPojo>> gameInfo, String possessionTeam) {
		try {
			if (d != 0 && possessionTeam.equals(lastPossessionTeam)) {
				if (periodDrives.get(PlayPeriodEnum.SECOND).getStartDrive().equals(d)
						|| periodDrives.get(PlayPeriodEnum.FOURTH).getStartDrive().equals(d)) {
					drivesToCombine.add(d);
				} else if (!periodDrives.get(PlayPeriodEnum.THIRD).getStartDrive().equals(d)) {
					List<PbpServiceRequestPojo> lastDrive = gameInfo.get(d - 1);
					PbpServiceRequestPojo lastPlayLastDrive = lastDrive.get(lastDrive.size() - 1);
					if (Boolean.FALSE.equals(lastPlayLastDrive.getPlay().getPlayResult().isPlayResultTurnover())
							&& !PlayTypeEnum.PUNT.equals(lastPlayLastDrive.getPlay().getPlayType())) {
						String logInfo = String.format(DRIVE_NUMBER_S, d);
						LoggingUtils.logInfo(logInfo);
						throw new IllegalArgumentException("Same possession team across consecutive drives");
					}
				}
			}
			return drivesToCombine;
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	/**
	 * Clean Up Scoring Plays, Extra Point, Penalties before kickoff
	 */
	private static void scoringPlayCleanup(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

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
				} else if (pp.getPoints() == -6) {
					scoringPlayCleanupDefTdHelper(gameInfo.get(pp.getDrive()), pp);
				} else {
					logInfoPlayText = String.format(TEXT_S,
							gameInfo.get(pp.getDrive()).get(pp.getPlay() + 1).getPlayRawText());
					LoggingUtils.logInfo(logInfoPlayText);
					logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
					LoggingUtils.logInfo(logInfoPlayNumber);
					logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
					LoggingUtils.logInfo(logInfoDriveNumber);
					throw new IllegalArgumentException("Handle uncaught scoring logic");
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void scoringPlayCleanupDefTdHelper(List<PbpServiceRequestPojo> driveInfo, PlayResultMeta pp) {
		try {
			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			if (driveInfo.size() > pp.getPlay() + 1) {
				logInfoPlayText = String.format(TEXT_S, driveInfo.get(pp.getPlay() + 1).getPlayRawText());
				LoggingUtils.logInfo(logInfoPlayText);
				logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
				LoggingUtils.logInfo(logInfoPlayNumber);
				logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
				LoggingUtils.logInfo(logInfoDriveNumber);
				throw new IllegalArgumentException("What to do for play after Defensive Touchdown in drive?");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void scoringPlayCleanupTdHelper(List<PbpServiceRequestPojo> driveInfo, PlayResultMeta pp) {
		try {
			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			Integer tdDriveLength = driveInfo.size();
			if (tdDriveLength == pp.getPlay() + 1
					&& !PlayTypeEnum.PUNT.equals(driveInfo.get(pp.getPlay()).getPlay().getPlayType())
					&& !PlayTypeEnum.KICKOFF.equals(driveInfo.get(pp.getPlay()).getPlay().getPlayType())) {
				logInfoPlayText = String.format(TEXT_S, driveInfo.get(pp.getPlay()).getPlayRawText());
				LoggingUtils.logInfo(logInfoPlayText);
				logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
				LoggingUtils.logInfo(logInfoPlayNumber);
				logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
				LoggingUtils.logInfo(logInfoDriveNumber);
				throw new IllegalArgumentException("What to do for TD as last play?");
			}
			for (PbpServiceRequestPojo ppp : driveInfo.subList(pp.getPlay() + 1, tdDriveLength)) {
				if (PlayTypeEnum.PAT.equals(ppp.getPlay().getPlayType())) {
					break; // Stop search at PAT
				} else {
					logInfoPlayText = String.format(TEXT_S, ppp.getPlayRawText());
					LoggingUtils.logInfo(logInfoPlayText);
					logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
					LoggingUtils.logInfo(logInfoPlayNumber);
					logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
					LoggingUtils.logInfo(logInfoDriveNumber);
					throw new IllegalArgumentException("What to do for non PAT play after TD in drive?");
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void scoringPlayCleanupPatHelper(List<PbpServiceRequestPojo> driveInfo, PlayResultMeta pp) {
		try {
			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			if (driveInfo.size() > pp.getPlay() + 1) {
				logInfoPlayText = String.format(TEXT_S, driveInfo.get(pp.getPlay() + 1).getPlayRawText());
				LoggingUtils.logInfo(logInfoPlayText);
				logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
				LoggingUtils.logInfo(logInfoPlayNumber);
				logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
				LoggingUtils.logInfo(logInfoDriveNumber);
				throw new IllegalArgumentException("Need to handle play after PAT in a drive");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void scoringPlayCleanupFgHelper(List<PbpServiceRequestPojo> driveInfo, PlayResultMeta pp) {
		try {
			String logInfoDriveNumber;
			String logInfoPlayNumber;
			String logInfoPlayText;
			if (driveInfo.size() > pp.getPlay() + 1) {
				logInfoPlayText = String.format(TEXT_S, driveInfo.get(pp.getPlay() + 1).getPlayRawText());
				LoggingUtils.logInfo(logInfoPlayText);
				logInfoPlayNumber = String.format(PLAY_NUMBER_S, pp.getPlay());
				LoggingUtils.logInfo(logInfoPlayNumber);
				logInfoDriveNumber = String.format(DRIVE_NUMBER_S, pp.getDrive());
				LoggingUtils.logInfo(logInfoDriveNumber);
				throw new IllegalArgumentException("What to do for play after FG in drive?");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Validate Plays
	 */
	private static void addTurnoverOnDowns(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				PbpServiceRequestPojo play = drive.get(playsInPossession - 1);
				if (PlayTypeEnum.OFFENSE.equals(play.getPlay().getPlayType())
						&& !PlayCallTypeEnum.FG.equals(play.getPlay().getPlayCallType())
						&& PlayDownEnum.FOURTH.equals(play.getPlay().getPlayStartDown())
						&& play.getPlay().getPlayYardToGain() > play.getPlay().getPlayResult().getPlayResultYard()) {
					play.getPlay().getPlayResult().setPlayResultTurnover(true);
					play.getPlay().getPlayResult().setPlayResultPossessionTeamId(play.getDefenseTeam());

				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Validate Plays
	 */
	private static void validatePlays(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			Integer drivesInGame = gameInfo.size();
			for (int d = 0; d < drivesInGame; d++) {
				List<PbpServiceRequestPojo> drive = gameInfo.get(d);
				Integer playsInPossession = drive.size();
				for (int p = 0; p < playsInPossession; p++) {
					PbpServiceRequestPojo play = drive.get(p);
					PbpValidateService.validate(play);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Add Time
	 */
	private static List<DrivePojo> applyTime(List<List<PbpServiceRequestPojo>> gameInfo) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

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

			LoggingUtils.logInfo("- [aggregateDrives] -");
			for (int d = 0; d < drivesInGame; d++) {
				DrivePojo drive = driveMap.get(d);

				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					LoggingUtils.logInfo(String.format(DRIVE_NUMBER_S, d));
					LoggingUtils.logInfo(String.format("Drive Start Time: %s", drive.getDriveStartTime()));
					LoggingUtils.logInfo(String.format(DRIVE_END_TIME_S, drive.getDriveEndTime()));
					LoggingUtils.logInfo(String.format("Drive Start Yardline: %s", drive.getDriveStartYardline()));
					LoggingUtils.logInfo(String.format("Drive End Yardline: %s", drive.getDriveEndYardline()));
				}

				if (!periodDrives.get(PlayPeriodEnum.FIRST).getStartDrive().equals(d)
						&& !periodDrives.get(PlayPeriodEnum.THIRD).getStartDrive().equals(d)) {
					// If field goal miss did not account for loss of yards from ball spot
					// Use following drive start yardline
					if (DriveResultEnum.FG.equals(drives.get(d - 1).getDriveResult())
							&& drives.get(d - 1).getDriveResultPoint().equals(0)) {
						if (drive.getDriveStartYardline() < 100 - drives.get(d - 1).getDriveStartYardline()) {
							Integer pullforwardYardLine = (int) 100 - drive.getDriveStartYardline();
							drives.get(d - 1).setDriveEndYardline(pullforwardYardLine);
							drives.get(d - 1).getDrivePlays().get(drives.get(d - 1).getDrivePlays().size() - 1)
									.getPlayResult().setPlayResultYardLine(pullforwardYardLine);
						}
					}
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
			LoggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	/**
	 * Apply time logic on second pass over drives
	 */
	private static void applyTimePerDriveHelper(HashMap<Integer, DrivePojo> driveMap,
			EnumMap<PlayPeriodEnum, DriveTimesPojo> periodDrives, Integer d) {
		try {
			if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
				LoggingUtils.logInfo(String.format("-- Drive Number: %s", d));
			}
			// Skip for first drive of the second half
			// Set next drive start time based on drive end time
			if (driveMap.containsKey(d + 1) && periodDrives.get(PlayPeriodEnum.THIRD).getStartDrive() != d + 1
					&& driveMap.get(d + 1).getDriveStartTime() > driveMap.get(d).getDriveEndTime()) {
				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					LoggingUtils.logInfo(String.format(DRIVE_END_TIME_S, driveMap.get(d).getDriveEndTime()));
					LoggingUtils.logInfo(
							String.format("Next Drive Start Time: %s", driveMap.get(d + 1).getDriveStartTime()));
				}
				driveMap.get(d + 1).setDriveStartTime(driveMap.get(d).getDriveEndTime());
			}
			// Skip for last drive of the first half
			// Set drive time based on next drive end time
			if (driveMap.containsKey(d + 1) && !periodDrives.get(PlayPeriodEnum.SECOND).getEndDrive().equals(d)
					&& driveMap.get(d + 1).getDriveStartTime() < driveMap.get(d).getDriveEndTime()) {
				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					LoggingUtils.logInfo(String.format(DRIVE_END_TIME_S, driveMap.get(d).getDriveEndTime()));
					LoggingUtils.logInfo(
							String.format("Next Drive Start Time: %s", driveMap.get(d + 1).getDriveStartTime()));
				}
				driveMap.get(d).setDriveEndTime(driveMap.get(d + 1).getDriveStartTime());
			}
			if (!periodDrives.get(PlayPeriodEnum.FIRST).getStartDrive().equals(d)
					&& !periodDrives.get(PlayPeriodEnum.THIRD).getStartDrive().equals(d)) {
				// If time difference for drive was all applied to following kickoff
				// Roll drive end time forward to apply all to drive and no time to kickoff
				if (DriveResultEnum.KICKOFF.equals(driveMap.get(d).getDriveResult())
						&& !driveMap.get(d).getDriveStartTime().equals(driveMap.get(d).getDriveEndTime())) {
					if (DriveResultEnum.TD.equals(driveMap.get(d - 1).getDriveResult())
							&& driveMap.get(d - 1).getDriveStartTime().equals(driveMap.get(d - 1).getDriveEndTime())
							&& driveMap.get(d).getDriveStartTime().equals(driveMap.get(d - 1).getDriveEndTime())) {
						Integer pullforwardDriveEnd = (Integer) driveMap.get(d).getDriveEndTime();
						driveMap.get(d - 1).setDriveEndTime(pullforwardDriveEnd);
						driveMap.get(d).setDriveStartTime(pullforwardDriveEnd);
					}
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Add Time - Per Drive
	 */
	private static void applyTimeDriveHelper(List<PbpServiceRequestPojo> drive, Integer d,
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
				if (p < playsInPossession - 1 && DriveResultEnum.TURNOVER.equals(standardDrive.getDriveResult())) {
					String logInfo = String.format("Play text: %s", play.getPlayRawText());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException("Turnover is not the last play of drive");
				}
			}
			if (Objects.isNull(standardDrive.getDriveEndYardline()) && drive.size() == 1
					&& PlayTypeEnum.PAT.equals(drive.get(0).getPlay().getPlayType())) {
				standardDrive.setDriveEndYardline(drive.get(0).getPlay().getPlayStartYard());
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
			LoggingUtils.logException(e, e.toString());

		}
	}

	/**
	 * Add Time - Per Play
	 */
	private static void applyTimePlayHelper(PbpServiceRequestPojo play, Integer p, DrivePojo standardDrive) {
		try {

			applyTimePlayHelperFirstPlay(play, standardDrive, p);

			if (Objects.nonNull(standardDrive.getDriveResult())
					&& !DriveResultEnum.TD.equals(standardDrive.getDriveResult())) {
				String logInfo = String.format("Play text: %s | Drive result: %s", play.getPlayRawText(),
						standardDrive.getDriveResult());
				LoggingUtils.logInfo(logInfo);
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

			applyTimePlayHelperOffensePlayType(play, standardDrive);

			if (PlayCallTypeEnum.PUNT.equals(play.getPlay().getPlayCallType())) {
				standardDrive.addDriveResultPoint(play.getPlay().getPlayResult().getPlayResultPoints() * -1);
			} else {
				standardDrive.addDriveResultPoint(play.getPlay().getPlayResult().getPlayResultPoints());
			}

			if (!standardDrive.getPossessionTeamId().equals(play.getPossessionTeam())
					&& !PlayTypeEnum.PUNT.equals(play.getPlay().getPlayType())) {
				throw new IllegalArgumentException("drive includes mulitple possession teams");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void applyTimePlayHelperOffensePlayType(PbpServiceRequestPojo play, DrivePojo standardDrive) {
		try {
			if (PlayTypeEnum.OFFENSE.equals(play.getPlay().getPlayType())
					&& !PlayCallTypeEnum.FG.equals(play.getPlay().getPlayCallType())) {
				if (play.getPlay().getPlayStartYard() >= 60) {
					standardDrive.setDriveResultScoringOpp(true);
				}
				if (Boolean.FALSE.equals(play.getPlay().getPlayResult().isPlayResultTurnover())) {
					standardDrive.addDriveOffenseYard(play.getPlay().getPlayResult().getPlayResultYard());
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Extra logic for first play in drive
	 */
	private static void applyTimePlayHelperFirstPlay(PbpServiceRequestPojo play, DrivePojo standardDrive, Integer p) {
		try {
			if (p == 0) {
				standardDrive.setDriveStartTime(play.getDrive().getDriveStartTime());
				standardDrive.setDriveEndTime(play.getDrive().getDriveEndTime());
				standardDrive.setPossessionTeamId(play.getPossessionTeam());
				standardDrive.setDriveStartYardline(play.getPlay().getPlayStartYard());
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Add Play Result - Per Play
	 */
	private static void applyTimePlayHelperApplyDriveResult(PbpServiceRequestPojo play, DrivePojo standardDrive) {
		try {
			if (Boolean.FALSE.equals(play.getPlay().getNoPlayPenalty())) {
				if (play.getPlay().getPlayResult().getPlayResultPoints() == 6) {
					if (PlayTypeEnum.PUNT.equals(play.getPlay().getPlayType())) {
						standardDrive.setDriveResult(DriveResultEnum.PUNT_TD);
					} else if (PlayTypeEnum.KICKOFF.equals(play.getPlay().getPlayType())) {
						standardDrive.setDriveResult(DriveResultEnum.KICKOFF_TD);
					} else {
						standardDrive.setDriveResult(DriveResultEnum.TD);
					}
				} else if (PlayTypeEnum.PUNT.equals(play.getPlay().getPlayType())) {
					standardDrive.setDriveResult(DriveResultEnum.PUNT);
				} else if (Boolean.TRUE.equals(play.getPlay().getPlayResult().isPlayResultTurnover())) {
					if (play.getPlay().getPlayResult().getPlayResultPoints() == -6) {
						standardDrive.setDriveResult(DriveResultEnum.TURNOVER_TD);
					} else {
						standardDrive.setDriveResult(DriveResultEnum.TURNOVER);
					}
				} else if (PlayCallTypeEnum.FG.equals(play.getPlay().getPlayCallType())) {
					standardDrive.setDriveResult(DriveResultEnum.FG);
				} else if (PlayTypeEnum.KICKOFF.equals(play.getPlay().getPlayType())) {
					standardDrive.setDriveResult(DriveResultEnum.KICKOFF);
				} else if (Objects.isNull(standardDrive.getDriveResult())
						&& PlayTypeEnum.PAT.equals(play.getPlay().getPlayType())) {
					standardDrive.setDriveResult(DriveResultEnum.PAT);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	/**
	 * Generate MAP with the start drive and end drive indices for each quarter
	 */
	private static EnumMap<PlayPeriodEnum, DriveTimesPojo> pullPeriodStartEndDrive(
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
			LoggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static PlayPojo initializePlay(PlayPeriodEnum playPeriod, String playRawText, String driveText,
			String possessionTeam, GameMapPojo gameMaps) {
		PlayPojo play = new PlayPojo();
		play.setPeriod(playPeriod);
		play.setPlayText(playRawText);
		play.setDriveText(driveText.replace("AMP;", ""));
		play.setPlayStartPossessionTeamId(possessionTeam);
		for (String teamId : gameMaps.getTeamDict().keySet()) {
			play.getPlayerStat().put(teamId, new PbpPlayerStatPojo());
		}
		return play;
	}

	private static List<List<PbpServiceRequestPojo>> driveFirstPassParse(PlayByPlayPojo playByPlayRaw, GamePojo game,
			GameMapPojo gameMaps) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			List<List<PbpServiceRequestPojo>> gameInfo = new ArrayList<>();
			Integer periodsInGame = playByPlayRaw.getPeriods().size();
			for (int q = 0; q < periodsInGame; q++) {
				PlayByPlayPeriodPojo period = playByPlayRaw.getPeriods().get(q);
				PlayPeriodEnum playPeriod = period.getTitleEnum();

				Integer drivesInPeriod = period.getPossessions().size();

				PlayParseStartTimeEndTime startTimeEndTime = new PlayParseStartTimeEndTime(
						PbpParsingUtils.convertMinSecToSec(QUARTER_START, playPeriod),
						PbpParsingUtils.convertMinSecToSec(QUARTER_START, playPeriod));
				for (int d = 0; d < drivesInPeriod; d++) {
					PlayByPlayPossessionPojo possession = period.getPossessions().get(d);
					driveFirstPassParseDriveHelper(game, possession, playByPlayRaw, startTimeEndTime, playPeriod,
							gameInfo, gameMaps);
				}
			}
			return gameInfo;
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void driveFirstPassParseDriveHelper(GamePojo game, PlayByPlayPossessionPojo possession,
			PlayByPlayPojo playByPlayRaw, PlayParseStartTimeEndTime startTimeEndTime, PlayPeriodEnum playPeriod,
			List<List<PbpServiceRequestPojo>> gameInfo, GameMapPojo gameMaps) {
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
				startTimeEndTime.setStartTime(PbpParsingUtils.convertMinSecToSec(possession.getTime(), playPeriod));
				startTimeEndTime.setEndTime(PbpParsingUtils.convertMinSecToSec(possession.getTime(), playPeriod));
			}

			Integer playsInPossession = possession.getPlays().size();
			for (int p = 0; p < playsInPossession; p++) {
				PlayByPlayPlayPojo playRaw = possession.getPlays().get(p);
				driveFirstPassParsePlayHelper(playRaw, playPeriod, possessionTeam, defenseTeam, startTimeEndTime,
						driveInfo, gameMaps);
			}
			if (!driveInfo.isEmpty()) {
				gameInfo.add(driveInfo);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void driveFirstPassParsePlayHelper(PlayByPlayPlayPojo playRaw, PlayPeriodEnum playPeriod,
			String possessionTeam, String defenseTeam, PlayParseStartTimeEndTime startTimeEndTime,
			List<PbpServiceRequestPojo> driveInfo, GameMapPojo gameMaps) {
		try {
			PlayPojo play;
			String playRawText = playRaw.getScoreText().replace("  ", " ").replaceAll("#\\d{1,2} ", "")
					.replace(" III,", ",").replace(" Jr ", " ");

			String suffixReplacementRegexCondition = "([A-Z]\\.[A-Z][a-z]+ [A-Z] )";
			if (PbpParsingUtils.evalMatch(playRawText, suffixReplacementRegexCondition)) {
				String suffixReplaceFull = PbpParsingUtils
						.extractCustom(playRawText, suffixReplacementRegexCondition, 1).split("\\~")[0];
				String suffixReplaceNew = suffixReplaceFull.split(" ")[0];
				playRawText = playRawText.replace(suffixReplaceFull.strip(), suffixReplaceNew);
			}

			String endTimeRegexCondition = "\\(?(\\d{1,2}:\\d{1,2})\\)?";
			if (PbpParsingUtils.evalMatch(playRawText, endTimeRegexCondition)) {
				String timeString = PbpParsingUtils.extractCustom(playRawText, endTimeRegexCondition, 1).split("~")[0];
				startTimeEndTime.setEndTime(PbpParsingUtils.convertMinSecToSec(timeString, playPeriod));
			}
			playRawText = playRawText.replaceAll("\\(?\\d{1,2}:\\d{1,2}\\)? ?", "");
			if (playRawText.contains("score nullified by penalty. ")) {
				playRawText = playRawText.split("score nullified by penalty\\. ")[1];
			}

			if (playRawText.contains("nullified")) {
				System.out.println("asdfadsfadfs");
			}
			if (filterPlaysByText(playRawText)) {
				play = initializePlay(playPeriod, playRawText, playRaw.getDriveText(), possessionTeam, gameMaps);
			} else {
				return;
			}

			playRawText = cleanUpPlayText(playRawText, possessionTeam, defenseTeam, gameMaps);
			String[] playTackles = PbpParsingUtils.extractTackle(playRawText);

			if (playRawText.startsWith(" ")) {
				playRawText = playRawText.stripLeading();
				play.setPlayText(playRawText);
			}

			PbpServiceRequestPojo serviceRequest = new PbpServiceRequestPojo(
					new DrivePojo(possessionTeam, playPeriod, startTimeEndTime.getStartTime(),
							startTimeEndTime.getEndTime()),
					play, playRaw, playRawText, playTackles, gameMaps.getTeamDict(), gameMaps.getTeamAbbrevDict());
			serviceRequest.setPossessionTeam(possessionTeam);
			serviceRequest.setDefenseTeam(defenseTeam);
			cleanUpPlay(serviceRequest);

			if (serviceRequest.getPlay().getDriveText().isEmpty()
					&& !PlayTypeEnum.KICKOFF.equals(serviceRequest.getPlay().getPlayType())
					&& !PlayTypeEnum.PAT.equals(serviceRequest.getPlay().getPlayType())) {
				return;
			}
			addEndYard(serviceRequest);
			if ("A. McNulty kickoff 50 yards to the WAG15, N Simmons return 0 yards to the WAG15 (C. Tate)"
					.equals(serviceRequest.getPlayRawText())) {
				serviceRequest.getPlay().getPlayResult().setPlayResultYardLine(15);
			}
			serviceRequest.getPlay().getPlayResult().setPlayResultPossessionTeamId(serviceRequest.getPossessionTeam());
			boolean declinedNoPlay = PbpPenaltyParseService.parsePenalty(serviceRequest);
			if (declinedNoPlay) {
				return;
			}

			driveInfo.add(serviceRequest);
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static boolean filterPlaysByText(String playRawText) {
		try {
			if (playRawText.contains("Start of ") && playRawText.contains("quarter")) {
				return false;
			} else if (PbpParsingUtils.evalMatch(playRawText, "(?i)w[io]ns?(?: the)?(?: coin)? toss")) {
				return false;
			} else if (playRawText.contains(" drive start at ")) {
				return false;
			} else if (playRawText.contains("End of game")) {
				return false;
			} else if (playRawText.contains("will receive")) {
				return false;
			} else if (playRawText.toUpperCase().contains("CAPTAINS")) {
				return false;
			} else if (playRawText.contains("free kick")) {
				return false; // TODO account for these
			} else if (playRawText.startsWith("End of ")) {
				return false;
			} else if (playRawText.startsWith("Timeout")) {
				return false; // TODO account for these
			} else if (PbpParsingUtils.evalMatch(playRawText, "^[a-z|A-Z]{2,5} ball on [a-z|A-Z]{2,5}\\d{1,2}\\.?$")) {
				return false;
			} else if (PbpParsingUtils.evalMatch(playRawText, "^(\\d[a-z]{2}) and (\\d+)\\.$")) {
				return false;
			} else if (PbpParsingUtils.evalMatch(playRawText, "^QB hurry by ")) {
				return false;
			} else if (PbpParsingUtils.evalMatch(playRawText, "^Clock \\.$")) {
				return false;
			} else if (PbpParsingUtils.evalMatch(playRawText, "^ ?\\(.+\\)\\.$")) {
				return false;
			} else if (PbpParsingUtils.evalMatch(playRawText, "^Change of possession.+ 1st and 10\\.$")) {
				return false;
			} else if (PbpParsingUtils.evalMatch(playRawText, "^[A-Z]{3} Capt. [\\d,]+$")) {
				return false;
			} else {
				return true;
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}

	}

	private static void addTempoType(PbpServiceRequestPojo serviceRequest) {
		try {
			String playRawText = serviceRequest.getPlay().getPlayText();
			if (playRawText.contains("[NHSG]")) {
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
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static String cleanUpPlayText(String playRawText, String possessionTeam, String defenseTeam,
			GameMapPojo gameMaps) {
		try {

			Map<String, PlayByPlayTeamPojo> teamAbbrevDict = gameMaps.getTeamAbbrevDict();
			playRawText = playRawText.replace("'", "");
			playRawText = playRawText.replace("Jr,", "Jr");
			playRawText = playRawText.replace("([^,])( Jr\\.) ", "$1 ");
			playRawText = playRawText.replaceAll("(([A-Z])\\.([A-Z][a-z]*))", "$2. $3");
			playRawText = playRawText.replaceAll("([A-Z])\\. ([A-Z])\\.", "$1$2");
			playRawText = playRawText.replaceAll("((, [A-Z])\\. )", "$2 ");
			playRawText = playRawText.replace(" . ", " ");
			playRawText = playRawText.replaceAll("(\\(OVERTURNED PLAY: .*\\))", "");
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
			playRawText = playRawText.replaceAll("([A-Z]{1,2} [a-z|A-Z]+) III", "$1");
			playRawText = playRawText.replaceAll("([A-Z]\\. [A-Z][a-z]+),?( II)", "$1");
			playRawText = playRawText.replaceAll("(, [A-Z] [A-Z][a-z]+),?( II) ", "$1 ");

			playRawText = playRawText.replaceAll("((, [A-Z])\\. )", "$2 ");
			playRawText = playRawText.replaceAll("([a-z|A-Z]+?) Jr.,([a-z|A-Z]*+)", "$1,$2");
			playRawText = playRawText.replace("C. La Chapelle", "C. LaChapelle");
			playRawText = playRawText.replace("A. Junior Ellis", "A. JuniorEllis");
			playRawText = playRawText.replace("A Junior Ellis", "A. JuniorEllis");
			playRawText = playRawText.replace("[NHPUNT]", "");

			playRawText = playRawText.replaceAll("([A-Z]\\.) ([a-z|A-Z]*+) IV ", "$1 $2 ");
			playRawText = playRawText.replaceAll("([A-Z][a-z]{0,3}\\.) ([a-z|A-Z]*+) IV ", "$1 $2 ");

			playRawText = playRawText.replaceAll("([a-z|A-Z]+?)-([a-z|A-Z]+?),([a-z|A-Z]*+)", "$1$2,$3");
			playRawText = playRawText.replace("Vander Esch,Caleb", "VanderEsch,Caleb");
			playRawText = playRawText.replace("Holmes,T Quele", "Holmes,TQuele");
			playRawText = playRawText.replace("Baker,Justin Richard", "Baker,JustinRichard");
			playRawText = playRawText.replaceAll(String.format("%s( Jr\\.)", NcaaConstants.PLAYER_NAME_REGEX), "$1");
			playRawText = playRawText.replaceAll("([A-Z][a-z|A-Z']+ St)\\.", "$1");
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

			playRawText = playRawText.replace(
					". PENALTY Before the snap, BUF Illegal Formation  enforced at the deadball spot for 5 yards from the WAG15 to the WAG20",
					"");

			playRawText = playRawText.replace(
					". PENALTY Before the snap, BUF Illegal Formation  enforced at the deadball spot for 5 yards from the WAG15 to the WAG20",
					"");

			playRawText = playRawText.replace(
					"T. Pastula punt 38 yards to the MAI15, J Hennie return 38 yards to the DEL47",
					"T. Pastula punt 38 yards to the MAI15, fair catch by J Hennie");

			if (playRawText.contains("Pastula punt 38 yards to the MAI15")) {
				System.out.println("catch");
			}
			playRawText = playRawText.replace(
					"C. Reed kickoff 56 yards to the SJS9, S Garrett return 0 yards to the SJS9 (J. Narayan). PENALTY SJS Holding on M. Tago enforced 10 yards from the SJS40 to the SJS30. NO PLAY (replay the down)",
					"C. Reed kickoff 56 yards to the SJS9, S Garrett return 0 yards to the SJS9 (J. Narayan). PENALTY SJS Holding on M. Tago enforced 10 yards from the SJS40 to the SJS30");
			return playRawText;
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
			throw new IllegalArgumentException(e.toString());
		}

	}

	private static void cleanUpYards(PbpServiceRequestPojo serviceRequest) {
		try {

			if (PlayTypeEnum.PAT.equals(serviceRequest.getPlay().getPlayType())) {
				cleanUpYardsPatHelper(serviceRequest);
			} else if (PlayCallTypeEnum.KICKOFF.equals(serviceRequest.getPlay().getPlayCallType())) {
				// Nothing needed here for now
			} else if (PlayTypeEnum.PUNT.equals(serviceRequest.getPlay().getPlayType())) {
				cleanUpYardsPuntHelper(serviceRequest);
			} else {
				cleanUpYardsOffenseHelper(serviceRequest);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void cleanUpYardsPatHelper(PbpServiceRequestPojo serviceRequest) {
		try {
			if ("".equals(serviceRequest.getPlay().getDriveText())
					|| serviceRequest.getPlay().getDriveText().endsWith("at -")) {
				serviceRequest.getPlay().setPlayStartYard(97);
				serviceRequest.getPlay().setPlayYardToGain(3);
			} else {
				String[] downAndDistance = PbpParsingUtils
						.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
				serviceRequest.getPlay().setPlayStartYard(
						PbpParsingUtils.formatYardLine(downAndDistance[2], serviceRequest.getPossessionTeam(),
								serviceRequest.getDefenseTeam(), serviceRequest.getTeamAbbrevDict()));
				serviceRequest.getPlay().setPlayYardToGain(100 - serviceRequest.getPlay().getPlayStartYard());
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void cleanUpYardsPuntHelper(PbpServiceRequestPojo serviceRequest) {
		try {
			String[] downAndDistance = PbpParsingUtils.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
			serviceRequest.getPlay().setPlayStartYard(
					PbpParsingUtils.formatYardLine(downAndDistance[2], serviceRequest.getDefenseTeam(),
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
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void cleanUpYardsOffenseHelper(PbpServiceRequestPojo serviceRequest) {
		try {
			String[] downAndDistance = PbpParsingUtils.convertDownAndDistance(serviceRequest.getPlay().getDriveText());
			serviceRequest.getPlay().setPlayStartYard(
					PbpParsingUtils.formatYardLine(downAndDistance[2], serviceRequest.getPossessionTeam(),
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
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void cleanUpPlay(PbpServiceRequestPojo serviceRequest) {
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
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void cleanUpPlayKickoffHelper(PbpServiceRequestPojo serviceRequest) {
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
				serviceRequest.getPlay().setPlayCallType(PlayCallTypeEnum.KICKOFF);
			} else {
				serviceRequest.getDrive().setKickoff(false);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}
	}

	private static void cleanUpPlayNonKickoffHelper(PbpServiceRequestPojo serviceRequest) {
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
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void addStartYardPenalty(PbpServiceRequestPojo serviceRequest) {
		try {
			boolean noPlay = serviceRequest.getPlay().getNoPlayPenalty();
			if (Objects.isNull(serviceRequest.getPlay().getPlayStartYard()) && noPlay) {
				String regexCondition = String.format("%s (?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.TEAM_YARD_REGEX,
						NcaaConstants.TEAM_YARD_REGEX);
				if (PbpParsingUtils.evalMatch(serviceRequest.getPlay().getPlayText(), regexCondition)) {
					String startYardString = PbpParsingUtils.extractCustom(serviceRequest.getPlay().getPlayText(),
							regexCondition, 2);
					String[] startYardStringArrays = startYardString.split("\\|");
					int startYardMatches = startYardStringArrays.length;
					if (startYardMatches == 0) {
						throw new IllegalArgumentException("Start regex extraction length = 0");
					}
					String[] startYardStringArray = startYardStringArrays[startYardMatches - 1].split("\\~");
					Integer startYard = PbpParsingUtils.formatYardLine(startYardStringArray[0],
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
			LoggingUtils.logException(e, e.toString());

		}

	}

	private static void reconcileNoPlayHalfDistance(PbpServiceRequestPojo params) {
		try {
			if (Boolean.TRUE.equals(params.getPlay().getNoPlayPenalty())
					&& Objects.isNull(params.getPlay().getPlayResult().getPlayResultYardLine())) {
				List<PlayerStatPenaltyPojo> offensePenalty = params.getPlay().getPlayerStat()
						.get(params.getPossessionTeam()).getPenalty();
				List<PlayerStatPenaltyPojo> defensePenalty = params.getPlay().getPlayerStat()
						.get(params.getDefenseTeam()).getPenalty();

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
					halfDistance = ((100 - startYard) / 2);
					if (defensePenalty.get(0).getPenaltyYards() > halfDistance) {
						defensePenalty.get(0).setPenaltyYards(halfDistance);
						params.getPlay().getPlayResult().setPlayResultYard(halfDistance);
						params.getPlay().getPlayResult().setPlayResultYardLine(startYard + halfDistance);
					}
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void addEndYard(PbpServiceRequestPojo serviceRequest) {
		try {
			if (PbpParsingUtils.evalMatch(serviceRequest.getPlay().getPlayText(), "Touchback.$")) {
				if (PlayTypeEnum.KICKOFF.equals(serviceRequest.getPlay().getPlayType())) {
					serviceRequest.getPlay().getPlayResult().setPlayResultYardLine(25);
				} else {
					serviceRequest.getPlay().getPlayResult().setPlayResultYardLine(20);
				}
			} else if (Objects.isNull(serviceRequest.getPlay().getPlayResult().getPlayResultYardLine())
					&& PbpParsingUtils.evalMatch(serviceRequest.getPlayRawText(),
							String.format("(?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.TEAM_YARD_REGEX))) {
				String endYardString = PbpParsingUtils.extractCustom(serviceRequest.getPlayRawText(),
						String.format("(?:(?:to)|(?:at))(?: the)?%s", NcaaConstants.TEAM_YARD_REGEX), 1);
				String[] endYardStringArrays = endYardString.split("\\|");
				int endYardMatches = endYardStringArrays.length;
				if (endYardMatches == 0) {
					throw new IllegalArgumentException("HANDLE THIS");
				}
				String[] endYardStringArray = endYardStringArrays[endYardMatches - 1].split("\\~");
				Integer endYard = PbpParsingUtils.formatYardLine(endYardStringArray[0],
						serviceRequest.getPossessionTeam(), serviceRequest.getDefenseTeam(),
						serviceRequest.getTeamAbbrevDict());
				serviceRequest.getPlay().getPlayResult().setPlayResultYardLine(endYard);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());

		}

	}

}
