package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp.validate;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.ThreadContext;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DriveResultEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive.HomeAwayScoreMeta;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;

public final class DriveValidationService {
	private static final String LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S = "Last drive end yardline: %s | Drive start yardline: %s";
	private static final String DRIVE_END_TIME_S = "Drive End Time: %s";
	private static final String DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE = "!drives.get(d-1).getDriveEndYardline().equals(drives.get(d).getDriveStartYardline())";
	private static final String STARTING_S = " -- Starting [%s]";

	// Private constructor to prevent instantiation
	private DriveValidationService() {
		throw new UnsupportedOperationException();
	}

	public static void validateDrives(List<DrivePojo> drives) {
		try {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String methodStartInfo = String.format(STARTING_S, ste[1].getMethodName());
			LoggingUtils.logInfo(methodStartInfo);

			HomeAwayScoreMeta homeAwayScoreMeta = new HomeAwayScoreMeta();
			Integer drivesInGame = drives.size();
			for (int d = 0; d < drivesInGame; d++) {
				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					LoggingUtils.logInfo(String.format("-- Drive Number: %s", d));
					LoggingUtils.logInfo(String.format("Drive Start Period: %s", drives.get(d).getDriveStartPeriod()));
					LoggingUtils.logInfo(String.format("Drive End Period: %s", drives.get(d).getDriveEndPeriod()));
					LoggingUtils.logInfo(String.format("Drive Start Time: %s", drives.get(d).getDriveStartTime()));
					LoggingUtils.logInfo(String.format(DRIVE_END_TIME_S, drives.get(d).getDriveEndTime()));
					LoggingUtils.logInfo(String.format("Drive Total Time: %s", drives.get(d).getDriveTotalTime()));
					LoggingUtils
							.logInfo(String.format("Drive Start Yardline: %s", drives.get(d).getDriveStartYardline()));
					LoggingUtils.logInfo(String.format("Drive End Yardline: %s", drives.get(d).getDriveEndYardline()));
					LoggingUtils.logInfo(String.format("Drive Total Yard: %s", drives.get(d).getDriveTotalYard()));
					LoggingUtils.logInfo(String.format("Drive Offense Yards: %s", drives.get(d).getDriveOffenseYard()));
					LoggingUtils.logInfo(String.format("Drive Result: %s", drives.get(d).getDriveResult()));
					LoggingUtils.logInfo(String.format("Drive Result Point: %s", drives.get(d).getDriveResultPoint()));
					LoggingUtils.logInfo(String.format("Drive Kickoff: %s", drives.get(d).getKickoff()));
					LoggingUtils
							.logInfo(String.format("Drive Offense Plays: %s", drives.get(d).getDriveOffensePlays()));
				}

				validateDrive(drives.get(d));
				validateDriveIncreasingScores(drives.get(d), homeAwayScoreMeta);

				if (d != 0) {
					if (!DriveResultEnum.END_HALF.equals(drives.get(d - 1).getDriveResult())) {
						validateDrivesNotEndHalfHelper(drives, d);
					} else {
						validateDrivesEndHalfHelper(drives, d);
					}
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesNotEndHalfHelper(List<DrivePojo> drives, Integer d) {
		try {
			validateDrivesTimeHelper(drives, d);

			if (DriveResultEnum.KICKOFF.equals(drives.get(d).getDriveResult())) {
				validateDrivesKickoffHelper(drives, d);
			} else if (DriveResultEnum.PAT.equals(drives.get(d).getDriveResult())) {
				validateDrivesPatHelper(drives, d);
			} else if (DriveResultEnum.PUNT.equals(drives.get(d - 1).getDriveResult())) {
				validateDrivesPuntHelper(drives, d);
			} else if (DriveResultEnum.FG.equals(drives.get(d - 1).getDriveResult())) {
				validateDrivesFieldGoalHelper(drives, d);
			} else if (DriveResultEnum.TURNOVER.equals(drives.get(d - 1).getDriveResult())) {
				validateDrivesTurnoverHelper(drives, d);
			} else {
				if (!drives.get(d - 1).getDriveEndYardline().equals(drives.get(d).getDriveStartYardline())) {
					String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
							drives.get(d - 1).getDriveEndYardline(), drives.get(d).getDriveStartYardline());
					LoggingUtils.logInfo(logInfo);
					throw new IllegalArgumentException(
							DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesKickoffHelper(List<DrivePojo> drives, Integer d) {
		try {
			if (drives.get(d - 1).getDriveEndYardline().equals(drives.get(d).getDriveStartYardline())) {
				String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
						drives.get(d - 1).getDriveEndYardline(), drives.get(d).getDriveStartYardline());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(
						"drives.get(d-1).getDriveEndYardline().equals(drives.get(d).getDriveStartYardline())");
			}
			if (!DriveResultEnum.FG.equals(drives.get(d - 1).getDriveResult())
					&& !DriveResultEnum.PAT.equals(drives.get(d - 1).getDriveResult())
					&& !drives.get(d - 1).getDriveEndYardline().equals(100)) {
				throw new IllegalArgumentException("!drives.get(d - 1).getDriveEndYardline().equals(100)");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesPatHelper(List<DrivePojo> drives, Integer d) {
		try {
			if (!DriveResultEnum.TURNOVER_TD.equals(drives.get(d - 1).getDriveResult())
					&& !DriveResultEnum.PUNT_TD.equals(drives.get(d - 1).getDriveResult())) {
				throw new IllegalArgumentException(
						"!DriveResultEnum.TURNOVER.equals(drives.get(d - 1).getDriveResult())");
			}
			if (!drives.get(d).getDriveOffensePlays().equals(0)) {
				throw new IllegalArgumentException("!drives.get(d).getDriveOffensePlays().equals(0)");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesPuntHelper(List<DrivePojo> drives, Integer d) {
		try {
			if (!drives.get(d - 1).getDriveEndYardline().equals(100 - drives.get(d).getDriveStartYardline())) {
				String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
						100 - drives.get(d - 1).getDriveEndYardline(), drives.get(d).getDriveStartYardline());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(
						DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesFieldGoalHelper(List<DrivePojo> drives, Integer d) {
		try {
			if (!drives.get(d - 1).getDriveEndYardline().equals(100 - drives.get(d).getDriveStartYardline())) {
				String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
						100 - drives.get(d - 1).getDriveEndYardline(), drives.get(d).getDriveStartYardline());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(
						DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesTurnoverHelper(List<DrivePojo> drives, Integer d) {
		try {
			if (!drives.get(d - 1).getDriveEndYardline().equals(100 - drives.get(d).getDriveStartYardline())) {
				String logInfo = String.format(LAST_DRIVE_END_YARDLINE_S_DRIVE_START_YARDLINE_S,
						100 - drives.get(d - 1).getDriveEndYardline(), drives.get(d).getDriveStartYardline());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(
						DRIVES_GET_D_1_GET_DRIVE_END_YARDLINE_EQUALS_DRIVES_GET_D_GET_DRIVE_START_YARDLINE);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesEndHalfHelper(List<DrivePojo> drives, Integer d) {
		try {
			if (!drives.get(d).getDriveStartTime().equals(1800)) {
				String logInfo = String.format("Last drive result: %s | Drive start time: %s",
						drives.get(d - 1).getDriveResult(), drives.get(d).getDriveStartTime());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("!drives.get(d).getDriveStartTime().equals(1800)");
			}
			if (!DriveResultEnum.KICKOFF.equals(drives.get(d).getDriveResult())) {
				String logInfo = String.format("Last drive result: %s | Drive result: %s",
						drives.get(d - 1).getDriveResult(), drives.get(d).getDriveResult());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("!DriveResultEnum.KICKOFF.equals(drives.get(d).getDriveResult())");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDrivesTimeHelper(List<DrivePojo> drives, Integer d) {
		try {
			if (!drives.get(d).getDriveStartTime().equals(drives.get(d - 1).getDriveEndTime())) {
				String logInfo = String.format("Last drive end time: %s | Drive start time: %s",
						drives.get(d - 1).getDriveEndTime(), drives.get(d).getDriveStartTime());
				LoggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException(
						"!drives.get(d).getDriveStartTime().equals(drives.get(d - 1).getDriveEndTime())");
			}

			if (drives.get(d).getDrivePlays().stream()
					.filter(p -> Boolean.FALSE.equals(p.getNoPlayPenalty())
							&& !PlayTypeEnum.PAT.equals(p.getPlayType()))
					.collect(Collectors.toList()).size() > 1 && drives.get(d).getDriveTotalTime() <= 0) {
				throw new IllegalArgumentException(
						"drives.get(d).getDrivePlays().size() > 1 && drives.get(d).getDriveTotalTime() <= 0");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.toString());
		}
	}

	private static void validateDriveIncreasingScores(DrivePojo drive, HomeAwayScoreMeta homeAwayScoreMeta) {
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

	private static void validateDrive(DrivePojo drive) {

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

		validatDriveScores(drive);
		validateKickoff(drive);

		if (drive.getDriveStartTime() < drive.getDriveEndTime()) {
			throw new IllegalArgumentException("drive.getDriveStartTime() < drive.getDriveEndTime()");
		}

	}

	private static void validatDriveScores(DrivePojo drive) {
		if (!drive.getDriveStartAwayScore().equals(drive.getDriveResultAwayScore())
				|| !drive.getDriveStartHomeScore().equals(drive.getDriveResultHomeScore())) {
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
	}

	private static void validatePlayIncreasingScores(DrivePojo drive) {
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

	private static void validateDriveScoreMatchLastPlay(DrivePojo drive) {
		if (!drive.getDriveStartHomeScore().equals(drive.getDrivePlays().get(0).getPlayStartHomeScore())) {

			String actualValueLogStr = String.format("HOME | DRIVE | START | score: %s",
					drive.getDriveStartHomeScore());
			String expectedValueLogStr = String.format("HOME | FIRST PLAY | START | score: %s",
					drive.getDrivePlays().get(0).getPlayStartHomeScore());
			LoggingUtils.logInfo(actualValueLogStr);
			LoggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveStartHomeScore() != drive.getDrivePlays().get(0).getPlayStartHomeScore()");
		}
		if (!drive.getDriveStartAwayScore().equals(drive.getDrivePlays().get(0).getPlayStartAwayScore())) {

			String actualValueLogStr = String.format("AWAY | DRIVE | START | score: %s",
					drive.getDriveStartAwayScore());
			String expectedValueLogStr = String.format("AWAY | FIRST PLAY | START | score: %s",
					drive.getDrivePlays().get(0).getPlayStartAwayScore());
			LoggingUtils.logInfo(actualValueLogStr);
			LoggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveStartAwayScore() != drive.getDrivePlays().get(0).getPlayStartAwayScore()");
		}
		if (!drive.getDriveResultHomeScore().equals(
				drive.getDrivePlays().get(drive.getDrivePlays().size() - 1).getPlayResult().getPlayResultHomeScore())) {

			String actualValueLogStr = String.format("HOME | DRIVE | RESULT | score: %s",
					drive.getDriveResultHomeScore());
			String expectedValueLogStr = String.format("HOME | LAST PLAY | RESULT | score: %s", drive.getDrivePlays()
					.get(drive.getDrivePlays().size() - 1).getPlayResult().getPlayResultHomeScore());
			LoggingUtils.logInfo(actualValueLogStr);
			LoggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveResultHomeScore() != drive.getDrivePlays().get(drive.getDrivePlays().size()-1).getPlayResult().getPlayResultHomeScore()");
		}
		if (!drive.getDriveResultAwayScore().equals(
				drive.getDrivePlays().get(drive.getDrivePlays().size() - 1).getPlayResult().getPlayResultAwayScore())) {

			String actualValueLogStr = String.format("AWAY | DRIVE | RESULT | score: %s",
					drive.getDriveResultAwayScore());
			String expectedValueLogStr = String.format("AWAY | LAST PLAY | RESULT | score: %s", drive.getDrivePlays()
					.get(drive.getDrivePlays().size() - 1).getPlayResult().getPlayResultAwayScore());
			LoggingUtils.logInfo(actualValueLogStr);
			LoggingUtils.logInfo(expectedValueLogStr);

			throw new IllegalArgumentException(
					"drive.getDriveResultAwayScore() != drive.getDrivePlays().get(drive.getDrivePlays().size()-1).getPlayResult().getPlayResultAwayScore()");
		}
	}

	private static void validateDriveNullCheckHelper(DrivePojo drive) {
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

	private static void validateDriveNullScoreHelper(DrivePojo drive) {
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

	private static void validateKickoff(DrivePojo drive) {
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
