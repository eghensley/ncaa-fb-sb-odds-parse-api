package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayTeamPojo;

@Service
public class PbpParsingUtils {
	private static final String ERROR_S_FAILED_WITH_S_INPUT_S = "ERROR: [%s] failed with %s.  Input = %s";
	private static final Logger LOG = Logger.getLogger(PbpParsingUtils.class.toString());

	public Integer convertMinSecToSec(String inputStr, PlayPeriodEnum playPeriod) {
		try {
			String[] timeSplit = inputStr.split(":");
			String minute = timeSplit[0];
			String second = timeSplit[1];
			Integer periodSeconds = (Integer.valueOf(minute) * 60) + Integer.valueOf(second);
			if (PlayPeriodEnum.FIRST == playPeriod || PlayPeriodEnum.THIRD == playPeriod) {
				periodSeconds += 900;
			} else if (PlayPeriodEnum.OT == playPeriod) {
				throw new IllegalArgumentException("Catch OT time");
			}
			return periodSeconds;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					inputStr);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] extractNames(String inputStr) {
		try {
			Pattern pattern = Pattern.compile(NcaaConstants.PLAYER_NAME_REGEX);
			Matcher matcher = pattern.matcher(inputStr);

			StringBuilder result = new StringBuilder();
			while (matcher.find()) {
				result.append(matcher.group(1));
				result.append("|");
			}
			return result.toString().split("\\|");
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					inputStr);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] extractYard(String inputStr) {
		try {
			Pattern pattern = Pattern.compile(NcaaConstants.TEAM_YARD_REGEX);
			Matcher matcher = pattern.matcher(inputStr);

			StringBuilder result = new StringBuilder();
			while (matcher.find()) {
				result.append(matcher.group(1));
				result.append("|");
			}
			return result.toString().split("\\|");
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					inputStr);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] extractTackle(String inputStr) {
		try {
			if (inputStr.toUpperCase().contains("BLOCKED BY")) {
				return new String[0];
			}
			String tackleRegex = "\\((.+)\\)"; // (\\([aA-zZ].+\\))
			Pattern tacklePattern = Pattern.compile(tackleRegex);
			Matcher tackleMatcher = tacklePattern.matcher(inputStr.replace("(Scoring play confirmed)", "")
					.replace("forced by ", "").replace("(After review, play stands as called on the field)", ""));

			boolean match = tackleMatcher.find();
			Integer tackleCount = tackleMatcher.groupCount();

			if (!match) {
				return new String[0];
			} else if (tackleCount > 1) {
				throw new IllegalArgumentException(
						String.format("%s tackling blocks found.  Value greater than 1.", tackleCount));
			} else {
				String rawTackles = tackleMatcher.group(1);
				String[] rawTackleNames = rawTackles.split(" ?; ?");
				StringBuilder rawResult = new StringBuilder();

				for (String rawTackleName : rawTackleNames) {
					if (!rawTackleName.isBlank() && !rawTackleName.toUpperCase().contains("BLOCKED BY")
							&& !rawTackleName.toUpperCase().contains("PLAY STANDS")) {
						rawResult.append(formatName(rawTackleName.replace(")", "")));
						rawResult.append("|");
					}
				}
				String rawResultString = rawResult.toString();
				return rawResultString.split("\\|");
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					inputStr);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public boolean evalMatch(String inputStr, String regex) {
		try {
			boolean match = false;
			Pattern pattern = Pattern.compile(regex);
			Matcher matcher = pattern.matcher(inputStr);

			while (matcher.find()) {
				match = true;
			}

			return match;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s.  Regex = %s",
					ste[1].getMethodName(), e.toString(), inputStr, regex);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String extractCustom(String inputStr, String regex, Integer groups) {
		try {
			Pattern pattern = Pattern.compile(regex);
			Matcher matcher = pattern.matcher(inputStr);

			StringBuilder result = new StringBuilder();
			while (matcher.find()) {
				for (int i = 0; i < groups; i++) {
					result.append(matcher.group(i + 1));
					result.append("~");
				}
				result.append("|");

			}

			if (StringUtils.isBlank(result.toString())) {
				throw new IllegalArgumentException("Extraction failed, no matches found");
			}

			return result.toString();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s.  Regex = %s",
					ste[1].getMethodName(), e.toString(), inputStr, regex);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] convertDownAndDistance(String inputStr) {
		try {
			String regex = String.format("(\\d[a-z]{2}) and (\\d+) at ?%s", NcaaConstants.TEAM_YARD_REGEX);
			Integer groups = 3;
			Pattern pattern = Pattern.compile(regex);
			Matcher matcher = pattern.matcher(inputStr);

			StringBuilder result = new StringBuilder();
			while (matcher.find()) {
				for (int i = 0; i < groups; i++) {
					result.append(matcher.group(i + 1));
					result.append("~");
				}
			}
			String resultRaw = result.toString().replace("|", "");
			String[] resultRawSplit = resultRaw.split("\\~");
			String down;
			String downRaw = resultRawSplit[0];
			if ("1st".equals(downRaw)) {
				down = "FIRST";
			} else if ("2nd".equals(downRaw)) {
				down = "SECOND";
			} else if ("3rd".equals(downRaw)) {
				down = "THIRD";
			} else if ("4th".equals(downRaw)) {
				down = "FOURTH";
			} else {
				throw new IllegalArgumentException(String.format("%s cannot be parsed to down enum", downRaw));
			}
			resultRawSplit[0] = down;
			resultRawSplit[2] = resultRawSplit[2].toUpperCase();

			return resultRawSplit;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					inputStr);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String formatName(String inputStrRaw) {
		try {
			if ("null".equals(inputStrRaw)) {
				throw new IllegalArgumentException("Name == null");
			}
			String name;
			String[] splitNameRaw;
			String inputStr = inputStrRaw.replaceAll("\\W$", "");
			if (inputStr.contains(",")) {
				splitNameRaw = inputStr.split(",");
				name = splitNameRaw[1].toUpperCase().strip() + " " + splitNameRaw[0].toUpperCase().strip();
			} else if (inputStr.contains(".")) {
				splitNameRaw = inputStr.split("\\.");
				name = splitNameRaw[0].toUpperCase().strip() + " " + splitNameRaw[1].toUpperCase().strip();
			} else if (inputStr.contains(" ")) {
				splitNameRaw = inputStr.split(" ");
				name = splitNameRaw[0].toUpperCase().strip() + " " + splitNameRaw[1].toUpperCase().strip();
			} else if ("".equals(inputStr)) {
				throw new IllegalArgumentException("Empty String");
			} else {
				splitNameRaw = inputStr.split("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])");
				if (splitNameRaw.length == 2) {
					name = splitNameRaw[0].toUpperCase() + " " + splitNameRaw[1].toUpperCase();
				} else {
					name = inputStr.toUpperCase();
					if (!"TEAM".equals(name)) {
						String logStr = String.format("SINGLE NAME: %s", name);
						LOG.log(Level.WARNING, logStr);
						throw new IllegalArgumentException("Invalid single name");
					}
				}
			}

			return name;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					inputStrRaw);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public boolean resolvePossesionTeam(String abbrev, String possTeam, String defTeam,
			Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {
			if (NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.containsKey(possTeam)
					&& NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.get(possTeam).contains(abbrev.toUpperCase())
					&& NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.containsKey(defTeam)
					&& NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.get(defTeam).contains(abbrev.toUpperCase())) {
				throw new IllegalArgumentException(String.format("Double match found for %s", abbrev));
			} else if (NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.containsKey(possTeam)
					&& NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.get(possTeam).contains(abbrev.toUpperCase())) {
				return true;
			} else if (NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.containsKey(defTeam)
					&& NcaaConstants.TEAM_ID_YARD_ABBREV_DICT.get(defTeam).contains(abbrev.toUpperCase())) {
				return false;
			} else {
				throw new IllegalArgumentException(String.format("No match found for %s", abbrev));
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					abbrev);
			LOG.log(Level.INFO, String.format("Abbrev: %s", abbrev));
			LOG.log(Level.INFO, String.format("Possession Team: %s", teamAbbrevDict.get(possTeam)));
			LOG.log(Level.INFO, String.format("Defense Team: %s", teamAbbrevDict.get(defTeam)));
			throw new IllegalArgumentException(errorStr);
		}
	}

	public Integer formatYardLine(String inputStr, String possTeam, String defTeam,
			Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {
			if ("50 yardline".equals(inputStr.strip()) || "the 50".equals(inputStr.strip())
					|| "50 YARDLINE".equals(inputStr.strip())) {
				return 50;
			} else {

				Integer yardLine;
				String rawYard = extractCustom(inputStr, "([A-Z]*?-?[a-zA-Z]{2,3})\\s?(\\d{1,2})", 2);
				String[] splitYard = rawYard.split("\\|")[0].split("\\~");

				Integer baseYardLine = Integer.valueOf(splitYard[1]);
				String teamField = splitYard[0];

				if (resolvePossesionTeam(teamField, possTeam, defTeam, teamAbbrevDict)) {
					yardLine = baseYardLine;
				} else {
					yardLine = 100 - baseYardLine;
				}

				return yardLine;
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					inputStr);
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}
}
