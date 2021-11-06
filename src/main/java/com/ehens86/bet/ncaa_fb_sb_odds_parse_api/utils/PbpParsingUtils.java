package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayTeamPojo;

@Service
public class PbpParsingUtils {
	private static final Logger LOG = Logger.getLogger(PbpParsingUtils.class.toString());

	public Integer convertMinSecToSec(String inputStr) {
		try {
			String[] timeSplit = inputStr.split(":");
			String minute = timeSplit[0];
			String second = timeSplit[1];
			Integer periodSeconds = (Integer.valueOf(minute) * 60) + Integer.valueOf(second);
			return periodSeconds;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStr);
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] extractNames(String inputStr) {
		try {
//		String regex = "([aA-zZ]+, [aA-zZ][a-z]+)";
			// "([A-Z][aA-zZ']+ ?[A-Z]{0,2}\\.?,.?[aA-zZ][aA-zZ']+)";
			// String regex = "(((Van )?[A-Z][aA-zZ']+
			// ?[A-Z]{0,2}\\.?,.?[aA-zZ][aA-zZ'\\.]+)|([A-Z]([A-Z]|[a-z]+) [A-Z][a-z]+))";

			Pattern pattern = Pattern.compile(NcaaConstants.playerNameRegex);
			Matcher matcher = pattern.matcher(inputStr);

			StringBuilder result = new StringBuilder();
			while (matcher.find()) {
				result.append(matcher.group(1));
				result.append("|");
			}

			// System.out.println(result.toString());
			return result.toString().split("\\|");
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStr);
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] extractYard(String inputStr) {
		try {
//		String regex = "([aA-zZ]+, [aA-zZ][a-z]+)";
			// String regex = " [A-Z]*?-?([aA-zZ]{2,3}\\s?\\d{1,2})";
			Pattern pattern = Pattern.compile(NcaaConstants.teamYardRegex);
			Matcher matcher = pattern.matcher(inputStr);

			StringBuilder result = new StringBuilder();
			while (matcher.find()) {
				result.append(matcher.group(1));
				result.append("|");
			}

			// System.out.println(result.toString());
			// String outputString = result.toString();
			String[] splitString = result.toString().split("\\|");

			return splitString;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStr);
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] extractTackle(String inputStr) {
		try {
//		String regex = "([aA-zZ]+, [aA-zZ][a-z]+)";
			if (inputStr.toUpperCase().contains("BLOCKED BY")) {
				return null;
			}
			String tackleRegex = "\\((.+)\\)"; // (\\([aA-zZ].+\\))
			Pattern tacklePattern = Pattern.compile(tackleRegex);
			Matcher tackleMatcher = tacklePattern.matcher(inputStr.replace("(Scoring play confirmed)", "").replace("forced by ", "").replace("(After review, play stands as called on the field)", ""));

			boolean match = tackleMatcher.find();
			Integer tackleCount = tackleMatcher.groupCount();

			if (!match) {
				@SuppressWarnings("unused")
				String warningStr = String.format("WARNING: no tackles found for string - %s", inputStr);
				// LOG.log(Level.WARNING, warningStr);
				return null; // new String[] {""};
			} else if (tackleCount > 1) {
				throw new IllegalArgumentException(
						String.format("%s tackling blocks found.  Value greater than 1.", tackleCount));
			} else {
				String rawTackles = tackleMatcher.group(1);
				String[] rawTackleNames = rawTackles.split(" ?; ?");
				StringBuilder rawResult = new StringBuilder();

				for (String rawTackleName : rawTackleNames) {
					if (!rawTackleName.isBlank() && !rawTackleName.toUpperCase().contains("BLOCKED BY") && !rawTackleName.toUpperCase().contains("PLAY STANDS")) {
						rawResult.append(formatName(rawTackleName.replace(")", "")));
						rawResult.append("|");
					}
				}
				String rawResultString = rawResult.toString();
				if (!rawResultString.contains("|")) {
					System.out.println("catch");
				}
				String[] result = rawResultString.split("\\|");
				return result;
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStr);
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
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
//			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
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

			// System.out.println(result.toString());
			if (StringUtils.isBlank(result.toString())) {
				throw new IllegalArgumentException("Extraction failed, no matches found");
			}

			return result.toString();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s.  Regex = %s",
					ste[1].getMethodName(), e.toString(), inputStr, regex);
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public String[] convertDownAndDistance(String inputStr) {
		try {
			String regex = String.format("(\\d[a-z]{2}) and (\\d+) at ?%s", NcaaConstants.teamYardRegex);
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
			// System.out.println(result.toString());
			return resultRawSplit;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStr);
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
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
//				return inputStr.toUpperCase();
			} else {
				splitNameRaw = inputStr.split("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])");
				if (splitNameRaw.length == 2) {
					name = splitNameRaw[0].toUpperCase() + " " + splitNameRaw[1].toUpperCase();
				} else {
					name = inputStr.toUpperCase();
					if (!"TEAM".equals(name)) {
						LOG.log(Level.WARNING, String.format("SINGLE NAME: %s", name));
						throw new IllegalArgumentException("Invalid single name");
					}
				}
			}

			return name;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStrRaw);
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public boolean resolvePossesionTeam(String abbrev, String possTeam, String defTeam,
			Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {
			if (NcaaConstants.teamIdYardAbbrevDict.containsKey(possTeam)
					&& NcaaConstants.teamIdYardAbbrevDict.get(possTeam).contains(abbrev.toUpperCase())
					&& NcaaConstants.teamIdYardAbbrevDict.containsKey(defTeam)
					&& NcaaConstants.teamIdYardAbbrevDict.get(defTeam).contains(abbrev.toUpperCase())) {
				throw new IllegalArgumentException(String.format("Double match found for %s", abbrev));
			} else if (NcaaConstants.teamIdYardAbbrevDict.containsKey(possTeam)
					&& NcaaConstants.teamIdYardAbbrevDict.get(possTeam).contains(abbrev.toUpperCase())) {
				return true;
			} else if (NcaaConstants.teamIdYardAbbrevDict.containsKey(defTeam)
					&& NcaaConstants.teamIdYardAbbrevDict.get(defTeam).contains(abbrev.toUpperCase())) {
				return false;
			} else {
				throw new IllegalArgumentException(String.format("No match found for %s", abbrev));
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), abbrev);
//			LOG.log(Level.SEVERE, errorStr);
			System.out.println(abbrev);
			// System.out.println(possTeam);
			System.out.println(teamAbbrevDict.get(possTeam));
			// System.out.println(defTeam);
			System.out.println(teamAbbrevDict.get(defTeam));
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public Integer formatYardLine(String inputStr, String possTeam, String defTeam,
			Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {
			// getSixCharAbbr().substring(0, 2)
			if ("50 yardline".equals(inputStr.strip()) || "the 50".equals(inputStr.strip()) || "50 YARDLINE".equals(inputStr.strip())) {
				return 50;
			} else {

				Integer yardLine;
				String rawYard = extractCustom(inputStr, "([A-Z]*?-?[a-zA-Z]{2,3})\\s?(\\d{1,2})", 2);
				// System.out.println(rawYard);
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
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStr);
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
}
