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
			String regex = "(\\([aA-zZ].+\\))";
			Pattern pattern = Pattern.compile(regex);
			Matcher matcher = pattern.matcher(inputStr);

			StringBuilder result = new StringBuilder();
			while (matcher.find()) {
				result.append(matcher.group(1));
				result.append("|");
			}

			// System.out.println(result.toString());
			String[] resultRawSplit = result.toString().split("\\|");
			if (resultRawSplit.length > 1) {
				throw new IllegalArgumentException(
						String.format("%s tackling blocks found.  Value greater than 1.", resultRawSplit.length));
			} else if ("".equals(resultRawSplit[0])) {
				@SuppressWarnings("unused")
				String warningStr = String.format("WARNING: no tackles found for string - %s", inputStr);
				// LOG.log(Level.WARNING, warningStr);
				return resultRawSplit;
			}
			String[] resultCleanedSplit = resultRawSplit[0].substring(1, resultRawSplit[0].length() - 1).split("; ");
			return resultCleanedSplit;
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
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
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
			e.printStackTrace();
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

	public String formatName(String inputStr) {
		try {
			String name;
			String[] splitNameRaw;
			if (inputStr.contains(",")) {
				splitNameRaw = inputStr.split(",");
				name = splitNameRaw[1].toUpperCase() + " " + splitNameRaw[0].toUpperCase();
			} else if (inputStr.contains(".")) {
				splitNameRaw = inputStr.split("\\.");
				name = splitNameRaw[0].toUpperCase() + " " + splitNameRaw[1].toUpperCase();
			} else if (inputStr.contains(" ")){
				splitNameRaw = inputStr.split(" ");
				name = splitNameRaw[0].toUpperCase() + " " + splitNameRaw[1].toUpperCase();
			} else if ("".equals(inputStr)) {
				throw new IllegalArgumentException("Empty String");
//				return inputStr.toUpperCase();
			} else {
				name = inputStr.toUpperCase();
			}

			return name;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), inputStr);
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public boolean resolvePossesionTeam(String abbrev, String possTeam, String defTeam,
			Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {

			if ((NcaaConstants.teamIdYardAbbrevDict.containsKey(possTeam)
					&& NcaaConstants.teamIdYardAbbrevDict.get(possTeam).contains(abbrev))
					|| ((!NcaaConstants.teamIdYardAbbrevDict.containsKey(possTeam)
							&& teamAbbrevDict.get(possTeam).getSeoName().toUpperCase().contains(abbrev)))
					|| ((!NcaaConstants.teamIdYardAbbrevDict.containsKey(defTeam)
							&& teamAbbrevDict.get(possTeam).getSeoName().contains("-")
							&& abbrev.equals(String.format("%s%sU",
									teamAbbrevDict.get(possTeam).getSeoName().split("-")[0].substring(0, 1)
											.toUpperCase(),
									teamAbbrevDict.get(possTeam).getSeoName().split("-")[1].substring(0, 1)
											.toUpperCase()))))
					|| ((!NcaaConstants.teamIdYardAbbrevDict.containsKey(defTeam)
							&& !teamAbbrevDict.get(possTeam).getSeoName().contains("-")
							&& abbrev.equals(String.format("U%s",
									teamAbbrevDict.get(possTeam).getSeoName().split("-")[0].substring(0, 1)
											.toUpperCase())))
							|| (!NcaaConstants.teamIdYardAbbrevDict.containsKey(defTeam)
									&& !teamAbbrevDict.get(possTeam).getSixCharAbbr().contains(abbrev))
					)) {
				return true;
			} else if ((NcaaConstants.teamIdYardAbbrevDict.containsKey(defTeam)
					&& NcaaConstants.teamIdYardAbbrevDict.get(defTeam).contains(abbrev))
					|| ((!NcaaConstants.teamIdYardAbbrevDict.containsKey(defTeam)
							&& teamAbbrevDict.get(defTeam).getSeoName().toUpperCase().contains(abbrev)))
					|| ((!NcaaConstants.teamIdYardAbbrevDict.containsKey(possTeam)
							&& teamAbbrevDict.get(defTeam).getSeoName().contains("-")
							&& abbrev.equals(String.format("%s%sU",
									teamAbbrevDict.get(defTeam).getSeoName().split("-")[0].substring(0, 1)
											.toUpperCase(),
									teamAbbrevDict.get(defTeam).getSeoName().split("-")[1].substring(0, 1)
											.toUpperCase()))))
					|| ((!NcaaConstants.teamIdYardAbbrevDict.containsKey(possTeam)
							&& !teamAbbrevDict.get(defTeam).getSeoName().contains("-")
							&& abbrev.equals(String.format("U%s",
									teamAbbrevDict.get(defTeam).getSeoName().split("-")[0].substring(0, 1)
											.toUpperCase())))
							|| (!NcaaConstants.teamIdYardAbbrevDict.containsKey(possTeam)
									&& !teamAbbrevDict.get(defTeam).getSixCharAbbr().contains(abbrev))

					)) {
				return false;
			} else {
				throw new IllegalArgumentException(String.format("No match found for %s", abbrev));
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), abbrev);
			LOG.log(Level.SEVERE, errorStr);
			System.out.println(possTeam);
			System.out.println(defTeam);
			System.out.println(teamAbbrevDict);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	public Integer formatYardLine(String inputStr, String possTeam, String defTeam,
			Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		try {
			// getSixCharAbbr().substring(0, 2)
			if ("50 yardline".equals(inputStr) || "the 50".equals(inputStr) || "50 YARDLINE".equals(inputStr)) {
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
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
}
