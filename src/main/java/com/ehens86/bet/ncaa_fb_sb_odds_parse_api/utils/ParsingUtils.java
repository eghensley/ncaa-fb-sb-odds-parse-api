package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;

public final class ParsingUtils {

    // Private constructor to prevent instantiation
    private ParsingUtils() {
        throw new UnsupportedOperationException();
    }
    
	public static Integer parseString(String val) {
		try {
			if ("".equals(val)) {
				return 0;
			} else {
				return Integer.valueOf(val);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(NcaaConstants.ERROR_S_FAILED_WITH_S, ste[1].getMethodName(), e.toString());
			LoggingUtils.logInfo(errorStr);
			LoggingUtils.logException(e, errorStr);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public static Double parseStringToDouble(String val) {
		try {
			if ("".equals(val)) {
				return 0.0;
			} else {
				return Double.valueOf(val);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(NcaaConstants.ERROR_S_FAILED_WITH_S, ste[1].getMethodName(), e.toString());
			LoggingUtils.logInfo(errorStr);
			LoggingUtils.logException(e, errorStr);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public static Integer splitValue(String value, String delim, Integer index) {
		try {
			ArrayList<Integer> mults = new ArrayList<>(Arrays.asList(1, 1));

			Integer delimCount = StringUtils.countMatches(value, delim);
			if (Integer.valueOf("2").equals(delimCount)) {
				if ('-' == value.charAt(0)) {
					mults.set(0, -1);
				} else {
					mults.set(1, -1);
				}
			} else if (Integer.valueOf("0").equals(delimCount)) {
				String noDelimiterMessageStr = String.format(
						"No delimiters found in string provided.  Input value: %s | Delimiter value: %s", value, delim);
				LoggingUtils.logInfo(noDelimiterMessageStr);
			} else if (delimCount > 2) {
				String excessDelimiterMessageStr = String.format(
						"Multiple delimiters found in string provided.  Delimiters found: %s | Input value: %s | Delimiter value: %s",
						delimCount, value, delim);
				LoggingUtils.logInfo(excessDelimiterMessageStr);
			}

			Integer delimIndex = value.indexOf(delim);
			if (Integer.valueOf(value.length()).equals(delimIndex + 1)) {
				value = value + "0";
			}
			if (Integer.valueOf("0").equals(delimIndex)) {
				value = "0" + value;
			}

			String[] splitValue = value.split(delim);
			splitValue = Arrays.stream(splitValue).filter(x -> x.length() > 0).toArray(String[]::new);

			String indexedValue = splitValue[index];
			return Integer.valueOf(indexedValue) * mults.get(index);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(NcaaConstants.ERROR_S_FAILED_WITH_S, ste[1].getMethodName(), e.toString());
			LoggingUtils.logInfo(errorStr);
			LoggingUtils.logException(e, errorStr);
			throw new IllegalArgumentException(errorStr);
		}
	}
}
