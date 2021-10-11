package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants;

import static java.util.Map.entry;

import java.util.Map;

public class NcaaConstants {
	public static final Integer fuzzyThreshold = 3;

	public static final Map<String, String> teamIdYardAbbrevDict = Map.ofEntries(entry("2609", "UND"),
			entry("2024", "USU"), entry("774", "KSU"), entry("2614", "NU"), entry("2472", "ISU"), entry("51305", "KSU"),
			entry("51", "GT"), entry("1274", "NSU"), entry("2358", "WFU"), entry("802", "MSU"), entry("861", "YSU"),
			entry("1174", "ASU"), entry("2661", "SBU"), entry("56197", "GA")

	);
}
