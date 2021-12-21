package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;

public class NcaaConstants {
	public static final String CONTEXT_DEBUG_KEY = "F_DEBUG";
	public static final String CONTEXT_DEBUG_VALUE_TRUE = "TRUE";
	public static final String CONTEXT_DEBUG_VALUE_FALSE = "FALSE";

	public static final String CONTEXT_STACK_KEY = "F_STACK";
	public static final String CONTEXT_STACK_VALUE_TRUE = "TRUE";
	public static final String CONTEXT_STACK_VALUE_FALSE = "FALSE";
	
	public static final Integer FUZZY_THRESHOLD = 3;
	public static final String ERROR_S_FAILED_WITH_S = "ERROR: [%s] failed with %s";

	public static final Map<String, List<String>> TEAM_ID_YARD_ABBREV_DICT = loadJson();

	@SuppressWarnings("unchecked")
	private static Map<String, List<String>> loadJson() {
		ObjectMapper mapper = new ObjectMapper();
		try {
			return mapper.readValue(new File("/home/ehens86/git/ncaa-fb-sb-odds-parse-api/src/main/resources/teamIdAcroynmMap.json"), Map.class);
		} catch (IOException e) {
			return new HashMap<>();
		}
	}

	public static final String PLAYER_NAME_REGEX = "(((Van )?[A-Z][aA-zZ']+ ?[A-Z]{0,2}\\.?,.?[aA-zZ][aA-zZ\\-'\\.]*)|([A-Z]([A-Z]|[a-z]+) [A-Z][a-z]+)|([aA-zZ]+\\.? ?[aA-zZ\\-\\']+)|([A-Z][a-z]+\\. [A-Z][a-z]+))";
	public static final String TEAM_YARD_REGEX = " ([A-Z]*?-?[aA-zZ]{2,3}\\s?\\d{1,2}|50 yardline)";
}
