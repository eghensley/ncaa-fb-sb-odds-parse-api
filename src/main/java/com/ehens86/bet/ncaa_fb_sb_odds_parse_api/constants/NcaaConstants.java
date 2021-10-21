package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants;

import static java.util.Map.entry;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;

public class NcaaConstants {
	public static final Integer fuzzyThreshold = 3;
//	static Gson gson = new Gson();
//	static File file = new File(
//	        this.getClass().getClassLoader().getResource("teamIdAcroynm.json").getFile()
//	    );
//	
//	@Value("classpath:resources/teamIdAcroynm.json")
//	static
//	Resource resourceFile;
	
	
	public static final Map<String, List<String>> teamIdYardAbbrevDict = loadJson();

	//throws JsonParseException, JsonMappingException, IOException
	@SuppressWarnings("unchecked")
	private static Map<String, List<String>> loadJson() {
		ObjectMapper mapper = new ObjectMapper();
		try {
			return mapper.readValue(new File("/home/ehens86/git/ncaa-fb-sb-odds-parse-api/src/main/resources/teamIdAcroynmMap.json"), Map.class);
		} catch (IOException e) {
			return new HashMap<String, List<String>>();
		}
	}
//	public static final Map<String, List<String>> teamIdYardAbbrevDict = mapper.readValue(resourceFile, Map.class);

//	public static final Map<String, List<String>> teamIdYardAbbrevDict = gson.fromJson(file, Map.class);
//	public static final Map<String, List<String>> teamIdYardAbbrevDict = Map.ofEntries(
//			entry("2609", Arrays.asList("UND")), entry("2024", Arrays.asList("USU")),
//			entry("774", Arrays.asList("KSU")), entry("2614", Arrays.asList("NU")), entry("2472", Arrays.asList("ISU")),
//			entry("51305", Arrays.asList("KSU")), entry("51", Arrays.asList("GT")), entry("1274", Arrays.asList("NSU")),
//			entry("2358", Arrays.asList("WFU")), entry("802", Arrays.asList("MSU")), entry("861", Arrays.asList("YSU")),
//			entry("1174", Arrays.asList("ASU")), entry("2661", Arrays.asList("SBU")),
//			entry("56197", Arrays.asList("POINT-GA", "POINT")), entry("2283", Arrays.asList("NDS")),
//			entry("912", Arrays.asList("NCC")), entry("1482", Arrays.asList("FIU")),
//			entry("1386928", Arrays.asList("LIU")), entry("15", Arrays.asList("CCU")),
//			entry("1558", Arrays.asList("BSU")), entry("1376", Arrays.asList("WIU")),
//			entry("624", Arrays.asList("TTU")), entry("258", Arrays.asList("SAM")), entry("2677", Arrays.asList("UTA")),
//			entry("983", Arrays.asList("WSU")), entry("1064", Arrays.asList("UCD")),
//			entry("2269", Arrays.asList("MUR", "MSU")), entry("808", Arrays.asList("MVS")),
//			entry("1681", Arrays.asList("SHS")), entry("1959", Arrays.asList("NAU")),
//			entry("1475", Arrays.asList("EWU")), entry("1627", Arrays.asList("NLV")),
//			entry("274", Arrays.asList("SDU", "USD")), entry("1823", Arrays.asList("KAN")),
//			entry("1815", Arrays.asList("ISU")), entry("1859", Arrays.asList("ALB")),
//			entry("2461", Arrays.asList("GAS")), entry("2569", Arrays.asList("GWU")),
//			entry("444", Arrays.asList("MTS")), entry("2267", Arrays.asList("MON")), entry("977", Arrays.asList("UVA")),
//			entry("1195", Arrays.asList("WM")), entry("1703", Arrays.asList("TCU")),
//			entry("1465", Arrays.asList("DUQ")), entry("644", Arrays.asList("UW")), entry("1616", Arrays.asList("UM")),
//			entry("40", Arrays.asList("DUK")), entry("559", Arrays.asList("AT", "CAT", "NCAT")), entry("2103", Arrays.asList("CMU")),
//			entry("1309", Arrays.asList("RMU")), entry("1776", Arrays.asList("CIN")),
//			entry("56189", Arrays.asList("MER")), entry("1175", Arrays.asList("ALA")),
//			entry("1081", Arrays.asList("CLE")), entry("2002", Arrays.asList("SCS")),
//			entry("490", Arrays.asList("WMU")), entry("412", Arrays.asList("ISU")), entry("1830", Arrays.asList("LAM")),
//			entry("2737", Arrays.asList("UTS")), entry("1518", Arrays.asList("LAF")),
//			entry("2227", Arrays.asList("BUC")), entry("974", Arrays.asList("VIL")), entry("437", Arrays.asList("MAR")),
//			entry("1809", Arrays.asList("HAM")), entry("2291", Arrays.asList("ODU")),
//			entry("1694", Arrays.asList("SIU")), entry("419", Arrays.asList("KSU")), entry("574", Arrays.asList("OKL")),
//			entry("1374", Arrays.asList("WCU")), entry("1502", Arrays.asList("IDA")),
//			entry("759", Arrays.asList("IND")), entry("279", Arrays.asList("MSU")), entry("2241", Arrays.asList("UCA")),
//			entry("2457", Arrays.asList("FSU")), entry("764", Arrays.asList("JSU")), entry("906", Arrays.asList("NEV")),
//			entry("1344", Arrays.asList("TEN")), entry("365", Arrays.asList("COL")),
//			entry("1148", Arrays.asList("MAR")), entry("1557", Arrays.asList("APS")),
//			entry("1945", Arrays.asList("MOR","MSU")), entry("1160", Arrays.asList("MIA")),
//			entry("1963", Arrays.asList("OSU")), entry("2308", Arrays.asList("RIC")),
//			entry("2312", Arrays.asList("RUT")), entry("2120", Arrays.asList("DEL")),
//			entry("1059", Arrays.asList("SSU", "SAC")), entry("351", Arrays.asList("CAL")),
//			entry("1188", Arrays.asList("BGS")), entry("1413", Arrays.asList("UIW")),
//			entry("621", Arrays.asList("TXS")), entry("913", Arrays.asList("NCS")),
//			entry("1108", Arrays.asList("FUR")), entry("540", Arrays.asList("TSU")),
//			entry("958", Arrays.asList("SUU")), entry("640", Arrays.asList("VT")),
//			entry("1501", Arrays.asList("HOL")),  entry("1904", Arrays.asList("ELO")),
//			entry("1645", Arrays.asList("NIU")),  entry("1835", Arrays.asList("MAI")),
//			entry("247", Arrays.asList("RIC")),  entry("2672", Arrays.asList("TSU")),
//			entry("295", Arrays.asList("AF")), entry("920", Arrays.asList("UNI")),
//			entry("1683", Arrays.asList("SJS")), entry("978", Arrays.asList("WAG")),
//			entry("1353", Arrays.asList("TLS")), entry("626", Arrays.asList("UTM")),
//			entry("1377", Arrays.asList("WKU")), entry("205", Arrays.asList("UNM")),
//			entry("56199", Arrays.asList("HBU")), entry("457", Arrays.asList("ASU")),
//			entry("1958", Arrays.asList("UNC")), entry("2110", Arrays.asList("COL")),
//			entry("2537", Arrays.asList("BC")), entry("1092", Arrays.asList("DAV")),
//			entry("639", Arrays.asList("VMI")), entry("1415", Arrays.asList("WOF")),
//			entry("2476", Arrays.asList("ISU")), entry("2643", Arrays.asList("USD")),
//			entry("698", Arrays.asList("CP")), entry("1981", Arrays.asList("URI")),
//			entry("132", Arrays.asList("BRY")), entry("558", Arrays.asList("UNA")),
//			entry("276", Arrays.asList("SLU")), entry("72", Arrays.asList("JMU")),
//			entry("1524", Arrays.asList("LIB")), entry("354", Arrays.asList("CAM")),
//			entry("812", Arrays.asList("ACU"))
//			
//			)
//
//	;

	public static final String playerNameRegex = "(((Van )?[A-Z][aA-zZ']+ ?[A-Z]{0,2}\\.?,.?[aA-zZ][aA-zZ\\-'\\.]*)|([A-Z]([A-Z]|[a-z]+) [A-Z][a-z]+)|([aA-zZ]+\\.? ?[aA-zZ\\-\\']+)|([A-Z][a-z]+\\. [A-Z][a-z]+))";
	public static final String teamYardRegex = " ([A-Z]*?-?[aA-zZ]{2,3}\\s?\\d{1,2}|50 yardline)";
}
