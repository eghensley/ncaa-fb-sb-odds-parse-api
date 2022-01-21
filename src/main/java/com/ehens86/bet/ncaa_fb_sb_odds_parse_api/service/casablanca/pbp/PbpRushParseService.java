package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;
import java.util.Objects;

import org.apache.logging.log4j.ThreadContext;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

public final class PbpRushParseService {

	// private static static constructor to prevent instantiation
    private PbpRushParseService() {
        throw new UnsupportedOperationException();
    }

	public static boolean parseRush(PbpServiceRequestPojo params, boolean updated) {
		try {
			if (params.getPlayRawText().contains(" rush ") || params.getPlayRawText().contains("rush for")
					|| params.getPlayRawText().toUpperCase().contains("KNEEL")
					|| params.getPlayRawText().toUpperCase().contains("RUSHING ATTEMPT")
					|| params.getPlayRawText().toUpperCase().contains("SCRAMBLES")) {
				cleanUpRushingString(params);

				parseRushHelper(params);

				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					String offTeam = params.getPossessionTeam();
					String defTeam = params.getDefenseTeam();
					PbpPlayerStatRushingPojo rushingInfo = params.getPlay().getPlayerStat().get(offTeam).getOffense()
							.getRushingStat().get(0);
					List<PbpPlayerStatDefenseProductionPojo> defenseInfo = params.getPlay().getPlayerStat().get(defTeam)
							.getDefense().getDefenseProduction();

					LoggingUtils.logInfo("- [parseRush] Results -");
					LoggingUtils.logInfo(
							String.format("Offense Team: %s", params.getTeamAbbrevDict().get(offTeam).getShortname()));
					LoggingUtils.logInfo(String.format("Rush Info: %s", rushingInfo.toString()));

					LoggingUtils.logInfo(
							String.format("Defense Team: %s", params.getTeamAbbrevDict().get(defTeam).getShortname()));
					if (!defenseInfo.isEmpty()) {
						for (PbpPlayerStatDefenseProductionPojo def : defenseInfo) {
							LoggingUtils.logInfo(String.format("Defense Info: %s", def.toString()));
						}
					} else {
						LoggingUtils.logInfo("Defense: None");
					}
				}

				updated = true;
			}
			return updated;
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void parseRushHelper(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().startsWith("KNEEL DOWN ")) {
				parseKneel(params);
			} else if (params.getPlayRawText().toUpperCase().contains("RUSHING ATTEMPT")) {
				parsePatRush(params);
			} else {
				parseRushingBasic(params);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, e.getLocalizedMessage());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private static void cleanUpRushingString(PbpServiceRequestPojo params) {
		try {
			params.setPlayRawText(params.getPlayRawText().replace("no gain", "0 yards"));

			if (params.getPlayRawText().startsWith("rush")) {
				String replacementPlayerName = String.format("TEAM, %s", params.getTeamAbbrevDict()
						.get(params.getPossessionTeam()).getSeoName().toUpperCase().replace(" ", "").replace("-", ""));
				String replacementPlayText = String.format("%s %s", replacementPlayerName, params.getPlayRawText());
				replacementPlayText = replacementPlayText.replaceAll("((fumbled by)( at))",
						String.format("$2 %s$3", replacementPlayerName));

				params.setPlayRawText(replacementPlayText);
				params.getPlay().setPlayText(params.getPlayRawText());
			}

			params.setPlayRawText(params.getPlayRawText().replace(". St. rush", " rush"));
			params.setPlayRawText(params.getPlayRawText().replace(", rush for ", " rush for "));
			params.setPlayRawText(params.getPlayRawText().replaceAll(
					String.format("(%s rush .+ \\d{1,3} yards, )()(fumbled at )", NcaaConstants.PLAYER_NAME_REGEX),
					"$1$2 $10"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"TEAM, LONGISLAND rush for 7 yards loss to the LIU40 fumbled by TEAM, LONGISLAND at forced by Brown,Mike recovered by LIU Wells,Davon at LIU40 (RuckerFurlow,Jadon)",
					"TEAM, LONGISLAND rush for 7 yards loss to the LIU40 fumbled by TEAM, LONGISLAND at LIU40 forced by Brown,Mike recovered by LIU Wells,Davon at LIU40 (RuckerFurlow,Jadon)"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"DEL fumbled snap, DEL rush up the middle for a loss of 1 yard to the DEL42, End of Play",
					"TEAM, Delaware fumbled snap, TEAM, Delaware rush up the middle for a loss of 1 yard to the DEL42, End of Play"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"DEL fumbled snap, DEL rush up the middle for 0 yards to the MAI20, End of Play",
					"TEAM, Delaware fumbled snap, TEAM, Delaware rush up the middle for 0 yards to the MAI20, End of Play"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"KSU fumbled snap, KSU rush up the middle for a loss of 33 yards, KSU fumbled at KSU35 , recovered by KSU B. Moran at the KSU17, returned -9 yards to the KSU8 (J. Perry); KSU turnover on downs",
					"TEAM, Kennesaw fumbled snap, TEAM, Kennesaw rush up the middle for a loss of 33 yards, TEAM, KennesawSt fumbled at KSU35 , recovered by KSU B. Moran at the KSU17, returned -9 yards to the KSU8 (J. Perry); KSU turnover on downs"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"UNM rush up the middle for a loss of 5 yards, UNM fumbled at HBU32 , recovered by UNM T. Wilson at the HBU32, returned 0 yards to the HBU32, End of Play",
					"TEAM, NewMexico rush up the middle for a loss of 5 yards, TEAM, NewMexico fumbled at HBU32 , recovered by UNM T. Wilson at the HBU32, returned 0 yards to the HBU32, End of Play"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"SAU rush up the middle for a gain of 2 yards to the PRE45 (J. Nagy)",
					"TEAM, StAndrews rush up the middle for a gain of 2 yards to the PRE45 (J. Nagy)"));
			params.setPlayRawText(params.getPlayRawText().replace(
					"SMU rush up the middle for a loss of 11 yards to the SMU47, End of Play",
					"TEAM, SMU rush up the middle for a loss of 11 yards to the SMU47, End of Play"));

			params.setPlayRawText(params.getPlayRawText().replaceAll("^([a-z|A-Z]+) rush",
					String.format("TEAM,%s rush", params.getTeamAbbrevDict().get(params.getPossessionTeam())
							.getSeoName().replace(" ", "").replace("-", ""))));
			params.setPlayRawText(params.getPlayRawText().replaceAll("Kneel down by ([a-z|A-Z]+) at",
					String.format("Kneel down by TEAM,%s at", params.getTeamAbbrevDict().get(params.getPossessionTeam())
							.getSeoName().replace(" ", "").replace("-", ""))));
			params.setPlayRawText(params.getPlayRawText().replace("Kneel down at",
					String.format("Kneel down by TEAM,%s at", params.getTeamAbbrevDict().get(params.getPossessionTeam())
							.getSeoName().replace(" ", "").replace("-", ""))));

			params.setPlayRawText(params.getPlayRawText().replace("TEAM rush for",
					String.format("TEAM, %s rush for", params.getTeamAbbrevDict().get(params.getPossessionTeam())
							.getSeoName().replace(" ", "").replace("-", ""))));

			params.setPlayRawText(params.getPlayRawText().replace(
					"TEAM,tennesseetech rush for loss of 11 yards to the TTU15, fumble by TEAM recovered by TTU MILLER, Willie at TTU3",
					"TEAM,tennesseetech rush for loss of 11 yards to the TTU15, fumble by TEAM, tennesseetech recovered by TTU MILLER, Willie at TTU3"));

			if (params.getPlay().getDriveText().equals("3rd and 0 at -")) {
				params.getPlay().setDriveText("3rd and 0 at ALCORN0");
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parsePatRush(PbpServiceRequestPojo params) {
		try {
			String rushString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format(
							"%s rushing attempt ((?:is good)|(?:failed))(; conversion is no good \\[down at the%s\\])?",
							NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX),
					10);
			String[] rushStringArray = rushString.split("\\|")[0].split("\\~");

			PbpPlayerStatRushingPojo rushingStat = new PbpPlayerStatRushingPojo(
					PbpParsingUtils.formatName(rushStringArray[0]));
			rushingStat.setRushingDirection(PlayDirectionEnum.MISSING);

			if ("is good".equals(rushStringArray[7])) {
				rushingStat.setRushingYard(100 - params.getPlay().getPlayStartYard());
				params.getPlay().getPlayResult().setPlayResultYard(rushingStat.getRushingYard());
				rushingStat.setRushingTwoPointConversion(1);
				params.getPlay().getPlayResult().setPlayResultPoints(2);
			} else if ("failed".equals(rushStringArray[7])) {
				Integer yards = PbpParsingUtils.formatYardLine(rushStringArray[9], params.getPossessionTeam(),
						params.getDefenseTeam(), params.getTeamAbbrevDict()) - params.getPlay().getPlayStartYard();
				rushingStat.setRushingYard(yards);
				params.getPlay().getPlayResult().setPlayResultYard(yards);
				rushingStat.setRushingTwoPointConversion(0);
			} else {
				throw new IllegalArgumentException("HANDLE");
			}

			rushingStat.setRushingKneel(0);

			params.getPlay().setPlayYardToGain(100 - params.getPlay().getPlayStartYard());
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
					.add(rushingStat);
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parseKneel(PbpServiceRequestPojo params) {
		try {
			String rushString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("Kneel down by %s at%s \\(?[aA-zZ]+ loss of (\\d{1,2})",
							NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX),
					9);
			String[] rushStringArray = rushString.split("\\|")[0].split("\\~");

			PbpPlayerStatRushingPojo rushingStat = new PbpPlayerStatRushingPojo(
					PbpParsingUtils.formatName(rushStringArray[0]));
			rushingStat.setRushingDirection(PlayDirectionEnum.MISSING);

			Integer yardGain = Integer.valueOf(rushStringArray[8]) * -1;
			rushingStat.setRushingYard(yardGain);
			rushingStat.setRushingKneel(1);
			rushingStat.setRushingTwoPointConversion(0);
			rushingStat.setRushingLineYard(0.0);
			rushingStat.setRushingStuff(false);
			rushingStat.setRushingOpenField(false);
			rushingStat.setRushingPower(false);
			rushingStat.setRushingSecondLevel(false);
			params.setPlayTackles(null);
			params.getPlay().getPlayResult().setPlayResultYard(yardGain);
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
					.add(rushingStat);
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parseRushingBasic(PbpServiceRequestPojo params) {
		try {
			Integer yardGain;
			boolean loss = false;
			PbpPlayerStatRushingPojo rushingStat;
			String[] rushStringArray;
			String rushString = PbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"%s (?:(?:rush)|(?:scrambles)) ?((?:up (?:the )?middle)|(?:to the (?:(?:right)|(?:left)))|(?:over (?:(?:left)|(?:right)) end))? for( a? ?(loss|gain) of)? (\\d{1,3}) yards? ?(loss|gain|)(.?)",
					NcaaConstants.PLAYER_NAME_REGEX), 13);
			rushStringArray = rushString.split("\\|")[0].split("\\~");

			rushingStat = new PbpPlayerStatRushingPojo(PbpParsingUtils.formatName(rushStringArray[0]));
			if (!"null".equals(rushStringArray[7])) {
				if (rushStringArray[7].endsWith(" end")) {
					rushingStat.setRushingDirection(PlayDirectionEnum
							.valueOf(String.format("%s_%s", rushStringArray[7].split(" ")[1].toUpperCase(),
									rushStringArray[7].split(" ")[2].toUpperCase())));
				} else if (rushStringArray[7].equals("up middle")) {
					rushingStat.setRushingDirection(
							PlayDirectionEnum.valueOf(rushStringArray[7].split(" ")[1].toUpperCase()));
				} else {
					rushingStat.setRushingDirection(
							PlayDirectionEnum.valueOf(rushStringArray[7].split(" ")[2].toUpperCase()));
				}
			} else {
				rushingStat.setRushingDirection(PlayDirectionEnum.MISSING);
			}

			yardGain = Integer.valueOf(rushStringArray[10]);
			if (("loss".equals(rushStringArray[11]) || "loss".equals(rushStringArray[9]))) {
				loss = true;
				yardGain *= -1;
			}
			if ((loss && !params.getPlayRawText().toUpperCase().contains("LOSS"))
					|| (!loss && params.getPlayRawText().toUpperCase().contains("LOSS"))) {
				throw new IllegalArgumentException("Loss does not reconcile");
			}
			rushingStat.setRushingYard(yardGain);
			rushingStat.setRushingKneel(0);
			rushingStat.setRushingTwoPointConversion(0);
			if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultYard())) {
				params.getPlay().getPlayResult().setPlayResultYard(yardGain);
			} else {
				params.getPlay().getPlayResult().updatePenaltyPlayResultYard(yardGain);
			}

			parseRushingBasicExtraStatHelper(yardGain, rushingStat);

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
					.add(rushingStat);
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parseRushingBasicExtraStatHelper(Integer yardGain, PbpPlayerStatRushingPojo rushingStat) {
		try {
			Double rushingLineYard;
			Integer rushingOpenFieldYard = null;
			boolean rushingOpenField = false;
			Integer rushingSecondLevelYard = null;
			boolean rushingSecondLevel = false;
			boolean rushingStuff = false;

			if (yardGain <= 0) {
				rushingStuff = true;
				rushingLineYard = 1.2 * yardGain;
			} else if (yardGain <= 4) {
				rushingLineYard = yardGain.doubleValue();
			} else if (yardGain <= 10) {
				rushingSecondLevel = true;
				rushingLineYard = 4 + ((yardGain - 4) * .5);
				rushingSecondLevelYard = yardGain - 5;
			} else {
				rushingSecondLevel = true;
				rushingOpenField = true;
				rushingOpenFieldYard = yardGain - 10;
				rushingLineYard = 7.0;
				rushingSecondLevelYard = 5;
			}

			rushingStat.setRushingLineYard(rushingLineYard);
			rushingStat.setRushingOpenField(rushingOpenField);
			rushingStat.setRushingOpenFieldYard(rushingOpenFieldYard);
			rushingStat.setRushingSecondLevel(rushingSecondLevel);
			rushingStat.setRushingSecondLevelYard(rushingSecondLevelYard);
			rushingStat.setRushingStuff(rushingStuff);
		} catch (Exception e) {
			LoggingUtils.logException(e, e.getMessage());
		}
	}

}
