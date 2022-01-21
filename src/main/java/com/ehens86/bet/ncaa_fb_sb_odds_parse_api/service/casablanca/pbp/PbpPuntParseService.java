package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;


import org.apache.logging.log4j.ThreadContext;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PbpPlayerStatSpecialTeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

public final class PbpPuntParseService {

	// private static static constructor to prevent instantiation
    private PbpPuntParseService() {
        throw new UnsupportedOperationException();
    }

	public static boolean parsePunt(PbpServiceRequestPojo params, boolean updated) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("PUNT")) {
				cleanupPuntString(params);
				parsePuntBasic(params);
				parsePuntReturn(params);
				parsePuntMuffed(params);
				parsePuntFairCatch(params);
				parsePuntTouchback(params);
				parsePuntReturnStartYard(params);
				parsePuntBlock(params);

				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					String retTeam = params.getPossessionTeam();
					String puntTeam = params.getDefenseTeam();
					PbpPlayerStatSpecialTeamPojo returnInfo = params.getPlay().getPlayerStat().get(retTeam)
							.getSpecialTeam();
					PbpPlayerStatSpecialTeamPojo puntInfo = params.getPlay().getPlayerStat().get(puntTeam)
							.getSpecialTeam();

					LoggingUtils.logInfo("- [parsePunt] Results -");
					LoggingUtils.logInfo(
							String.format("Punting Team: %s", params.getTeamAbbrevDict().get(puntTeam).getShortname()));
					LoggingUtils.logInfo(String.format("Punt Yards: %s", puntInfo.getPunting().get(0).getPuntYard()));
					LoggingUtils.logInfo(
							String.format("Punt Yardline Landed: %s", puntInfo.getPunting().get(0).getPuntLandYard()));
					LoggingUtils.logInfo(
							String.format("Punt Return Yards: %s", puntInfo.getPunting().get(0).getPuntReturnYard()));
					LoggingUtils.logInfo(String.format("Punt Info: %s", puntInfo.getPunting().get(0).toString()));

					LoggingUtils.logInfo(
							String.format("Return Team: %s", params.getTeamAbbrevDict().get(retTeam).getShortname()));
					if (!returnInfo.getPuntReturn().isEmpty()) {
						LoggingUtils.logInfo(String.format("Return Start Yardline: %s",
								returnInfo.getPuntReturn().get(0).getPuntReturnStartYard()));
						LoggingUtils.logInfo(String.format("Return Yards: %s",
								returnInfo.getPuntReturn().get(0).getPuntReturnYard()));
						LoggingUtils.logInfo(
								String.format("Return Info: %s", returnInfo.getPuntReturn().get(0).toString()));
					} else {
						LoggingUtils.logInfo("Return: None");
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

	private static void cleanupPuntString(PbpServiceRequestPojo params) {
		if ("STEINDORF, Kaedin punt yards to the , End of Play.".equals(params.getPlayRawText())) {
			// TODO Handle this
			return;
		}
		if (" punt yards to the , End of Play.".equals(params.getPlayRawText())) {
			// TODO Handle this
			return;
		}
		if (params.getPlayRawText().startsWith(" punt")) {
			params.setPlayRawText(String.format("%s, TEAM%s",
					params.getTeamAbbrevDict().get(params.getDefenseTeam()).getSeoName().toUpperCase().replace(" ", ""),
					params.getPlayRawText()));
			params.getPlay().setPlayText(params.getPlayRawText());
		}
		if (params.getPlayRawText().startsWith("punt")) {
			params.setPlayRawText(String.format("%s, TEAM %s",
					params.getTeamAbbrevDict().get(params.getDefenseTeam()).getSeoName().toUpperCase().replace(" ", ""),
					params.getPlayRawText()));
			params.getPlay().setPlayText(params.getPlayRawText());
		}
		params.setPlayRawText(params.getPlayRawText().replace(" loss of ", " -"));
		params.getPlay().setPlayText(params.getPlay().getPlayText().replace(" loss of ", " -"));

		params.setPlayRawText(params.getPlayRawText().replace(
				"Silva,Evan punt 41 yards to the EWU24 return 8 yards to the EWU32 (Lewis,Aaron)",
				"Silva,Evan punt 41 yards to the EWU24 Chism, Efton return 8 yards to the EWU32 (Lewis,Aaron)"));

		params.setPlayRawText(params.getPlayRawText().replaceAll(
				"(Punt .+ blocked .+ recovered .+ returned )(0)( yards? to the .+?)(\\d{1,2})(, TOUCHDOWN)",
				"$1$4$3$4$5"));
	}

	private static void parsePuntBasic(PbpServiceRequestPojo params) {
		try {
			PbpPlayerStatPuntingPojo puntingStat = new PbpPlayerStatPuntingPojo();

			String puntString = PbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"((%s|[aA-zZ]+?)( fumbled snap,)? punt ((-?\\d+) yards to the%s|blocked|BLOCKED))|Punt by %s at the%s is blocked",
					NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX, NcaaConstants.PLAYER_NAME_REGEX,
					NcaaConstants.TEAM_YARD_REGEX), 21);
			String[] puntStringArray = puntString.split("\\|")[0].split("\\~");
			puntStringArray[12] = puntStringArray[12].toUpperCase();

			if ("BLOCKED".equals(puntStringArray[10])) {
				puntingStat.setPlayerName(PbpParsingUtils.formatName(puntStringArray[1]));
				puntingStat.setPuntYard(0);
				puntingStat.setPuntLandYard(params.getPlay().getPlayStartYard());
			} else if ("null".equals(puntStringArray[0])) {
				puntingStat.setPlayerName(PbpParsingUtils.formatName(puntStringArray[13]));
				puntingStat.setPuntYard(0);
				puntingStat.setPuntLandYard(params.getPlay().getPlayStartYard());
			} else {
				puntingStat.setPlayerName(PbpParsingUtils.formatName(puntStringArray[1]));
				puntingStat.setPuntYard(Integer.valueOf(puntStringArray[11]));
				puntingStat.setPuntLandYard(PbpParsingUtils.formatYardLine(puntStringArray[12],
						params.getDefenseTeam(), params.getPossessionTeam(), params.getTeamAbbrevDict()));
			}

			puntingStat.setPunt(1);
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting()
					.add(puntingStat);
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parsePuntReturn(PbpServiceRequestPojo params) {
		try {
			if (PbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s", NcaaConstants.PLAYER_NAME_REGEX,
							NcaaConstants.TEAM_YARD_REGEX))) {
				String puntReturnString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s",
								NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX),
						10);
				String[] puntReturnStringArray = puntReturnString.split("\\~");
				String playerName = PbpParsingUtils.formatName(puntReturnStringArray[0]);
				Integer returnYards = Integer.valueOf(puntReturnStringArray[8]);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findPuntReturnByName(playerName).setPuntReturnYard(returnYards);

				if (" loss".equals(puntReturnStringArray[9])) {
					returnYards *= -1;
				}
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnYard(returnYards);
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnYard(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parsePuntFairCatch(PbpServiceRequestPojo params) {
		try {
			if (PbpParsingUtils.evalMatch(params.getPlayRawText(), "([aA-zZ]{2,3}\\d{1,2},? fair catch by)")) {
				/**
				 * Add punt return fair catch stats
				 */
				String returnerName = PbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format(" fair catch by %s", NcaaConstants.PLAYER_NAME_REGEX), 1).split("\\~")[0];

				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findPuntReturnByName(PbpParsingUtils.formatName(returnerName)).applyReturnFairCatch();

				/**
				 * Add punt fair catch stats
				 */
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.applyPuntFairCatch();
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntFairCatch(0);
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
							.get(0).setPuntReturnFairCatch(0);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parsePuntTouchback(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("TOUCHBACK")) {
				/**
				 * Add punting touchback stats
				 */
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.applyPuntTouchback();
				params.getPlay().getPlayResult().setPlayResultYardLine(20);
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntTouchback(0);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parsePuntReturnStartYard(PbpServiceRequestPojo params) {
		try {
			if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
					.isEmpty()) {
				/**
				 * Add punt return start yard
				 */
				String startYard = PbpParsingUtils
						.extractCustom(params.getPlayRawText(),
								String.format("punt -?\\d{1,2} yards to the%s", NcaaConstants.TEAM_YARD_REGEX), 1)
						.split("\\~")[0].toUpperCase();
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn().get(0)
						.setPuntReturnStartYard(PbpParsingUtils.formatYardLine(startYard,
								params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parsePuntMuffed(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().contains(" muff")) {
				if (PbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("(?:return)? muffed by %s at(?: the)?%s", NcaaConstants.PLAYER_NAME_REGEX,
								NcaaConstants.TEAM_YARD_REGEX))) {
					String returnerMuffString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("(?:return)? muffed by %s at(?: the)?%s", NcaaConstants.PLAYER_NAME_REGEX,
									NcaaConstants.TEAM_YARD_REGEX),
							8);
					String returnerName = returnerMuffString.split("\\~")[0];
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
							.findPuntReturnByName(PbpParsingUtils.formatName(returnerName)).applyReturnMuff();
					parsePuntReturnStartYard(params);
					parsePuntFairCatch(params);
				} else {
					throw new IllegalArgumentException("Handle Case");
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private static void parsePuntBlock(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("BLOCK")) {
				String puntBlockString = PbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("\\(?blocked by %s\\)?", NcaaConstants.PLAYER_NAME_REGEX), 7);
				String puntBlockName = puntBlockString.split("\\~")[0];
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntBlocked(1);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.applyPuntBlock(PbpParsingUtils.formatName(puntBlockName));
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntBlocked(0);
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().findPuntReturner()
							.setPuntReturnBlock(0);
				}
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, params.getPlayRawText());
		}
	}

}
