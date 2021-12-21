package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.List;

import org.apache.logging.log4j.ThreadContext;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpKickoffParseService {
	private final PbpParsingUtils pbpParsingUtils;
	private final LoggingUtils loggingUtils;

	public PbpKickoffParseService(PbpParsingUtils pbpParsingUtils, LoggingUtils loggingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
		this.loggingUtils = loggingUtils;
	}

	public boolean parseKickoff(PbpServiceRequestPojo params, boolean updated) {

		try {
			if (params.getPlayRawText().toUpperCase().contains("KICKOFF")) {
				cleanup(params);
				parseBasic(params);
				parseKickoffReturn(params);
				parseKickReturnStartYard(params);
				parseTouchback(params);
				parseOnsideKick(params);
				parseOutOfBoundsKick(params);
				parseKickoffFairCatch(params);
				parseMuff(params);

				if (NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_DEBUG_KEY))) {
					String retTeam = params.getPossessionTeam();
					String koTeam = params.getDefenseTeam();
					PbpPlayerStatKickoffPojo kickoffInfo = params.getPlay().getPlayerStat().get(koTeam).getSpecialTeam()
							.getKickoff().get(0);
					List<PbpPlayerStatKickReturnPojo> returnInfo = params.getPlay().getPlayerStat().get(retTeam)
							.getSpecialTeam().getKickReturn();

					loggingUtils.logInfo( "- [parseKickoff] Results -");
					loggingUtils.logInfo(
							String.format("Kickoff Team: %s", params.getTeamAbbrevDict().get(koTeam).getShortname()));
					loggingUtils.logInfo( String.format("Kickoff Yards: %s", kickoffInfo.getKickoffYard()));
					loggingUtils.logInfo( String.format("Kickoff Yardline Landed: %s", kickoffInfo.getKickoffLandYard()));
					loggingUtils.logInfo( String.format("Kickoff Return Yards: %s", kickoffInfo.getKickoffReturnYard()));
					loggingUtils.logInfo( String.format("Kickoff Info: %s", kickoffInfo.toString()));

					loggingUtils.logInfo(
							String.format("Return Team: %s", params.getTeamAbbrevDict().get(retTeam).getShortname()));
					if (!returnInfo.isEmpty()) {
						loggingUtils.logInfo( String.format("Return Start Yardline: %s", returnInfo.get(0).getKickReturnStartYard()));
						loggingUtils.logInfo( String.format("Return Yards: %s", returnInfo.get(0).getKickReturnYard()));
						loggingUtils.logInfo( String.format("Return Info: %s", returnInfo.get(0).toString()));
					} else {
						loggingUtils.logInfo( "Return: None");
					}
				}

				updated = true;
			}
			return updated;
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private void cleanup(PbpServiceRequestPojo params) {
		if (params.getPlayRawText().startsWith(" kickoff")) {
			params.setPlayRawText(String.format("%s, TEAM%s",
					params.getTeamAbbrevDict().get(params.getDefenseTeam()).getSeoName().toUpperCase().replace(" ", ""),
					params.getPlayRawText()));
			params.getPlay().setPlayText(params.getPlayRawText());
		}
		if (params.getPlayRawText().startsWith("kickoff")) {
			params.setPlayRawText(String.format("%s, TEAM %s",
					params.getTeamAbbrevDict().get(params.getDefenseTeam()).getSeoName().toUpperCase().replace(" ", ""),
					params.getPlayRawText()));
			params.getPlay().setPlayText(params.getPlayRawText());
		}

		if (params.getPlayRawText().equals("TAGER, Pablo kickoff 70 yards to the CP-5, touchback.")) {
			params.setPlayRawText("TAGER, Pablo kickoff 65 yards to the CP0, touchback.");
		}
		params.setPlayRawText(params.getPlayRawText().replace("TAGER, Pablo kickoff 70 yards to the CP-5, touchback",
				"TAGER, Pablo kickoff 70 yards to the CP0, touchback"));
		params.setPlayRawText(params.getPlayRawText().replace(
				"C. Carrick kickoff 65 yards to the BRY0, return 6 yards to the BRY6 (M. Gavek;A. Rogers)",
				"C. Carrick kickoff 65 yards to the BRY0, YOUNG, Shai return 6 yards to the BRY6 (M. Gavek;A. Rogers)"));
		params.setPlayRawText(params.getPlayRawText().replace(
				"Sojat,Dominik kickoff 65 yards to the GAS00 return 18 yards to the GAS18 (Robinson,Matthew)",
				"Sojat,Dominik kickoff 65 yards to the GAS00, HOOD, Khaleb return 18 yards to the GAS18 (Robinson,Matthew)"));
		params.setPlayRawText(params.getPlayRawText().replace(
				"Sojat,Dominik kickoff 62 yards to the GAS03 return 18 yards to the GAS21 (Coneway,Warren)",
				"Sojat,Dominik kickoff 62 yards to the GAS03, CARTER,Dext return 18 yards to the GAS21 (Coneway,Warren)"));

	}

	private void parseBasic(PbpServiceRequestPojo params) {
		try {
			String kickoffString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("(%s|[aA-zZ]+?)( onside)? kickoff ((-?\\d+) yards.+?to the%s|blocked|BLOCKED)",
							NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX),
					12);
			String[] kickoffStringArray = kickoffString.split("\\|")[0].split("\\~");
			PbpPlayerStatKickoffPojo kickingStat = new PbpPlayerStatKickoffPojo(
					pbpParsingUtils.formatName(kickoffStringArray[0]));
			kickingStat.setKickoffYard(Integer.valueOf(kickoffStringArray[10]));
			Integer formattedYard = pbpParsingUtils.formatYardLine(kickoffStringArray[11], params.getDefenseTeam(),
					params.getPossessionTeam(), params.getTeamAbbrevDict());
			Integer startingYard = formattedYard - kickingStat.getKickoffYard();
			params.getPlay().setPlayStartYard(startingYard);
			kickingStat.setKickoffLandYard(formattedYard);
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseKickoffReturn(PbpServiceRequestPojo params) {
		try {

			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s", NcaaConstants.PLAYER_NAME_REGEX,
							NcaaConstants.TEAM_YARD_REGEX))) {
				String kickReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s",
								NcaaConstants.PLAYER_NAME_REGEX, NcaaConstants.TEAM_YARD_REGEX),
						10);
				String[] kickReturnStringArray = kickReturnString.split("\\~");
				String playerName = pbpParsingUtils.formatName(kickReturnStringArray[0]);
				Integer returnYards = Integer.valueOf(kickReturnStringArray[8]);
				if (" loss".equals(kickReturnStringArray[9])) {
					returnYards *= -1;
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findKickReturnByName(playerName).setKickReturnYard(returnYards);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnYard(returnYards);

			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnYard(0);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseKickReturnStartYard(PbpServiceRequestPojo params) {
		try {
			if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
					.isEmpty()) {
				/**
				 * Add kickoff return start yard
				 */
				String startYard = pbpParsingUtils
						.extractCustom(params.getPlayRawText(),
								String.format("kickoff -?\\d{1,2} yards.+?to the%s", NcaaConstants.TEAM_YARD_REGEX), 1)
						.split("\\~")[0].toUpperCase();
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn().get(0)
						.setKickReturnStartYard(pbpParsingUtils.formatYardLine(startYard, params.getPossessionTeam(),
								params.getDefenseTeam(), params.getTeamAbbrevDict()));
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseTouchback(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String
					.format("kickoff \\d{1,2} yards to the%s, (?i)touchback(?i)", NcaaConstants.TEAM_YARD_REGEX))) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.applyTouchback();
				params.getPlay().getPlayResult().setPlayResultYardLine(25);
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffTouchback(0);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseOnsideKick(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("ONSIDE")) {
				String matchStr = "onside kickoff \\d{1,2} yards, recovered by ([A-Z]*?-?[aA-zZ]{2,3})";
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(), matchStr)) {
					String onsideString = pbpParsingUtils.extractCustom(params.getPlayRawText(), matchStr, 1);
					String[] onsideStringArray = onsideString.split("\\|")[0].split("\\~");
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
							.setKickoffOnsideAttempt(1);
					if (pbpParsingUtils.resolvePossesionTeam(onsideStringArray[0], params.getDefenseTeam(),
							params.getPossessionTeam(), params.getTeamAbbrevDict())) {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff()
								.get(0).setKickoffOnsideSuccess(0);
					} else {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff()
								.get(0).setKickoffOnsideSuccess(1);
					}
				} else if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getPenalty().isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
							.setKickoffOnsideAttempt(1);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
							.setKickoffOnsideSuccess(0);
				} else {
					String logInfo = String.format("Play Text: %s | Regex Match String: %s", params.getPlayRawText(),
							matchStr);
					loggingUtils.logInfo( logInfo);
					throw new IllegalArgumentException("Onside kick parsing failed regex match");
				}

			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOnsideAttempt(0);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOnsideSuccess(0);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseOutOfBoundsKick(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s kickoff (\\d+) yards to the%s,? out.of.?bounds", NcaaConstants.PLAYER_NAME_REGEX,
							NcaaConstants.TEAM_YARD_REGEX))) {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.applyOutOfBounds();
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOutOfBounds(0);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseKickoffFairCatch(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FAIR CATCH")) {
				/**
				 * Add kickoff return return fair catch stats
				 */
				String returnerName = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format(" fair catch by %s", NcaaConstants.PLAYER_NAME_REGEX), 1).split("\\~")[0];

				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findKickReturnByName(pbpParsingUtils.formatName(returnerName))
						.applyReturnFairCatch(params.getPlay().getPlayerStat().get(params.getDefenseTeam())
								.getSpecialTeam().getKickoff().get(0).getKickoffLandYard());

				/**
				 * Add kickoff fair catch stats
				 */
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.applyFairCatch();
			} else {
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffFairCatch(0);
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFairCatch(0);
				}

			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());
		}
	}

	private void parseMuff(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("MUFF")) {
				String returnerMuffString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("(return)? muffed by %s at( the)?%s", NcaaConstants.PLAYER_NAME_REGEX,
								NcaaConstants.TEAM_YARD_REGEX),
						10);
				String returnerName = returnerMuffString.split("\\~")[1];
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.findKickReturnByName(pbpParsingUtils.formatName(returnerName)).applyReturnMuff();
				parseKickReturnStartYard(params);
				parseKickoffFairCatch(params);
			} else {
				if (!params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumble(0);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumbleLost(0);
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());
		}
	}

}
