package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpKickoffParseService {
	private static final String ERROR_S_FAILED_WITH_S_INPUT_S = "ERROR: [%s] failed with %s.  Input = %s";

	private static final Logger LOG = Logger.getLogger(PbpKickoffParseService.class.toString());

	private final PbpParsingUtils pbpParsingUtils;

	public PbpKickoffParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public boolean parseKickoff(PbpServiceRequestPojo params, boolean updated) {

		try {
			// TODO kick coverage base class
			// TODO remove tackle format name
			// TODO check turnover logic
//			String kickoffTeam = params.getPossessionTeam();
//			String kickoffReturnTeam = params.getDefenseTeam();
//			params.setKickoffReturnTeam(kickoffReturnTeam);
//			params.setKickoffTeam(kickoffTeam);
//
//			params.getDrive().setKickoff(true);
//			params.getPlay().setPlayType(PlayTypeEnum.KICKOFF);
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
				updated = true;
			}
			return updated;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void cleanup(PbpServiceRequestPojo params) {
		if (params.getPlayRawText().startsWith(" kickoff")) {
			params.setPlayRawText(String.format("%s, TEAM%s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
					.getSeoName().toUpperCase().replace(" ", ""), params.getPlayRawText()));
			params.getPlay().setPlayText(params.getPlayRawText());
		}
		if (params.getPlayRawText().startsWith("kickoff")) {
			params.setPlayRawText(
					String.format("%s, TEAM %s", params.getTeamAbbrevDict().get(params.getPossessionTeam()).getSeoName()
							.toUpperCase().replace(" ", ""), params.getPlayRawText()));
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
							NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
					12);
			String[] kickoffStringArray = kickoffString.split("\\|")[0].split("\\~");
			PlayerStatKickoffPojo kickingStat = new PlayerStatKickoffPojo(
					pbpParsingUtils.formatName(kickoffStringArray[0]));
			kickingStat.setKickoffYard(Integer.valueOf(kickoffStringArray[10]));
			Integer formattedYard = pbpParsingUtils.formatYardLine(kickoffStringArray[11], params.getPossessionTeam(),
					params.getDefenseTeam(), params.getTeamAbbrevDict());
			Integer startingYard = formattedYard - kickingStat.getKickoffYard();
			params.getPlay().setPlayStartYard(startingYard);
//			params.getPlay().getPlayResult().setPlayResultTurnover(false);
//			params.getPlay().setPlayStartDown(PlayDownEnum.NA);
//			params.getPlay().setPlayCallType(PlayCallTypeEnum.NA);

			kickingStat.setKickoffLandYard(formattedYard);
			// TODO find by player name here
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
					.add(kickingStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickoffReturn(PbpServiceRequestPojo params) {
		try {

			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s", NcaaConstants.playerNameRegex,
							NcaaConstants.teamYardRegex))) {
				String kickReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex),
						10);
				String[] kickReturnStringArray = kickReturnString.split("\\~");
				String playerName = pbpParsingUtils.formatName(kickReturnStringArray[0]);
				Integer returnYards = Integer.valueOf(kickReturnStringArray[8]);
				if (" loss".equals(kickReturnStringArray[9])) {
					returnYards *= -1;
				}
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickReturnByName(playerName).setKickReturnYard(returnYards);
				// TODO find by player name here
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnYard(returnYards);

			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffReturnYard(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickReturnStartYard(PbpServiceRequestPojo params) {
		try {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
					.isEmpty()) {
				/**
				 * Add kickoff return start yard
				 */
				String startYard = pbpParsingUtils
						.extractCustom(params.getPlayRawText(),
								String.format("kickoff -?\\d{1,2} yards.+?to the%s", NcaaConstants.teamYardRegex), 1)
						.split("\\~")[0].toUpperCase();
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn().get(0)
						.setKickReturnStartYard(pbpParsingUtils.formatYardLine(startYard, params.getDefenseTeam(),
								params.getPossessionTeam(), params.getTeamAbbrevDict()));
			} else {

			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseTouchback(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("kickoff \\d{1,2} yards to the%s, (?i)touchback(?i)", NcaaConstants.teamYardRegex))) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.applyTouchback();
				params.getPlay().getPlayResult().setPlayResultYardLine(25);
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffTouchback(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseOnsideKick(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("ONSIDE")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("onside kickoff \\d{1,2} yards, recovered by ([A-Z]*?-?[aA-zZ]{2,3})"))) {
					String onsideString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("onside kickoff \\d{1,2} yards, recovered by ([A-Z]*?-?[aA-zZ]{2,3})"), 1);
					String[] onsideStringArray = onsideString.split("\\|")[0].split("\\~");
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
							.get(0).setKickoffOnsideAttempt(1);
					if (pbpParsingUtils.resolvePossesionTeam(onsideStringArray[0], params.getPossessionTeam(),
							params.getDefenseTeam(), params.getTeamAbbrevDict())) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
								.get(0).setKickoffOnsideSuccess(0);
					} else {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
								.get(0).setKickoffOnsideSuccess(1);
					}
				} else if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getPenalty().isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
							.get(0).setKickoffOnsideAttempt(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff()
							.get(0).setKickoffOnsideSuccess(0);
				} else {
					throw new IllegalArgumentException("handle onside kick");
				}

			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOnsideAttempt(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOnsideSuccess(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseOutOfBoundsKick(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s kickoff (\\d+) yards to the%s,? out.of.?bounds", NcaaConstants.playerNameRegex,
							NcaaConstants.teamYardRegex))) {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.applyOutOfBounds();
//				params.getPlay().getPlayResult().setPlayResultYard(null);
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffOutOfBounds(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseKickoffFairCatch(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FAIR CATCH")) {
				/**
				 * Add kickoff return return fair catch stats
				 */
				String returnerName = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format(" fair catch by %s", NcaaConstants.playerNameRegex), 1).split("\\~")[0];

				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickReturnByName(pbpParsingUtils.formatName(returnerName))
						.applyReturnFairCatch(params.getPlay().getPlayerStat().get(params.getPossessionTeam())
								.getSpecialTeam().getKickoff().get(0).getKickoffLandYard());

				/**
				 * Add kickoff fair catch stats
				 */
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.applyFairCatch();
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickoff().get(0)
						.setKickoffFairCatch(0);
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFairCatch(0);
				}

			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseMuff(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("MUFF")) {
				String returnerMuffString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("(return)? muffed by %s at( the)?%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex),
						10);
				String returnerName = returnerMuffString.split("\\~")[1];
				LOG.log(Level.INFO, String.format("Handle return yards after muff-- %s", params.getPlayRawText()));
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findKickReturnByName(pbpParsingUtils.formatName(returnerName)).applyReturnMuff();
				parseKickReturnStartYard(params);
				parseKickoffFairCatch(params);
			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumble(0);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getKickReturn()
							.get(0).setKickReturnFumbleLost(0);
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(ERROR_S_FAILED_WITH_S_INPUT_S, ste[1].getMethodName(), e.toString(),
					params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

}
