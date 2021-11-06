package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpPuntParseService {
	private static final Logger LOG = Logger.getLogger(PbpPuntParseService.class.toString());

	private final PbpParsingUtils pbpParsingUtils;

	public PbpPuntParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public boolean parsePunt(PbpServiceRequestPojo params, boolean updated) {
		try {
			// TODO punt coverage base class
			// TODO remove tackle format name
			// TODO check turnover logic
			// TODO play result yards for punt/return?

//			String puntTeam = params.getPossessionTeam();
//			String returnTeam = params.getDefenseTeam();
//			params.setPuntReturnTeam(returnTeam);
//			params.setPuntTeam(puntTeam);
//			params.getDrive().setKickoff(false);
//			params.getPlay().setPlayType(PlayTypeEnum.PUNT);
			if (params.getPlayRawText().toUpperCase().contains("PUNT")) {
				cleanupPuntString(params);
				parsePuntBasic(params);
				parsePuntReturn(params);
				parsePuntMuffed(params);
				parsePuntFairCatch(params);
				parsePuntTouchback(params);
				parsePuntReturnStartYard(params);
				parsePuntBlock(params);
				updated = true;
			}
			return updated;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void cleanupPuntString(PbpServiceRequestPojo params) {
		if ("STEINDORF, Kaedin punt yards to the , End of Play.".equals(params.getPlayRawText())) {
			// TODO Handle this
			return;
		}
		if (" punt yards to the , End of Play.".equals(params.getPlayRawText())) {
			// TODO Handle this
			return;
		}
		if (params.getPlayRawText().startsWith(" punt")) {
			params.setPlayRawText(String.format("%s, TEAM%s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
					.getSeoName().toUpperCase().replace(" ", ""), params.getPlayRawText()));
			params.getPlay().setPlayText(params.getPlayRawText());
			System.out.println(params.getPlay().getPlayText());
		}
		if (params.getPlayRawText().startsWith("punt")) {
			params.setPlayRawText(
					String.format("%s, TEAM %s", params.getTeamAbbrevDict().get(params.getPossessionTeam()).getSeoName()
							.toUpperCase().replace(" ", ""), params.getPlayRawText()));
			params.getPlay().setPlayText(params.getPlayRawText());
			System.out.println(params.getPlay().getPlayText());
		}
		params.setPlayRawText(params.getPlayRawText().replace(" loss of ", " -"));
		params.getPlay().setPlayText(params.getPlay().getPlayText().replace(" loss of ", " -"));


		params.setPlayRawText(params.getPlayRawText().replace(
				"Silva,Evan punt 41 yards to the EWU24 return 8 yards to the EWU32 (Lewis,Aaron)",
				"Silva,Evan punt 41 yards to the EWU24 Chism, Efton return 8 yards to the EWU32 (Lewis,Aaron)"));

		params.setPlayRawText(params.getPlayRawText().replaceAll(
				"(Punt .+ blocked .+ recovered .+ returned )(0)( yards? to the .+?)(\\d{1,2})(, TOUCHDOWN)",
				"$1$4$3$4$5"));

//		params.setPlayRawText(
//				params.getPlayRawText().replace("Juan Velarde punt 32 yards to the ALCORN8, fair catch by M. Jones",
//						"Juan Velarde punt 52 yards to the ALCORN8, fair catch by M. Jones"));
//		
//		params.setPlayRawText(
//				params.getPlayRawText().replace("W. Hart punt 32 yards to the SUU11, fair catch by E. Bolingbroke",
//						"W. Hart punt 46 yards to the SUU11, fair catch by E. Bolingbroke"));

	}

	private void parsePuntBasic(PbpServiceRequestPojo params) {
		try {
			PlayerStatPuntingPojo puntingStat = new PlayerStatPuntingPojo();

			String puntString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"((%s|[aA-zZ]+?)( fumbled snap,)? punt ((-?\\d+) yards to the%s|blocked|BLOCKED))|Punt by %s at the%s is blocked",
					NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex, NcaaConstants.playerNameRegex,
					NcaaConstants.teamYardRegex), 21);
			String[] puntStringArray = puntString.split("\\|")[0].split("\\~");
			puntStringArray[12] = puntStringArray[12].toUpperCase();

			if ("BLOCKED".equals(puntStringArray[10])) {
				puntingStat.setPlayerName(pbpParsingUtils.formatName(puntStringArray[1]));
				puntingStat.setPuntYard(0);
				puntingStat.setPuntLandYard(params.getPlay().getPlayStartYard());
			} else if ("null".equals(puntStringArray[0])) {
				puntingStat.setPlayerName(pbpParsingUtils.formatName(puntStringArray[13]));
				puntingStat.setPuntYard(0);
				puntingStat.setPuntLandYard(params.getPlay().getPlayStartYard());
			} else {
				puntingStat.setPlayerName(pbpParsingUtils.formatName(puntStringArray[1]));
				puntingStat.setPuntYard(Integer.valueOf(puntStringArray[11]));
				puntingStat.setPuntLandYard(pbpParsingUtils.formatYardLine(puntStringArray[12], params.getDefenseTeam(),
						params.getPossessionTeam(), params.getTeamAbbrevDict()));
			}

			puntingStat.setPunt(1);
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
					.add(puntingStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntReturn(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s", NcaaConstants.playerNameRegex,
							NcaaConstants.teamYardRegex))) {
				String puntReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("%s (return|for) (-?\\d+) yards?( loss)? to the%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex),
						10);
				String[] puntReturnStringArray = puntReturnString.split("\\~");
				String playerName = pbpParsingUtils.formatName(puntReturnStringArray[0]);
				Integer returnYards = Integer.valueOf(puntReturnStringArray[8]);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findPuntReturnByName(playerName).setPuntReturnYard(returnYards);

				if (" loss".equals(puntReturnStringArray[9])) {
					returnYards *= -1;
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnYard(returnYards);
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnYard(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntFairCatch(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), "([aA-zZ]{2,3}\\d{1,2},? fair catch by)")) {
				/**
				 * Add punt return fair catch stats
				 */
				String returnerName = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format(" fair catch by %s", NcaaConstants.playerNameRegex), 1).split("\\~")[0];

				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findPuntReturnByName(pbpParsingUtils.formatName(returnerName)).applyReturnFairCatch();

				/**
				 * Add punt fair catch stats
				 */
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.applyPuntFairCatch();
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntFairCatch(0);
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
							.get(0).setPuntReturnFairCatch(0);
				}
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

	private void parsePuntTouchback(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("TOUCHBACK")) {
				/**
				 * Add punting touchback stats
				 */
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.applyPuntTouchback();
				params.getPlay().getPlayResult().setPlayResultYardLine(20);
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntTouchback(0);
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

	private void parsePuntReturnStartYard(PbpServiceRequestPojo params) {
		try {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
					.isEmpty()) {
				/**
				 * Add punt return start yard
				 */
				String startYard = pbpParsingUtils
						.extractCustom(params.getPlayRawText(),
								String.format("punt -?\\d{1,2} yards to the%s", NcaaConstants.teamYardRegex), 1)
						.split("\\~")[0].toUpperCase();
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
						.setPuntReturnStartYard(100 - pbpParsingUtils.formatYardLine(startYard, params.getDefenseTeam(),
								params.getPossessionTeam(), params.getTeamAbbrevDict()));
			} else {
				// Handled.
				// throw new IllegalArgumentException("Handle this case");
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntMuffed(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().contains(" muff")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("(?:return)? muffed by %s at(?: the)?%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex))) {
					String returnerMuffString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("(?:return)? muffed by %s at(?: the)?%s", NcaaConstants.playerNameRegex,
									NcaaConstants.teamYardRegex),
							8);
					String returnerName = returnerMuffString.split("\\~")[0];
					LOG.log(Level.INFO, String.format("Handle return yards after muff-- %s", params.getPlayRawText()));
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findPuntReturnByName(pbpParsingUtils.formatName(returnerName)).applyReturnMuff();
					parsePuntReturnStartYard(params);
					parsePuntFairCatch(params);
				} else {
					throw new IllegalArgumentException("Handle Case");
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntBlock(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("BLOCK")) {
//				System.out.println(params.getPlayRawText());

				String puntBlockString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("\\(?blocked by %s\\)?", NcaaConstants.playerNameRegex), 7);
				String puntBlockName = puntBlockString.split("\\~")[0];
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntBlocked(1);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.applyPuntBlock(pbpParsingUtils.formatName(puntBlockName));
				// throw new IllegalArgumentException("Handle punt block");
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntBlocked(0);
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().findPuntReturner()
							.setPuntReturnBlock(0);
				}
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

}
