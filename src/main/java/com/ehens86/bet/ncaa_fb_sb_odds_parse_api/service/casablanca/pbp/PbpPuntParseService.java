package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntReturnPojo;
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

	public void parsePunt(PbpServiceRequestPojo params) {
		try {
			boolean simplePunt = false;
			params.getDrive().setKickoff(false);
			PlayerStatPuntingPojo puntingStat = new PlayerStatPuntingPojo();
			params.getPlay().setPlayType(PlayTypeEnum.PUNT);
			String[] playPlayerNames = pbpParsingUtils.extractNames(params.getPlayRawText());
			String[] playYardLines = pbpParsingUtils.extractYard(params.getPlayRawText());

			puntingStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[0]));

			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("%s punt (\\d+) yards to the ([aA-zZ]{2,3}\\d{1,2})\\.", playPlayerNames[0]))) {
				simplePunt = true;
			}



			parsePuntBasic(params, puntingStat, playPlayerNames);

//			if (simplePunt) {
//				/**
//				 * BASIC PUNT
//				 */
//				parsePuntBasic(params, playYardLines, puntingStat, playPlayerNames);
//			} else 
//				if (params.getPlayRawText().toUpperCase().contains("MUFFED") || params.getPlayRawText().toUpperCase().contains("RECOVER")) {
//				/**
//				 * MUFFED
//				 */
//				parsePuntMuffed(params, playYardLines, puntingStat, playPlayerNames);
//			} else if (params.getPlayRawText().toUpperCase().contains("FUMBLE")) {
//				/**
//				 * FUMBLE
//				 */
//				parsePuntFumble(params, playYardLines, puntingStat, playPlayerNames);
//			} else if (params.getPlayRawText().toUpperCase().contains("RECOVER")) {
//				/**
//				 * RECOVERY
//				 */
//				parsePuntRecover(params, playYardLines, puntingStat, playPlayerNames);
//			} else if (params.getPlayRawText().toUpperCase().contains("TOUCHBACK")) {
//				/**
//				 * TOUCHBACK
//				 */
//				parsePuntTouchback(params, playYardLines, puntingStat, playPlayerNames);
//			} else if (params.getPlayRawText().toUpperCase().contains("TOUCHDOWN")) {
//				/**
//				 * TOUCHDOWN
//				 */
//				parsePuntTouchdown(params, playYardLines, puntingStat, playPlayerNames);
//			} else if (params.getPlayRawText().toUpperCase().contains("RETURN")) {
//				/**
//				 * STANDARD RETURN
//				 */
//				parsePuntReturn(params, playYardLines, puntingStat, playPlayerNames);
//			} else if (params.getPlayRawText().toUpperCase().contains("BLOCK")) {
//				/**
//				 * BLOCKED
//				 */
//				parsePuntBlock(params, playYardLines, puntingStat, playPlayerNames);
//			} else if (params.getPlayRawText().toUpperCase().contains("FAIR CATCH")) {
//				/**
//				 * FAIR CATCH
//				 */
//				parsePuntFairCatch(params, playYardLines, puntingStat, playPlayerNames);
//			} else {
//				/**
//				 * OTHER?
//				 */
//				System.out.println(params.getPlayRawText());
//				System.out.println("MISSING ^^^^^^^");
//				return;
//			}
			System.out.println(params.getPlayRawText());
			validate(params);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void validate(PbpServiceRequestPojo params) { // DrivePojo
																											// drive
		LOG.log(Level.INFO, "-- Punt validation");
		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("Drive.isKickoff()");
		}

		if (params.getPlay().getPlayStartDown() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartDown() == null");
		}
		if (params.getPlay().getPlayStartYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayStartYard() == null");
		}
		if (params.getPlay().getPlayYardToGain() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayYardToGain() == null");
		}
		if (params.getPlay().getPlayCallType() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() == null");
		}
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.PUNT");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() != null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() != null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() != Boolean.FALSE) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown() != Boolean.FALSE");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 1");
		}

		/**
		 * NULL CHECK
		 */
		if (StringUtils.isBlank(
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPlayerName())) {
			throw new IllegalArgumentException(
					"StringUtils.isBlank(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPlayerName())");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntTouchback() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntTouchback() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntReturnYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntReturnYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntOutOfBounds() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntOutOfBounds() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntReturnTouchdown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntReturnTouchdown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntFairCatch() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntFairCatch() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPunt() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPunt() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntLandYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntLandYard() == null");
		}

		/**
		 * VALUE CHECK
		 */
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntTouchback() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntTouchback() > 1");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntOutOfBounds() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntOutOfBounds() > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntReturnTouchdown()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntFairCatch() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntFairCatch()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPunt() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPunt() > 1");
		}
//		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntLandYard() > 1) {
//			throw new IllegalArgumentException(
//					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
//							+ "					.getPuntLandYard() > 1");
//		}

		/**
		 * CONDITIONAL CHECK
		 */
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntTouchback() == 1) {
			/**
			 * TOUCHBACK
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnYard() != 25) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnYard() != 25");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntOutOfBounds() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntBlocked() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntBlocked() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnTouchdown() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntFairCatch() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntFairCatch() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0");
			}
		}

		else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntFairCatch() == 1) {
			/**
			 * FAIR CATCH
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnYard() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnYard() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntBlocked() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntBlocked() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntTouchback() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntTouchback() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnTouchdown() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntOutOfBounds() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFairCatch() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0).getPuntReturnFairCatch() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnStartYard() != 100 - (params.getPlay().getPlayStartYard() + params.getPlay().getPlayerStat()
							.get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntYard())) {

				System.out.println(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
						.getPuntReturnStartYard());
				System.out.println(params.getPlay().getPlayStartYard());
				System.out.println(
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntYard());
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "						.getPuntReturnStartYard() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFumble() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0).getPuntReturnFumble() != 0");
			}
		}

		else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntOutOfBounds() == 1) {
			/**
			 * OUT OF BOUNDS
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnYard() != 35) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnYard() != 35");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPuntBlocked() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntBlocked() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntTouchback() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntTouchback() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnTouchdown() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntFairCatch() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntFairCatch() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0");
			}
		}

		else if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntBlocked() == 1) {
			/**
			 * BLOCKED
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnYard() != 35) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnYard() != 35");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntOutOfBounds() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntOutOfBounds() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntTouchback() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntTouchback() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnTouchdown() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntReturnTouchdown() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntFairCatch() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntFairCatch() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().size() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0");
			}
		}

	}

	private void parsePuntTouchdown(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			PlayerStatPuntReturnPojo returnStat = new PlayerStatPuntReturnPojo();
			System.out.println(params.getPlayRawText());
//			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
//			returnStat.setPuntReturn(1);
//			returnStat.setPuntReturnStartYard(100
//					- pbpParsingUtils.formatYardLine(playYardLines[1], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
//			returnStat.setPuntReturnYard(0);
//
//			returnStat.setPuntReturnFairCatch(1);
//			puntingStat.setPuntTouchback(0);
//
//			puntingStat.setPuntFairCatch(1);
//			puntingStat.setPuntOutOfBounds(0);
//			puntingStat.setPuntReturnYard(0);
//			puntingStat.setPuntReturnTouchdown(0);
//			puntingStat.setPuntBlocked(0);
//
//			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
//			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);

			throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntRecover(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			PlayerStatPuntReturnPojo returnStat = new PlayerStatPuntReturnPojo();
			System.out.println(params.getPlayRawText());
//			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
//			returnStat.setPuntReturn(1);
//			returnStat.setPuntReturnStartYard(100
//					- pbpParsingUtils.formatYardLine(playYardLines[1], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
//			returnStat.setPuntReturnYard(0);
//
//			returnStat.setPuntReturnFairCatch(1);
//			puntingStat.setPuntTouchback(0);
//
//			puntingStat.setPuntFairCatch(1);
//			puntingStat.setPuntOutOfBounds(0);
//			puntingStat.setPuntReturnYard(0);
//			puntingStat.setPuntReturnTouchdown(0);
//			puntingStat.setPuntBlocked(0);
//
//			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
//			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);

			throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntFumble(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			PlayerStatPuntReturnPojo returnStat = new PlayerStatPuntReturnPojo();
			System.out.println(params.getPlayRawText());
//			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
//			returnStat.setPuntReturn(1);
//			returnStat.setPuntReturnStartYard(100
//					- pbpParsingUtils.formatYardLine(playYardLines[1], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
//			returnStat.setPuntReturnYard(0);
//
//			returnStat.setPuntReturnFairCatch(1);
//			puntingStat.setPuntTouchback(0);
//
//			puntingStat.setPuntFairCatch(1);
//			puntingStat.setPuntOutOfBounds(0);
//			puntingStat.setPuntReturnYard(0);
//			puntingStat.setPuntReturnTouchdown(0);
//			puntingStat.setPuntBlocked(0);
//
//			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
//			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);

			throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntMuffed(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			boolean puntingRecovery = false;
			boolean loss = false;

			puntingStat.setPuntReturnTouchdown(0);
			puntingStat.setPuntBlocked(0);
			puntingStat.setPuntFairCatch(0);
			puntingStat.setPuntOutOfBounds(0);
			puntingStat.setPuntTouchback(0);

			PlayerStatPuntReturnPojo returnStat = new PlayerStatPuntReturnPojo();
			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
			returnStat.setPuntReturn(1);
			returnStat.setPuntReturnFumble(1);
			returnStat.setPuntReturnFairCatch(0);

			returnStat.setPuntReturnStartYard(
					100 - pbpParsingUtils.formatYardLine(playYardLines[0], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format(
					"%s punt (\\d+) yards? to the ([aA-zZ]{2,3}\\s?\\d{1,2}) return muffed by ([aA-zZ][aA-zZ]+,.?[aA-zZ][aA-zZ]+) at the ([aA-zZ]{2,3}\\d{1,2}), out of bounds.",
					playPlayerNames[0]))) {
				returnStat.setPuntReturnYard(
						(100 - pbpParsingUtils.formatYardLine(playYardLines[1], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()))
								- returnStat.getPuntReturnStartYard());
				puntingStat.setPuntReturnYard(returnStat.getPuntReturnYard());

				params.getPlay().getPlayResult().setPlayResultTurnover(puntingRecovery);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);
				return;
			} else {
				String puntReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
						"%s punt (\\d+) yards? to the ([aA-zZ]{2,3}\\s?\\d{1,2}) return muffed by ([aA-zZ][aA-zZ]+,.?[aA-zZ][aA-zZ]+) at the ([aA-zZ]{2,3}\\d{1,2}), recovered by ([aA-zZ]{2,3}) ([aA-zZ][aA-zZ]+,.?[aA-zZ][aA-zZ]+) at the ([aA-zZ]{2,3}\\d{1,2}), returned (\\d{1,3}) yards to the ([aA-zZ]{2,3}\\d{1,2})",
						playPlayerNames[0]), 9);
				String[] puntReturnStringSplit = puntReturnString.split("\\~");
				returnStat.setPuntReturnYard(
						(100 - pbpParsingUtils.formatYardLine(playYardLines[1], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()))
								- returnStat.getPuntReturnStartYard());

				puntingStat.setPuntReturnYard(returnStat.getPuntReturnYard());

				if (returnStat.getPuntReturnYard() < 0) {
					loss = true;
				}

				if (pbpParsingUtils.resolvePossesionTeam(puntReturnStringSplit[4], params.getPossessionTeam(), params.getDefenseTeam(),
						params.getTeamAbbrevDict())) {
					PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
					kickCoverage.setPlayerName(pbpParsingUtils.formatName(puntReturnStringSplit[5]));
					kickCoverage.setFumbleRecovered(0);
					kickCoverage.setFumbleYard(Integer.valueOf(puntReturnStringSplit[7]));
					kickCoverage.setTackleSolo(0.0);
					kickCoverage.setTackleAssist(0.0);
					kickCoverage.setTackleTotal(0);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().add(kickCoverage);
					params.getPlay().getPlayResult().setPlayResultTurnover(true);
				} else {
					throw new IllegalArgumentException("Return team recovered punt... need to handle this case");
				}

				params.getPlay().getPlayResult().setPlayResultTurnover(puntingRecovery);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);
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

	private void parsePuntTouchback(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {

			puntingStat.setPuntTouchback(1);

			puntingStat.setPuntFairCatch(0);
			puntingStat.setPuntOutOfBounds(0);
			puntingStat.setPuntReturnYard(25);
			puntingStat.setPuntReturnTouchdown(0);
			puntingStat.setPuntBlocked(0);
//
//			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
//			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);

//			throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntBlock(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			PlayerStatPuntReturnPojo returnStat = new PlayerStatPuntReturnPojo();
			System.out.println(params.getPlayRawText());
//			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
//			returnStat.setPuntReturn(1);
//			returnStat.setPuntReturnStartYard(100
//					- pbpParsingUtils.formatYardLine(playYardLines[1], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
//			returnStat.setPuntReturnYard(0);
//
//			returnStat.setPuntReturnFairCatch(1);
//			puntingStat.setPuntTouchback(0);
//
//			puntingStat.setPuntFairCatch(1);
//			puntingStat.setPuntOutOfBounds(0);
//			puntingStat.setPuntReturnYard(0);
//			puntingStat.setPuntReturnTouchdown(0);
//			puntingStat.setPuntBlocked(0);
//
//			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
//			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);

			throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntBasic(PbpServiceRequestPojo params, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			String[] downAndDistance = pbpParsingUtils.convertDownAndDistance(params.getPlayRaw().getDriveText());
			String puntString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("%s punt (\\d+) yards to the ([A-Z]*?-?([aA-zZ]{2,3}\\s?\\d{1,2})|50 yardline)", playPlayerNames[0]), 2);
			String[] puntStringArray = puntString.split("\\|")[0].split("\\~");

			puntingStat.setPuntYard(Integer.valueOf(puntStringArray[0]));
			puntingStat.setPunt(1);
			puntingStat.setPuntLandYard(
					pbpParsingUtils.formatYardLine(puntStringArray[1], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			params.getPlay().setPlayStartDown(PlayDownEnum.valueOf(downAndDistance[0]));
			params.getPlay().setPlayYardToGain(Integer.valueOf(downAndDistance[1]));
			params.getPlay().setPlayStartYard(
					pbpParsingUtils.formatYardLine(downAndDistance[2], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			params.getPlay().setPlayCallType(PlayCallTypeEnum.PUNT);
			params.getPlay().getPlayResult().setPlayResultFirstDown(false);
			
			
//			puntingStat.setPuntTouchback(0);
//
//			puntingStat.setPuntFairCatch(0);
//			puntingStat.setPuntOutOfBounds(0);
//			puntingStat.setPuntReturnYard(0);
//			puntingStat.setPuntReturnTouchdown(0);
//			puntingStat.setPuntBlocked(0);

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
			// throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntFairCatch(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			PlayerStatPuntReturnPojo returnStat = new PlayerStatPuntReturnPojo();
			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
			returnStat.setPuntReturn(1);
			returnStat.setPuntReturnFumble(0);

			returnStat.setPuntReturnStartYard(100
					- pbpParsingUtils.formatYardLine(playYardLines[0], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			returnStat.setPuntReturnYard(0);

			returnStat.setPuntReturnFairCatch(1);
			puntingStat.setPuntTouchback(0);

			puntingStat.setPuntFairCatch(1);
			puntingStat.setPuntOutOfBounds(0);
			puntingStat.setPuntReturnYard(0);
			puntingStat.setPuntReturnTouchdown(0);
			puntingStat.setPuntBlocked(0);

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);

			// throw new IllegalArgumentException();
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parsePuntReturn(PbpServiceRequestPojo params,
			String[] playYardLines, PlayerStatPuntingPojo puntingStat, String[] playPlayerNames) {
		try {
			boolean loss = false;

			PlayerStatPuntReturnPojo returnStat = new PlayerStatPuntReturnPojo();
			returnStat.setPlayerName(pbpParsingUtils.formatName(playPlayerNames[1]));
			returnStat.setPuntReturn(1);
			returnStat.setPuntReturnFumble(0);

			returnStat.setPuntReturnStartYard(100
					- pbpParsingUtils.formatYardLine(playYardLines[0], params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));

			String puntReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("%s return (-?\\d+) yards? to the (([aA-zZ]{2,6}\\s?\\d{1,2})|50 yardline)", playPlayerNames[1]),
					2);

			returnStat.setPuntReturnYard(Integer.valueOf(puntReturnString.split("\\~")[0]));

			returnStat.setPuntReturnFairCatch(0);
			puntingStat.setPuntTouchback(0);

			puntingStat.setPuntFairCatch(0);
			puntingStat.setPuntOutOfBounds(0);
			puntingStat.setPuntReturnYard(returnStat.getPuntReturnYard());
			puntingStat.setPuntReturnTouchdown(0);
			puntingStat.setPuntBlocked(0);

			if (returnStat.getPuntReturnYard() < 0) {
				loss = true;
			}

			if (params.getPlayTackles().length == 1 && "".equals(params.getPlayTackles()[0])) {
				if (!params.getPlayRawText().contains("out-of-bounds")) {
					LOG.log(Level.WARNING, String.format("WARNING: no tackles found for play: %s", params.getPlayRawText()));
				}
			} else if (params.getPlayTackles().length == 1) {
				PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
				kickCoverage.setPlayerName(pbpParsingUtils.formatName(params.getPlayTackles()[0]));
				kickCoverage.setTackleSolo(1.0);
				kickCoverage.setTackleAssist(0.0);
				kickCoverage.setTackleTotal(1);
				if (loss) {
					kickCoverage.setTackleForLoss(1);
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().add(kickCoverage);

			} else {
				for (String coverageTackle : params.getPlayTackles()) {
					PlayerStatDefenseProductionPojo kickCoverage = new PlayerStatDefenseProductionPojo();
					kickCoverage.setPlayerName(pbpParsingUtils.formatName(coverageTackle));
					kickCoverage.setTackleTotal(1);
					kickCoverage.setTackleAssist(1.0);
					if (loss) {
						kickCoverage.setTackleForLoss(1);
					}
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKickCoverage().add(kickCoverage);
				}
			}

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().add(puntingStat);
			params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().add(returnStat);

			// throw new IllegalArgumentException();
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
