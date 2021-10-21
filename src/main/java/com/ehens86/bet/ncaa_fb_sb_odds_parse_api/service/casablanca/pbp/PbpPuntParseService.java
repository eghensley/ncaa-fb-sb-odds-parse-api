package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
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
			//TODO punt coverage base class
			//TODO remove tackle format name
			//TODO check turnover logic

			if ("STEINDORF, Kaedin punt yards to the , End of Play.".equals(params.getPlayRawText())) {
				return;
			}
			if (" punt yards to the , End of Play.".equals(params.getPlayRawText())) {
				return;
			}
			if (params.getPlayRawText().startsWith(" punt")) {
				params.setPlayRawText(
						String.format("%s, TEAM%s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
								.getSeoName().toUpperCase().replace(" ", ""), params.getPlayRawText()));
				params.getPlay().setPlayText(params.getPlayRawText());
				System.out.println(params.getPlay().getPlayText());
			}
			if (params.getPlayRawText().startsWith("punt")) {
				params.setPlayRawText(
						String.format("%s, TEAM %s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
								.getSeoName().toUpperCase().replace(" ", ""), params.getPlayRawText()));
				params.getPlay().setPlayText(params.getPlayRawText());
				System.out.println(params.getPlay().getPlayText());
			}
			params.setPlayRawText(params.getPlayRawText().replace(" loss of ", " -"));
			params.getPlay().setPlayText(params.getPlay().getPlayText().replace(" loss of ", " -"));

			params.getDrive().setKickoff(false);
			params.getPlay().setPlayType(PlayTypeEnum.PUNT);

			parsePuntBasic(params);
			parsePuntReturn(params);
			parsePuntTackles(params);
			parsePuntFairCatch(params);
			parsePuntTouchback(params);
			parsePuntReturnStartYard(params);
			parsePuntFumble(params);
			parsePuntMuffed(params);
			parsePuntRecover(params);
			parsePuntBlock(params);
			parsePuntTouchdown(params);
			reconcileFumbleLost(params);
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

	private void validate(PbpServiceRequestPojo params) {
		// LOG.log(Level.INFO, "-- Punt validation");
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
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() != Boolean.FALSE");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().size() != 1");
		}

		/**
		 * NULL CHECK
		 */
		if (StringUtils.isBlank(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
				.getPunting().get(0).getPlayerName())) {
			throw new IllegalArgumentException(
					"StringUtils.isBlank(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0).getPlayerName())");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntYard() == null) {
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
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPunt() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPunt() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntLandYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntLandYard() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntFairCatch() == 1
				&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "						.getPuntFairCatch() == 1\n"
							+ "				&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()\n"
							+ "						.isEmpty()");
		}
		if ((params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntReturnYard() != 0
				&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.getPuntTouchback() != 1)
				&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
			throw new IllegalArgumentException(
					"((params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "				.getPuntReturnYard() != 0\n"
							+ "				&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "						.getPuntTouchback() != 1)\n"
							+ "				&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()\n"
							+ "						.isEmpty()");
		}
		if ((params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntTouchback() == 1
				|| params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.getPuntFairCatch() == 1)
				&& !params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
						.isEmpty()) {
			throw new IllegalArgumentException(
					"(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "				.getPuntTouchback() == 1\n"
							+ "				|| params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "						.getPuntFairCatch() == 1\n)\n"
							+ "				&& !params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()\n"
							+ "						.isEmpty()");
		}

		/**
		 * VALUE CHECK
		 */
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntTouchback() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntTouchback() > 1");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntReturnTouchdown() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntReturnTouchdown()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntFairCatch() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPuntFairCatch()  > 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPunt() > 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
							+ "					.getPunt() > 1");
		}

		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().isEmpty()) {
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPlayerName() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPlayerName() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturn() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getpuntReturn() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnYard() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getpuntReturnYard() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnTouchdown() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getpuntReturnTouchdown() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFairCatch() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getpuntReturnFairCatch() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnStartYard() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getpuntReturnStartYard() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFumble() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getpuntReturnFumble() == null");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFumbleLost() == null) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getpuntReturnFumbleLost() == null");
			}

			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturn() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPuntReturn() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnTouchdown() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPuntReturnTouchdown() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFairCatch() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPuntReturnFairCatch() > 1");
			}
//			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
//					.getPuntReturnStartYard() > 50) {
//				throw new IllegalArgumentException(
//						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
//								+ "					.getPuntReturnStartYard() > 50");
//			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFumble() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPuntReturnFumble() > 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFumbleLost() > 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPuntReturnFumbleLost() > 1");
			}
		}

		/**
		 * CONDITIONAL CHECK
		 */
		if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().isEmpty()
				&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
						.getPuntReturnTouchdown() == 1) {
			/**
			 * RETURN TOUCHDOWN
			 */
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntReturnTouchdown() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntReturnTouchdown() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntBlocked() == 0
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
							.get(0).getPuntLandYard()
							- params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.getPunting().get(0).getPuntReturnYard() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntBlocked() == 0\n"
								+ "					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()\n"
								+ "							.get(0).getPuntLandYard()\n"
								+ "							- params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()\n"
								+ "									.getPunting().get(0).getPuntReturnYard() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntBlocked() == 0
					&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
							.get(0).getPuntReturnStartYard()
							+ params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
									.getPuntReturn().get(0).getPuntReturnYard() != 100) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntBlocked() == 0\n"
								+ "					&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPuntReturnStartYard()\n"
								+ "					+ params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()\n"
								+ "							.get(0).getPuntReturnYard() != 100");
			}

		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntReturnTouchdown() == 1) {
			/**
			 * RETURN TOUCHDOWN
			 */
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().isEmpty()
					|| params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
							.get(0).getPuntReturnTouchdown() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().isEmpty()\n"
								+ "					|| params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "							.getPuntReturnTouchdown() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntBlocked() == 0
					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
							.get(0).getPuntLandYard()
							- params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.getPunting().get(0).getPuntReturnYard() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntBlocked() == 0\n"
								+ "					&& params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()\n"
								+ "							.get(0).getPuntLandYard()\n"
								+ "							- params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()\n"
								+ "									.getPunting().get(0).getPuntReturnYard() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntBlocked() == 0
					&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
							.get(0).getPuntReturnStartYard()
							+ params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
									.getPuntReturn().get(0).getPuntReturnYard() != 100) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "					.getPuntBlocked() == 0\n"
								+ "					&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)\n"
								+ "					.getPuntReturnStartYard()\n"
								+ "					+ params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()\n"
								+ "							.get(0).getPuntReturnYard() != 100");
			}
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntTouchback() == 1) {
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
					.getPuntBlocked() != 0) {
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
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage().size() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
					.size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 0");
			}
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
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
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntBlocked() != 0) {
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
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage().size() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
					.size() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnFairCatch() != 1) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0).getPuntReturnFairCatch() != 1");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
					.getPuntReturnStartYard() != 100 - (params.getPlay().getPlayStartYard()
							+ params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
									.getPunting().get(0).getPuntYard())) {

				System.out.println(params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.getPuntReturn().get(0).getPuntReturnStartYard());
				System.out.println(params.getPlay().getPlayStartYard());
				System.out.println(params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
						.getPunting().get(0).getPuntYard());
				System.out.println(params.getPlay().getDriveText());
				System.out.println(params.getTeamAbbrevDict());

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

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
				.getPuntBlocked() == 1) {
			/**
			 * BLOCKED
			 */
//			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
//					.getPuntReturnYard() != 35) {
//				throw new IllegalArgumentException(
//						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
//								+ "						.getPuntReturnYard() != 35");
//			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntTouchback() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntTouchback() != 0");
			}
//			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
//					.getPuntReturnTouchdown() != 0) {
//				throw new IllegalArgumentException(
//						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
//								+ "						.getPuntReturnTouchdown() != 0");
//			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
					.getPuntFairCatch() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)\n"
								+ "						.getPuntFairCatch() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage()
					.size() != 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPuntCoverage().size() != 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
					.size() == 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().size() == 0");
			}
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().stream()
					.filter(returner -> returner.getPuntReturnBlock() == 1).collect(Collectors.toList()).size() == 0) {
				throw new IllegalArgumentException(
						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().stream()\n"
								+ "					.filter(returner -> returner.getPuntReturnBlock() == 1).collect(Collectors.toList()).size() == 0");
			}
		}

	}

	private void parsePuntBasic(PbpServiceRequestPojo params) {
		try {
			PlayerStatPuntingPojo puntingStat = new PlayerStatPuntingPojo();

			String[] downAndDistance = pbpParsingUtils.convertDownAndDistance(params.getPlay().getDriveText());
//			if ("LPO42".equals(downAndDistance[2])) {
//				System.out.println("catch");
//			}
			String puntString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("(%s|[aA-zZ]+?)( fumbled snap,)? punt ((-?\\d+) yards to the%s|blocked|BLOCKED)",
							NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
					12);
			String[] puntStringArray = puntString.split("\\|")[0].split("\\~");
			puntStringArray[11] = puntStringArray[11].toUpperCase();
			puntingStat.setPlayerName(pbpParsingUtils.formatName(puntStringArray[0]));
			params.getPlay().setPlayStartYard(pbpParsingUtils.formatYardLine(downAndDistance[2],
					params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));

			if ("BLOCKED".equals(puntStringArray[9])) {
				puntingStat.setPuntYard(0);
				puntingStat.setPuntLandYard(params.getPlay().getPlayStartYard());
			} else {
				puntingStat.setPuntYard(Integer.valueOf(puntStringArray[10]));
				puntingStat.setPuntLandYard(pbpParsingUtils.formatYardLine(puntStringArray[11],
						params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			}

			puntingStat.setPunt(1);

			params.getPlay().setPlayStartDown(PlayDownEnum.valueOf(downAndDistance[0]));
			params.getPlay().setPlayYardToGain(Integer.valueOf(downAndDistance[1]));

			params.getPlay().setPlayCallType(PlayCallTypeEnum.PUNT);
			params.getPlay().getPlayResult().setPlayResultFirstDown(false);
// TODO find by player name
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
					String.format("%s (return|for) (-?\\d+) yards? to the%s", NcaaConstants.playerNameRegex,
							NcaaConstants.teamYardRegex))) {
				String puntReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("%s (return|for) (-?\\d+) yards? to the%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex),
						9);
				String playerName = pbpParsingUtils.formatName(puntReturnString.split("\\~")[0]);
				Integer returnYards = Integer.valueOf(puntReturnString.split("\\~")[8]);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findPuntReturnByName(playerName).setPuntReturnYard(returnYards);
				// TODO find by player name
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

	private void parsePuntTackles(PbpServiceRequestPojo params) {
		try {
			boolean loss = false;
			boolean solo = false;
			if (params.getPlayTackles().length == 1 && "".equals(params.getPlayTackles()[0])) {
				return;
			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()
						&& params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
								.getPuntReturn().get(0).getPuntReturnYard() < 0) {
					loss = true;
				}
				if (params.getPlayTackles().length == 1) {
					solo = true;
				}
				for (String coverageTackleRaw : params.getPlayTackles()) {
					if (coverageTackleRaw.contains("blocked by ")) {
						continue;
					}
					String coverageTackleName = pbpParsingUtils.formatName(coverageTackleRaw);

					if (!coverageTackleName.contains(" ")) {
						continue;
					}
					if (solo) {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findPuntCoverageByName(coverageTackleName).applyTackleSolo(loss);
					} else {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
								.findPuntCoverageByName(coverageTackleName).applyTackle(loss);
					}

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

	private void parsePuntFairCatch(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), "([aA-zZ]{2,3}\\d{1,2} fair catch by)")) {
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
						.setPuntReturnStartYard(100 - pbpParsingUtils.formatYardLine(startYard,
								params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			} else {

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

	private void parsePuntFumble(PbpServiceRequestPojo params) {
		try {
//			if (params.getPlayRawText().toUpperCase().contains("FUMBLE")) {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("fumble by %s", NcaaConstants.playerNameRegex))) {
				// String fumbleString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
				// String.format("fumble by %s", NcaaConstants.playerNameRegex), 7);
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn().get(0)
						.setPuntReturnFumble(1);
			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
							.get(0).setPuntReturnFumble(0);
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

	private void parsePuntMuffed(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(), String.format("return muffed by %s at the%s",
					NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex))) {
				String returnerMuffString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
						"return muffed by %s at the%s", NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex), 8);
				String returnerName = returnerMuffString.split("\\~")[0];
				LOG.log(Level.INFO, String.format("Handle return yards after muff-- %s", params.getPlayRawText()));
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
						.findPuntReturnByName(pbpParsingUtils.formatName(returnerName)).applyReturnMuff();
				parsePuntReturnStartYard(params);
				parsePuntFairCatch(params);
			} else {

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

	private void parsePuntRecover(PbpServiceRequestPojo params) {
		try {
			if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
					String.format("recovered by ([aA-zZ]{2,3}) %s at the%s", NcaaConstants.playerNameRegex,
							NcaaConstants.teamYardRegex))) {
				String fumbleReturnString = "";
				Integer fumbleReturnYards;
				String returnerFumbleRecoverString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("recovered by ([aA-zZ]{2,3}) %s at the%s", NcaaConstants.playerNameRegex,
								NcaaConstants.teamYardRegex),
						9);
				String abbrev = returnerFumbleRecoverString.split("\\~")[0];
				String recoverName = pbpParsingUtils.formatName(returnerFumbleRecoverString.split("\\~")[1]);

				boolean turnover = pbpParsingUtils.resolvePossesionTeam(abbrev, params.getPossessionTeam(),
						params.getDefenseTeam(), params.getTeamAbbrevDict());

				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("at the%s, returned (-?\\d+) yards? to the%s", NcaaConstants.teamYardRegex,
								NcaaConstants.teamYardRegex))) {
					fumbleReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("at the%s, returned (-?\\d+) yards? to the%s", NcaaConstants.teamYardRegex,
									NcaaConstants.teamYardRegex),
							3);
					fumbleReturnYards = Integer.valueOf(fumbleReturnString.split("\\~")[1]);
				} else {
					fumbleReturnYards = 0;
				}
				if (turnover) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam()
							.findPuntCoverageByName(recoverName).applyFumbleRecovery(fumbleReturnYards);
				} else {
					if (fumbleReturnYards > 0) {
						throw new IllegalArgumentException("Handle punt return team fumble recover");
					} else {

					}
				}
				params.getPlay().getPlayResult().setPlayResultTurnover(turnover);

			} else {
				params.getPlay().getPlayResult().setPlayResultTurnover(false);

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

	private void parsePuntBlock(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("BLOCK")) {
				System.out.println(params.getPlayRawText());

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

	private void parsePuntTouchdown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("TOUCHDOWN")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("%s (return|for) (-?\\d+) yards? to the%s,? TOUCHDOWN",
								NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex))) {
					String puntReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("%s (return|for) (-?\\d+) yards? to the%s,? TOUCHDOWN",
									NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
							10);
					String returnerName = puntReturnString.split("\\~")[0];
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findPuntReturnByName(pbpParsingUtils.formatName(returnerName)).setPuntReturnTouchdown(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting()
							.get(0).setPuntReturnTouchdown(1);
					params.getPlay().getPlayResult().setPlayResultPoints(-6);
				} else {
					System.out.println(params.getPlayRawText());
					throw new IllegalArgumentException("Handle punt return touchdown");
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getPunting().get(0)
						.setPuntReturnTouchdown(0);
				params.getPlay().getPlayResult().setPlayResultPoints(0);
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

	private void reconcileFumbleLost(PbpServiceRequestPojo params) {
		try {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
					.findPuntReturnWithFumble().isEmpty()) {
				if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findPuntReturnWithFumble().get(0).setPuntReturnFumbleLost(1);
				} else {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam()
							.findPuntReturnWithFumble().get(0).setPuntReturnFumbleLost(0);
				}

			} else {
				if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
						.isEmpty()) {
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getSpecialTeam().getPuntReturn()
							.get(0).setPuntReturnFumbleLost(0);
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
