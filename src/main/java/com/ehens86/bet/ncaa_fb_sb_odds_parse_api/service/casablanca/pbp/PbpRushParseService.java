package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense.PlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpRushParseService {
	private static final Logger LOG = Logger.getLogger(PbpRushParseService.class.toString());

	private final PbpParsingUtils pbpParsingUtils;

	public PbpRushParseService(PbpParsingUtils pbpParsingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
	}

	public void parseRush(PbpServiceRequestPojo params) {
		try {
			params.setPlayRawText(params.getPlayRawText().replace("no gain", "0 yards"));
			// TODO remove this
			// params.setPlayRawText(params.getPlayRawText().replace(".", ""));

//			if (params.getPlayRawText().startsWith(" rush")) {
//				params.setPlayRawText(
//						String.format("TEAM, %s%s", params.getTeamAbbrevDict().get(params.getPossessionTeam())
//								.getSeoName().toUpperCase().replace(" ", ""), params.getPlayRawText()));
//				params.getPlay().setPlayText(params.getPlayRawText());
//			}

			if (params.getPlayRawText().startsWith("rush")) {
				String replacementPlayerName = String.format("TEAM, %s", params.getTeamAbbrevDict()
						.get(params.getPossessionTeam()).getSeoName().toUpperCase().replace(" ", "").replace("-", ""));
				String replacementPlayText = String.format("%s %s", replacementPlayerName, params.getPlayRawText());
				replacementPlayText = replacementPlayText.replaceAll("((fumbled by)( at))",
						String.format("$2 %s$3", replacementPlayerName));

				params.setPlayRawText(replacementPlayText);
				params.getPlay().setPlayText(params.getPlayRawText());
			}

//			params.setPlayRawText(params.getPlayRawText().replaceAll("([A-Z]\\. [A-Z][a-z]+)( Jr\\.)", "$1"));
//			params.setPlayRawText(params.getPlayRawText().replaceAll("(([A-Z])\\.([A-Z][a-z]*))", "$2. $3"));
//			params.setPlayRawText(params.getPlayRawText().replaceAll("([A-Z])\\. ([A-Z])\\.", "$1$2"));
//			params.setPlayRawText(params.getPlayRawText().replaceAll("((, [A-Z])\\. )", "$2 "));
//			params.setPlayRawText(params.getPlayRawText().replaceAll("(( [A-Z][a-z])\\.,? )((?:at|rush))", "$2 $3"));
//			params.setPlayRawText(params.getPlayRawText().replaceAll("[\\.]+$", ""));
//			params.setPlayRawText(params.getPlayRawText()
//					.replaceAll(String.format(" (to the|at)%s,? fumbled? by %s at forced", NcaaConstants.teamYardRegex,
//							NcaaConstants.playerNameRegex), " $1 $2 fumbled by $3 at $2 forced"));
//			params.setPlayRawText(params.getPlayRawText().replace("Albany (NY)", "Albany"));

			params.setPlayRawText(params.getPlayRawText().replace(". St. rush", " rush"));
			params.setPlayRawText(params.getPlayRawText().replace(", rush for ", " rush for "));

			
//			if ("Jones,Donovan rush for 1 yard loss to the GWU22 fumbled by Jones,Donovan at GWU22 forced by Shoemaker,Nick recovered by MON Shoemaker,Nick at GWU22 Shoemaker,Nick return 22 yards to the GWU00 TOUCHDOWN, clock 02:01"
//					.equals(params.getPlayRawText())) {
//				System.out.println("catch");
//			}
			if (params.getPlay().getDriveText().equals("3rd and 0 at -")) {
				params.getPlay().setDriveText("3rd and 0 at ALCORN0");
			}

			params.getDrive().setKickoff(false);
			parseRushingBasic(params);
			if (!pbpParsingUtils.evalMatch(params.getPlayRawText(), "fumble.+recovered")) {
				parseRushingTackles(params);
			}
			parseRushingFirstDown(params);
			parseRushingFumble(params);
			parseRushingRecovery(params);
			parseFumbleForced(params);
			parseRushingTouchdown(params);
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
		//TODO add conditional tests
		// LOG.log(Level.INFO, "-- Rushing validation");
		if (params.getDrive().isKickoff()) {
			throw new IllegalArgumentException("params.getDrive().isKickoff()");
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
		if (params.getPlay().getPlayCallType() != PlayCallTypeEnum.RUN) {
			throw new IllegalArgumentException("params.getPlay().getPlayCallType() != PlayCallTypeEnum.RUN");
		}
		if (params.getPlay().getPlayResult().getPlayResultYard() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().getPlayResultYard() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == null) {
			throw new IllegalArgumentException("params.getPlay().getPlayResult().isPlayResultFirstDown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
				.size() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().size() != 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getPlayerName() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getPlayerName() == null");
		}

		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingFumble() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingFumble() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingFumbleLost() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingFumbleLost() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingAttempt() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingAttempt() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingAttempt() != 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingAttempt() != 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingFirstDown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingFirstDown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingTouchdown() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingTouchdown() == null");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingYard() == null) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingYard() == null");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayerStat()
				.get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingFirstDown() == 0) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == true && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)\n"
							+ "				.getRushingFirstDown() == 0");
		}
		if (params.getPlay().getPlayResult().isPlayResultFirstDown() == false && params.getPlay().getPlayerStat()
				.get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getRushingFirstDown() == 1) {
			throw new IllegalArgumentException(
					"params.getPlay().getPlayResult().isPlayResultFirstDown() == false && params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)\n"
							+ "				.getRushingFirstDown() == 1");
		}
		if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
				.getRushingTouchdown() == 0) {
			if (params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
//				throw new IllegalArgumentException(
//						"params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			}
		} else {
			if (!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
					.isEmpty()) {
				throw new IllegalArgumentException(
						"!params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction().isEmpty()");
			}

		}

	}

	private void parseRushingBasic(PbpServiceRequestPojo params) {
		try {
			String[] downAndDistance = pbpParsingUtils.convertDownAndDistance(params.getPlay().getDriveText());
			String rushString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
					String.format("%s rush for( a? ?(loss|gain) of)? (\\d{1,3}) yards? ?(loss|gain|)(.?)",
							NcaaConstants.playerNameRegex, NcaaConstants.teamYardRegex),
					12);
			String[] rushStringArray = rushString.split("\\|")[0].split("\\~");
			// rushStringArray[11] = rushStringArray[11].toUpperCase();
			PlayerStatRushingPojo rushingStat = new PlayerStatRushingPojo(
					pbpParsingUtils.formatName(rushStringArray[0]));
			Integer yardGain = Integer.valueOf(rushStringArray[9]);
			// rushStringArray.length == 11 &&
			boolean loss = false;
			if (("loss".equals(rushStringArray[10]) || "loss".equals(rushStringArray[8]))) {
				loss = true;
				yardGain *= -1;
			}
			if ((loss && !params.getPlayRawText().toUpperCase().contains("LOSS"))
					|| (!loss && params.getPlayRawText().toUpperCase().contains("LOSS"))) {
				throw new IllegalArgumentException("Loss does not reconcile");
			}
			rushingStat.setRushingYard(yardGain);
			params.getPlay().setPlayStartYard(pbpParsingUtils.formatYardLine(downAndDistance[2],
					params.getPossessionTeam(), params.getDefenseTeam(), params.getTeamAbbrevDict()));
			params.getPlay().setPlayStartDown(PlayDownEnum.valueOf(downAndDistance[0]));
			params.getPlay().setPlayYardToGain(Integer.valueOf(downAndDistance[1]));
			params.getPlay().getPlayResult().setPlayResultYard(yardGain);
			params.getPlay().getPlayResult().setPlayResultTurnover(false);
			params.getPlay().setPlayCallType(PlayCallTypeEnum.RUN);
			// TODO find by player name
			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
					.add(rushingStat);
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
			// e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseRushingTackles(PbpServiceRequestPojo params) {
		try {

			boolean loss = false;
			boolean solo = false;
			if (Objects.isNull(params.getPlayTackles())) {
				return;
			} else {
				if (params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
						.get(0).getRushingYard() < 0) {
					loss = true;
				}
				if (params.getPlayTackles().length == 1) {
					solo = true;
				}
				for (String tackleName : params.getPlayTackles()) {
					if (solo) {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findRushDefenseProductionByName(tackleName).applyTackleSolo(tackleName, loss);
					} else {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findRushDefenseProductionByName(tackleName).applyTackle(tackleName, loss);
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

	private void parseRushingFirstDown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("1ST DOWN")) {
				params.getPlay().getPlayResult().setPlayResultFirstDown(true);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFirstDown(1);
			} else {
				params.getPlay().getPlayResult().setPlayResultFirstDown(false);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFirstDown(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
//			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseRushingFumble(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FUMBLE")) {
				String fumbleString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("(?:(?:fumbled? by %s ?)| %s fumbled at)", NcaaConstants.playerNameRegex,
								NcaaConstants.playerNameRegex),
						14);
				String[] fumbleStringArray = fumbleString.split("\\|")[0].split("\\~");
				fumbleStringArray[0] = fumbleStringArray[0].replace(" recovered", "");
				if (fumbleStringArray[0].endsWith(" at")) {
					fumbleStringArray[0] = fumbleStringArray[0].replace(" at", "");
				}
				String fumbleName;
				if (!"null".equals(fumbleStringArray[0])) {
					fumbleName = fumbleStringArray[0];
				} else {
					fumbleName = fumbleStringArray[7];
				}
				if (!pbpParsingUtils.formatName(fumbleName).equals(params.getPlay().getPlayerStat()
						.get(params.getPossessionTeam()).getOffense().getRushingStat().get(0).getPlayerName())) {
					throw new IllegalArgumentException("RUSHING FUMBLE - Name Mismatch");
				}
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFumble(1);
				// throw new IllegalArgumentException("RUSHING FUMBLE");
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFumble(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFumbleLost(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
//			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseFumbleForced(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("FORCED")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format(" (to the|at)%s,? (fumble )?forced by %s", NcaaConstants.teamYardRegex,
								NcaaConstants.playerNameRegex))) {
					String fumbleRecoveryString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format(" (to the|at)%s,? (fumble )?forced by %s", NcaaConstants.teamYardRegex,
									NcaaConstants.playerNameRegex),
							10);
					String[] fumbleRecoveryStringArray = fumbleRecoveryString.split("\\|")[0].split("\\~");
					if (Objects.isNull(params.getPlayTackles()) || params.getPlay().getPlayResult().isPlayResultTurnover()) {
						params.setPlayTackles(new String[] {pbpParsingUtils.formatName(fumbleRecoveryStringArray[3])});	
						parseRushingTackles(params);
					} else {
						LOG.log(Level.WARNING, "Validate fumble tackle reconciliation");
						//throw new IllegalArgumentException("HANDLE FORCED FUMBLE - tackle reconciliation");
					}
					
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findRushDefenseProductionByName(pbpParsingUtils.formatName(fumbleRecoveryStringArray[3]))
							.setFumbleForced(1);
					// throw new IllegalArgumentException("HANDLE FORCED FUMBLE");
				} else {
					throw new IllegalArgumentException("HANDLE FORCED FUMBLE");
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

	private void parseRushingRecovery(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("RECOVER")) {
				if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
						String.format("recovered by ([A-Z]*?-?[aA-zZ]{2,3}).? %s", NcaaConstants.playerNameRegex))) {
					String fumbleReturnString = "";
					Integer fumbleReturnYards;
					String returnerFumbleRecoverString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
							String.format("recovered by ([A-Z]*?-?[aA-zZ]{2,3}).? %s", NcaaConstants.playerNameRegex),
							8);
					String abbrev = returnerFumbleRecoverString.split("\\~")[0];
					String recoverName = pbpParsingUtils.formatName(returnerFumbleRecoverString.split("\\~")[1]);

					boolean turnover = !pbpParsingUtils.resolvePossesionTeam(abbrev, params.getPossessionTeam(),
							params.getDefenseTeam(), params.getTeamAbbrevDict());

					if (pbpParsingUtils.evalMatch(params.getPlayRawText(),
							String.format("at the%s, returned (-?\\d+) yards? to the%s", NcaaConstants.teamYardRegex,
									NcaaConstants.teamYardRegex))) {
						fumbleReturnString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
								String.format("at the%s, returned (-?\\d+) yards? to the%s",
										NcaaConstants.teamYardRegex, NcaaConstants.teamYardRegex),
								3);
						fumbleReturnYards = Integer.valueOf(fumbleReturnString.split("\\~")[1]);
					} else {
						fumbleReturnYards = 0;
					}
					if (turnover) {
						params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
								.findRushDefenseProductionByName(recoverName).applyFumbleRecovery(fumbleReturnYards);
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
								.get(0).setRushingFumbleLost(1);
					} else {
						params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
								.get(0).setRushingFumbleLost(0);
						if (fumbleReturnYards > 0) {
							LOG.log(Level.WARNING, "Validate offense fumble recovery yards");
							//throw new IllegalArgumentException("Handle offense fumble recover");
						}
					}
					params.getPlay().getPlayResult().setPlayResultTurnover(turnover);

				} else {
					throw new IllegalArgumentException("HANDLE NEW RECOVERY");

				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFumble(0);
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingFumbleLost(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
//			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}

	private void parseRushingTouchdown(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("TOUCHDOWN")) {
				if (params.getPlay().getPlayResult().isPlayResultTurnover()) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
							.get(0).setRushingTouchdown(0);
					params.getPlay().getPlayResult().setPlayResultPoints(-6);
					params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense()
							.findDefenseWithFumbleRecovery().setFumbleTouchdown(1);
				} else {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat()
							.get(0).setRushingTouchdown(1);
					params.getPlay().getPlayResult().setPlayResultPoints(6);
				}
			} else {
				params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getOffense().getRushingStat().get(0)
						.setRushingTouchdown(0);
				params.getPlay().getPlayResult().setPlayResultPoints(0);
			}
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format("ERROR: [%s] failed with %s.  Input = %s", ste[1].getMethodName(),
					e.toString(), params.getPlayRawText());
			LOG.log(Level.SEVERE, errorStr);
//			e.printStackTrace();
			throw new IllegalArgumentException(errorStr);
		}
	}
}
