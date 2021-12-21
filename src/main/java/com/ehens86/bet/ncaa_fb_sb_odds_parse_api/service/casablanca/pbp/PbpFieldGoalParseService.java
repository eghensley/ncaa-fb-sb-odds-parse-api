package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.pbp;

import java.util.Objects;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.KickMissReasonEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.PbpServiceRequestPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.PbpParsingUtils;

@Service
public class PbpFieldGoalParseService {
	private final PbpParsingUtils pbpParsingUtils;
	private final LoggingUtils loggingUtils;

	public PbpFieldGoalParseService(PbpParsingUtils pbpParsingUtils, LoggingUtils loggingUtils) {
		this.pbpParsingUtils = pbpParsingUtils;
		this.loggingUtils = loggingUtils;
	}

	public boolean parseFieldGoal(PbpServiceRequestPojo params, boolean updated) {
		try {
			if (params.getPlayRaw().getScoreText().toUpperCase().contains("FIELD GOAL ATTEMPT")
					|| params.getPlayRawText().toUpperCase().contains(" PAT ")
					|| params.getPlayRawText().toUpperCase().contains(" KICK ATTEMPT ")) {
				if (params.getPlayRawText().toUpperCase().contains(" FIELD GOAL ")) {
					parseFieldGoalBasic(params);
				} else if (params.getPlayRawText().toUpperCase().contains(" PAT ")
						|| params.getPlayRawText().toUpperCase().contains(" KICK ATTEMPT")) {
					parsePatBasic(params);
				}
				parseFieldGoalBlocked(params);
				updated = true;
			}
			return updated;
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());
			throw new IllegalArgumentException(e.toString());
		}
	}

	private void parsePatBasic(PbpServiceRequestPojo params) {
		try {
			String patString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"%s (?:PAT )?kick attempt (?:is )?(?i)(GOOD|BLOCKED|MISSED|NO GOOD|FAILED \\(BLOCKED\\))(.*(((?:wide)|(?:short)) ((?:left)|(?:right)|(?:middle)))|.*(off crossbar)|.*(((?:left)|(?:right)) upright))?",
					NcaaConstants.PLAYER_NAME_REGEX), 15);
			String[] patStringArray = patString.split("\\|")[0].split("\\~");

			PbpPlayerStatKickingPojo patStat = new PbpPlayerStatKickingPojo(pbpParsingUtils.formatName(patStringArray[0]));

			patStat.setExtraPointYard(100 - params.getPlay().getPlayStartYard());
			patStat.setExtraPointAttempt(1);
			patStat.setFieldGoal(0);
			patStat.setFieldGoalBlock(0);
			patStat.setFieldGoalYard(0);
			patStat.setFieldGoalMiss(0);
			patStat.setFieldGoalAttempt(0);

			if ("MISSED".equalsIgnoreCase(patStringArray[7]) || "NO GOOD".equalsIgnoreCase(patStringArray[7])) {
				patStat.setExtraPointMiss(1);
				patStat.setExtraPointBlock(0);
				patStat.setExtraPoint(0);
				patStat.setTotalPoint(0);
			} else if ("GOOD".equalsIgnoreCase(patStringArray[7])) {
				patStat.setExtraPointMiss(0);
				patStat.setExtraPointBlock(0);
				patStat.setExtraPoint(1);
				patStat.setTotalPoint(1);
				params.getPlay().getPlayResult().setPlayResultPoints(1);
			} else if ("BLOCKED".equalsIgnoreCase(patStringArray[7])
					|| "FAILED (BLOCKED)".equalsIgnoreCase(patStringArray[7])) {
				patStat.setExtraPointMiss(0);
				patStat.setExtraPointBlock(1);
				patStat.setExtraPoint(0);
				patStat.setTotalPoint(0);
			} else {
				String logInfo = String.format("Play text: %s", params.getPlayRawText());
				loggingUtils.logInfo(logInfo);
				throw new IllegalArgumentException("PAT did not evaluate");
			}

			if ("WIDE".equalsIgnoreCase(patStringArray[10]) || !"null".equals(patStringArray[13])) {
				patStat.setKickMissReason(KickMissReasonEnum.WIDE);
				params.setPlayTackles(new String[0]);
			} else if ("SHORT".equalsIgnoreCase(patStringArray[10])
					|| "OFF CROSSBAR".equalsIgnoreCase(patStringArray[12])) {
				patStat.setKickMissReason(KickMissReasonEnum.SHORT);
				params.setPlayTackles(new String[0]);
			}

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking().add(patStat);
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseFieldGoalBasic(PbpServiceRequestPojo params) {
		try {
			String fgString = pbpParsingUtils.extractCustom(params.getPlayRawText(), String.format(
					"%s field goal attempt from (\\d{1,2})( yards?)? (GOOD|BLOCKED|MISSED|NO GOOD)(.*(((?:wide)|(?:short)) ((?:left)|(?:right)|(?:middle)))|.*(off crossbar)|.*(((?:left)|(?:right)) upright))?",
					NcaaConstants.PLAYER_NAME_REGEX), 15);
			String[] fgStringArray = fgString.split("\\|")[0].split("\\~");

			PbpPlayerStatKickingPojo fieldGoalStat = new PbpPlayerStatKickingPojo(
					pbpParsingUtils.formatName(fgStringArray[0]));

			Integer yards = Integer.valueOf(fgStringArray[7]);
			fieldGoalStat.setFieldGoalYard(yards);
			fieldGoalStat.setFieldGoalAttempt(1);
			fieldGoalStat.setExtraPoint(0);
			fieldGoalStat.setExtraPointBlock(0);
			fieldGoalStat.setExtraPointYard(0);
			fieldGoalStat.setExtraPointMiss(0);
			fieldGoalStat.setExtraPointAttempt(0);

			if ("MISSED".equals(fgStringArray[9]) || "NO GOOD".equals(fgStringArray[9])) {
				fieldGoalStat.setFieldGoalMiss(1);
				fieldGoalStat.setFieldGoalBlock(0);
				fieldGoalStat.setFieldGoal(0);
				fieldGoalStat.setTotalPoint(0);
				if (Objects.isNull(params.getPlay().getPlayResult().getPlayResultYardLine())) {
					params.getPlay().getPlayResult().setPlayResultYardLine(params.getPlay().getPlayStartYard());
				}
				params.getPlay().getPlayResult().setPlayResultYard(
						params.getPlay().getPlayResult().getPlayResultYardLine() - params.getPlay().getPlayStartYard());
			} else if ("GOOD".equals(fgStringArray[9])) {
				fieldGoalStat.setFieldGoalMiss(0);
				fieldGoalStat.setFieldGoalBlock(0);
				fieldGoalStat.setFieldGoal(1);
				fieldGoalStat.setTotalPoint(3);
				params.getPlay().getPlayResult().setPlayResultPoints(3);
				params.getPlay().getPlayResult().setPlayResultYardLine(params.getPlay().getPlayStartYard());
				params.getPlay().getPlayResult().setPlayResultYard(0);
			} else if ("BLOCKED".equals(fgStringArray[9])) {
				fieldGoalStat.setFieldGoalMiss(0);
				fieldGoalStat.setFieldGoalBlock(1);
				fieldGoalStat.setFieldGoal(0);
				fieldGoalStat.setTotalPoint(0);
			}

			if ("WIDE".equalsIgnoreCase(fgStringArray[12]) || !"null".equals(fgStringArray[10])) {
				fieldGoalStat.setKickMissReason(KickMissReasonEnum.WIDE);
				params.setPlayTackles(new String[0]);
			} else if ("SHORT".equalsIgnoreCase(fgStringArray[12])
					|| "OFF CROSSBAR".equalsIgnoreCase(fgStringArray[14])) {
				fieldGoalStat.setKickMissReason(KickMissReasonEnum.SHORT);
				params.setPlayTackles(new String[0]);
			}

			params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
					.add(fieldGoalStat);
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());

		}
	}

	private void parseFieldGoalBlocked(PbpServiceRequestPojo params) {
		try {
			if (params.getPlayRawText().toUpperCase().contains("BLOCK")) {
				String kickBlockString = pbpParsingUtils.extractCustom(params.getPlayRawText(),
						String.format("\\(?(?i)block(?:ed)? by %s\\)?", NcaaConstants.PLAYER_NAME_REGEX), 7);
				String kickBlockName = kickBlockString.split("\\~")[0];
				PbpPlayerStatDefenseProductionPojo blocker = new PbpPlayerStatDefenseProductionPojo();
				blocker.applyBase(pbpParsingUtils.formatName(kickBlockName));
				blocker.setKickBlock(1);

				if (params.getPlay().getPlayCallType() == PlayCallTypeEnum.FG) {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
							.get(0).setFieldGoalBlock(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
							.get(0).setFieldGoalMiss(0);
				} else {
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
							.get(0).setExtraPointBlock(1);
					params.getPlay().getPlayerStat().get(params.getPossessionTeam()).getSpecialTeam().getKicking()
							.get(0).setExtraPointMiss(0);
				}
				params.getPlay().getPlayerStat().get(params.getDefenseTeam()).getDefense().getDefenseProduction()
						.add(blocker);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, params.getPlayRawText());
		}
	}

}
