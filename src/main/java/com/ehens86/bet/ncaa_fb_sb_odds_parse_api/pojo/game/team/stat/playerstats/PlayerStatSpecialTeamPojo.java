package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.player.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.PlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player.PlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player.PlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player.PlayerStatPuntingPojo;

public class PlayerStatSpecialTeamPojo {
	private static final Logger LOG = Logger.getLogger(PlayerStatSpecialTeamPojo.class.toString());

	private List<PlayerStatKickingPojo> kicking;
	private List<PlayerStatPuntingPojo> punting;
	private List<PlayerStatPuntReturnPojo> puntReturn;
	private List<PlayerStatKickReturnPojo> kickReturn;
	private List<PlayerStatDefenseProductionPojo> kickCoverage;
	private List<PlayerStatDefenseProductionPojo> puntCoverage;
	private List<PlayerStatKickoffPojo> kickoff;

	public PlayerStatSpecialTeamPojo() {
		this.kicking = new ArrayList<>();
		this.punting = new ArrayList<>();
		this.puntReturn = new ArrayList<>();
		this.kickReturn = new ArrayList<>();
		this.kickCoverage = new ArrayList<>();
		this.kickoff = new ArrayList<>();
		this.puntCoverage = new ArrayList<>();
	}

	public PlayerStatSpecialTeamPojo(List<PlayerStatKickingPojo> kicking, List<PlayerStatPuntingPojo> punting,
			List<PlayerStatPuntReturnPojo> puntReturn, List<PlayerStatKickReturnPojo> kickReturn,
			List<PlayerStatDefenseProductionPojo> kickCoverage, List<PlayerStatKickoffPojo> kickoff) {
		super();
		this.kicking = kicking;
		this.punting = punting;
		this.puntReturn = puntReturn;
		this.kickReturn = kickReturn;
		this.kickCoverage = kickCoverage;
		this.kickoff = kickoff;
	}

	/**
	 * @return the kicking
	 */
	public List<PlayerStatKickingPojo> getKicking() {
		return kicking;
	}

	/**
	 * @param kicking the kicking to set
	 */
	public void setKicking(List<PlayerStatKickingPojo> kicking) {
		this.kicking = kicking;
	}

	/**
	 * @return the punting
	 */
	public List<PlayerStatPuntingPojo> getPunting() {
		return punting;
	}

	/**
	 * @param punting the punting to set
	 */
	public void setPunting(List<PlayerStatPuntingPojo> punting) {
		this.punting = punting;
	}

	/**
	 * @return the puntReturn
	 */
	public List<PlayerStatPuntReturnPojo> getPuntReturn() {
		return puntReturn;
	}

	/**
	 * @param puntReturn the puntReturn to set
	 */
	public void setPuntReturn(List<PlayerStatPuntReturnPojo> puntReturn) {
		this.puntReturn = puntReturn;
	}

	/**
	 * @return the kickReturn
	 */
	public List<PlayerStatKickReturnPojo> getKickReturn() {
		return kickReturn;
	}

	/**
	 * @param kickReturn the kickReturn to set
	 */
	public void setKickReturn(List<PlayerStatKickReturnPojo> kickReturn) {
		this.kickReturn = kickReturn;
	}

	/**
	 * @return the puntCoverage
	 */
	public List<PlayerStatDefenseProductionPojo> getPuntCoverage() {
		return puntCoverage;
	}

	/**
	 * @param puntCoverage the puntCoverage to set
	 */
	public void setPuntCoverage(List<PlayerStatDefenseProductionPojo> puntCoverage) {
		this.puntCoverage = puntCoverage;
	}

	public PlayerStatPuntReturnPojo findPuntReturnByPlayerName(String rawPlayerName) {

		try {
			String playerName;
			if (rawPlayerName.contains(",")) {
				String[] rawPlayerNameSplit = rawPlayerName.split(",");
				playerName = String.format("%s %s", rawPlayerNameSplit[1], rawPlayerNameSplit[0]);
			} else {
				playerName = rawPlayerName;
			}

			Integer bestMatchScore = 1000;
			PlayerStatPuntReturnPojo bestMatch = new PlayerStatPuntReturnPojo();
			for (PlayerStatPuntReturnPojo potentialMatch : this.puntReturn) {
				Integer potentialMatchScore = StringUtils
						.getLevenshteinDistance(potentialMatch.getPlayerName().toUpperCase(), playerName.toUpperCase());
				if (potentialMatchScore < bestMatchScore) {
					bestMatch = potentialMatch;
					bestMatchScore = potentialMatchScore;
				}
			}

			if (bestMatch.getPlayerName().isEmpty()) {
				throw new IllegalArgumentException(String.format("ERROR: No match found for player %s", playerName));
			}
			if (bestMatchScore > NcaaConstants.FUZZY_THRESHOLD) {
				throw new IllegalArgumentException(String.format(
						"ERROR: Match failed for %s - score of %s above confidence threshold.  Best match: %s",
						playerName, bestMatchScore, bestMatch.getPlayerName()));
			}
			String logInfoStr = String.format("INFO: Matched %s -> %s with score of %s", playerName,
					bestMatch.getPlayerName(), bestMatchScore);
			LOG.log(Level.INFO, logInfoStr);
			return bestMatch;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(NcaaConstants.ERROR_S_FAILED_WITH_S, ste[1].getMethodName(), e.toString());
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	public PlayerStatKickReturnPojo findKickoffReturnByPlayerName(String rawPlayerName) {

		try {
			String playerName;
			if (rawPlayerName.contains(",")) {
				String[] rawPlayerNameSplit = rawPlayerName.split(",");
				playerName = String.format("%s %s", rawPlayerNameSplit[1], rawPlayerNameSplit[0]);
			} else {
				playerName = rawPlayerName;
			}
			Integer bestMatchScore = 1000;
			PlayerStatKickReturnPojo bestMatch = new PlayerStatKickReturnPojo();
			for (PlayerStatKickReturnPojo potentialMatch : this.kickReturn) {
				Integer potentialMatchScore = StringUtils
						.getLevenshteinDistance(potentialMatch.getPlayerName().toUpperCase(), playerName.toUpperCase());
				if (potentialMatchScore < bestMatchScore) {
					bestMatch = potentialMatch;
					bestMatchScore = potentialMatchScore;
				}
			}

			if ("".equals(bestMatch.getPlayerName())) {
				throw new IllegalArgumentException(String.format("ERROR: No match found for player %s", playerName));
			}
			if (bestMatchScore > NcaaConstants.FUZZY_THRESHOLD) {
				throw new IllegalArgumentException(String.format(
						"ERROR: Match failed for %s - score of %s above confidence threshold.  Best match: %s",
						playerName, bestMatchScore, bestMatch.getPlayerName()));
			}
			String logInfoStr = String.format("INFO: Matched %s -> %s with score of %s", playerName,
					bestMatch.getPlayerName(), bestMatchScore);
			LOG.log(Level.INFO, logInfoStr);
			return bestMatch;
		} catch (Exception e) {
			final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
			String errorStr = String.format(NcaaConstants.ERROR_S_FAILED_WITH_S, ste[1].getMethodName(), e.toString());
			LOG.log(Level.SEVERE, errorStr);
			LOG.log(Level.INFO, e.getMessage(), e);
			throw new IllegalArgumentException(errorStr);
		}
	}

	/**
	 * @return the kickCoverage
	 */
	public List<PlayerStatDefenseProductionPojo> getKickCoverage() {
		return kickCoverage;
	}

	/**
	 * @param kickCoverage the kickCoverage to set
	 */
	public void setKickCoverage(List<PlayerStatDefenseProductionPojo> kickCoverage) {
		this.kickCoverage = kickCoverage;
	}

	/**
	 * @return the kickoff
	 */
	public List<PlayerStatKickoffPojo> getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(List<PlayerStatKickoffPojo> kickoff) {
		this.kickoff = kickoff;
	}

	@Override
	public int hashCode() {
		return Objects.hash(kickCoverage, kickReturn, kicking, kickoff, puntCoverage, puntReturn, punting);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatSpecialTeamPojo)) {
			return false;
		}
		PlayerStatSpecialTeamPojo other = (PlayerStatSpecialTeamPojo) obj;
		return Objects.equals(kickCoverage, other.kickCoverage) && Objects.equals(kickReturn, other.kickReturn)
				&& Objects.equals(kicking, other.kicking) && Objects.equals(kickoff, other.kickoff)
				&& Objects.equals(puntCoverage, other.puntCoverage) && Objects.equals(puntReturn, other.puntReturn)
				&& Objects.equals(punting, other.punting);
	}

	@Override
	public String toString() {
		return "PlayerStatSpecialTeamPojo [kicking=" + kicking + ", punting=" + punting + ", puntReturn=" + puntReturn
				+ ", kickReturn=" + kickReturn + ", kickCoverage=" + kickCoverage + ", puntCoverage=" + puntCoverage
				+ ", kickoff=" + kickoff + "]";
	}

}
