package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.player.PlayerStatDefenseProductionPojo;

public class PlayerStatDefensePojo {
	private static final Logger LOG = Logger.getLogger(PlayerStatDefensePojo.class.toString());

	private List<PlayerStatDefenseProductionPojo> defenseProduction;

	public PlayerStatDefensePojo() {
		this.defenseProduction = new ArrayList<>();
	}

	public PlayerStatDefensePojo(List<PlayerStatDefenseProductionPojo> defenseProduction) {
		super();
		this.defenseProduction = defenseProduction;
	}

	/**
	 * @return the defenseProduction
	 */
	public List<PlayerStatDefenseProductionPojo> getDefenseProduction() {
		return defenseProduction;
	}

	/**
	 * @param defenseProduction the defenseProduction to set
	 */
	public void setDefenseProduction(List<PlayerStatDefenseProductionPojo> defenseProduction) {
		this.defenseProduction = defenseProduction;
	}

	public PlayerStatDefenseProductionPojo findByPlayerName(String rawPlayerName) {

		try {
			String playerName;
			if (rawPlayerName.contains(",")) {
				String[] rawPlayerNameSplit = rawPlayerName.split(",");
				playerName = String.format("%s %s", rawPlayerNameSplit[1], rawPlayerNameSplit[0]);
			} else {
				playerName = rawPlayerName;
			}
			Integer bestMatchScore = 1000;
			PlayerStatDefenseProductionPojo bestMatch = new PlayerStatDefenseProductionPojo();
			for (PlayerStatDefenseProductionPojo potentialMatch : this.defenseProduction) {
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
						"ERROR: Match failed for %s - score of %s above confidence threshold  Best match: %s",
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((defenseProduction == null) ? 0 : defenseProduction.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatDefensePojo)) {
			return false;
		}
		PlayerStatDefensePojo other = (PlayerStatDefensePojo) obj;
		if (defenseProduction == null) {
			if (other.defenseProduction != null) {
				return false;
			}
		} else if (!defenseProduction.equals(other.defenseProduction)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatDefensePojo [defenseProduction=" + defenseProduction + "]";
	}

}
