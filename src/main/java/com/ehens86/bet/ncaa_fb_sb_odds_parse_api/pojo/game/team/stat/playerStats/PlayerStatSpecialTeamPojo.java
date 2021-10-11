package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense.PlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams.PlayerStatPuntingPojo;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class PlayerStatSpecialTeamPojo {
	private static final Logger LOG = Logger.getLogger(PlayerStatSpecialTeamPojo.class.toString());
	@JsonIgnore
	private List<PlayerStatKickingPojo> kicking;
	private List<PlayerStatPuntingPojo> punting;
	private List<PlayerStatPuntReturnPojo> puntReturn;
	private List<PlayerStatKickReturnPojo> kickReturn;
	private List<PlayerStatDefenseProductionPojo> kickCoverage;
	private List<PlayerStatKickoffPojo> kickoff;

	public PlayerStatSpecialTeamPojo() {
		this.kicking = new ArrayList<PlayerStatKickingPojo>();
		this.punting = new ArrayList<PlayerStatPuntingPojo>();
		this.puntReturn = new ArrayList<PlayerStatPuntReturnPojo>();
		this.kickReturn = new ArrayList<PlayerStatKickReturnPojo>();
		this.kickCoverage = new ArrayList<PlayerStatDefenseProductionPojo>();
		this.kickoff = new ArrayList<PlayerStatKickoffPojo>();

	}


	public PlayerStatSpecialTeamPojo(List<PlayerStatKickingPojo> kicking,
			List<PlayerStatPuntingPojo> punting, List<PlayerStatPuntReturnPojo> puntReturn,
			List<PlayerStatKickReturnPojo> kickReturn, List<PlayerStatDefenseProductionPojo> kickCoverage,
			List<PlayerStatKickoffPojo> kickoff) {
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

			if ("".equals(bestMatch.getPlayerName())) {
				throw new IllegalArgumentException(String.format("ERROR: No match found for player %s", playerName));
			}
			if (bestMatchScore > NcaaConstants.fuzzyThreshold) {
				throw new IllegalArgumentException(String.format(
						"ERROR: Match failed for %s - score of %s above confidence threshold.  Best match: %s",
						playerName, bestMatchScore, bestMatch.getPlayerName()));
			}
			System.out.println(String.format("INFO: Matched %s -> %s with score of %s", playerName,
					bestMatch.getPlayerName(), bestMatchScore));
			return bestMatch;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			// e.printStackTrace();
			throw new IllegalArgumentException(e);
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
				Integer potentialMatchScore = StringUtils.getLevenshteinDistance(potentialMatch.getPlayerName().toUpperCase(),
						playerName.toUpperCase());
				if (potentialMatchScore < bestMatchScore) {
					bestMatch = potentialMatch;
					bestMatchScore = potentialMatchScore;
				}
			}

			if ("".equals(bestMatch.getPlayerName())) {
				throw new IllegalArgumentException(String.format("ERROR: No match found for player %s", playerName));
			}
			if (bestMatchScore > NcaaConstants.fuzzyThreshold) {
				throw new IllegalArgumentException(
						String.format("ERROR: Match failed for %s - score of %s above confidence threshold.  Best match: %s", playerName,
								bestMatchScore, bestMatch.getPlayerName()));
			}
			System.out.println(String.format("INFO: Matched %s -> %s with score of %s", playerName, bestMatch.getPlayerName(), bestMatchScore));
			return bestMatch;
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			//e.printStackTrace();
			throw new IllegalArgumentException(e);
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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((kickCoverage == null) ? 0 : kickCoverage.hashCode());
		result = prime * result + ((kickReturn == null) ? 0 : kickReturn.hashCode());
		result = prime * result + ((kicking == null) ? 0 : kicking.hashCode());
		result = prime * result + ((kickoff == null) ? 0 : kickoff.hashCode());
		result = prime * result + ((puntReturn == null) ? 0 : puntReturn.hashCode());
		result = prime * result + ((punting == null) ? 0 : punting.hashCode());
		return result;
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
		if (kickCoverage == null) {
			if (other.kickCoverage != null) {
				return false;
			}
		} else if (!kickCoverage.equals(other.kickCoverage)) {
			return false;
		}
		if (kickReturn == null) {
			if (other.kickReturn != null) {
				return false;
			}
		} else if (!kickReturn.equals(other.kickReturn)) {
			return false;
		}
		if (kicking == null) {
			if (other.kicking != null) {
				return false;
			}
		} else if (!kicking.equals(other.kicking)) {
			return false;
		}
		if (kickoff == null) {
			if (other.kickoff != null) {
				return false;
			}
		} else if (!kickoff.equals(other.kickoff)) {
			return false;
		}
		if (puntReturn == null) {
			if (other.puntReturn != null) {
				return false;
			}
		} else if (!puntReturn.equals(other.puntReturn)) {
			return false;
		}
		if (punting == null) {
			if (other.punting != null) {
				return false;
			}
		} else if (!punting.equals(other.punting)) {
			return false;
		}
		return true;
	}


	@Override
	public String toString() {
		return "PlayerStatSpecialTeamPojo [kicking=" + kicking + ", punting=" + punting + ", puntReturn=" + puntReturn
				+ ", kickReturn=" + kickReturn + ", kickCoverage=" + kickCoverage + ", kickoff=" + kickoff + "]";
	}



}
