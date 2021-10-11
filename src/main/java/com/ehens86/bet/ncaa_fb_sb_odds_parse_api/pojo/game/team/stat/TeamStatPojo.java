package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatDefensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatOffensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatSpecialTeamPojo;

public class TeamStatPojo {
	private TeamStatOffensePojo offense;
	private TeamStatDefensePojo defense;
	private TeamStatPenaltyPojo penalty;
	private TeamStatSpecialTeamPojo specialTeam;
	
	public TeamStatPojo() {
		this.offense = new TeamStatOffensePojo();
		this.defense = new TeamStatDefensePojo();
		this.penalty = new TeamStatPenaltyPojo();
		this.specialTeam = new TeamStatSpecialTeamPojo();
	}

	public TeamStatPojo(TeamStatOffensePojo offense, TeamStatDefensePojo defense, TeamStatPenaltyPojo penalty,
			TeamStatSpecialTeamPojo specialTeam) {
		super();
		this.offense = offense;
		this.defense = defense;
		this.penalty = penalty;
		this.specialTeam = specialTeam;
	}

	/**
	 * @return the offense
	 */
	public TeamStatOffensePojo getOffense() {
		return offense;
	}

	/**
	 * @param offense the offense to set
	 */
	public void setOffense(TeamStatOffensePojo offense) {
		this.offense = offense;
	}

	/**
	 * @return the defense
	 */
	public TeamStatDefensePojo getDefense() {
		return defense;
	}

	/**
	 * @param defense the defense to set
	 */
	public void setDefense(TeamStatDefensePojo defense) {
		this.defense = defense;
	}

	/**
	 * @return the penalty
	 */
	public TeamStatPenaltyPojo getPenalty() {
		return penalty;
	}

	/**
	 * @param penalty the penalty to set
	 */
	public void setPenalty(TeamStatPenaltyPojo penalty) {
		this.penalty = penalty;
	}

	/**
	 * @return the specialTeam
	 */
	public TeamStatSpecialTeamPojo getSpecialTeam() {
		return specialTeam;
	}

	/**
	 * @param specialTeam the specialTeam to set
	 */
	public void setSpecialTeam(TeamStatSpecialTeamPojo specialTeam) {
		this.specialTeam = specialTeam;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((defense == null) ? 0 : defense.hashCode());
		result = prime * result + ((offense == null) ? 0 : offense.hashCode());
		result = prime * result + ((penalty == null) ? 0 : penalty.hashCode());
		result = prime * result + ((specialTeam == null) ? 0 : specialTeam.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatPojo)) {
			return false;
		}
		TeamStatPojo other = (TeamStatPojo) obj;
		if (defense == null) {
			if (other.defense != null) {
				return false;
			}
		} else if (!defense.equals(other.defense)) {
			return false;
		}
		if (offense == null) {
			if (other.offense != null) {
				return false;
			}
		} else if (!offense.equals(other.offense)) {
			return false;
		}
		if (penalty == null) {
			if (other.penalty != null) {
				return false;
			}
		} else if (!penalty.equals(other.penalty)) {
			return false;
		}
		if (specialTeam == null) {
			if (other.specialTeam != null) {
				return false;
			}
		} else if (!specialTeam.equals(other.specialTeam)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatPojo [offense=" + offense + ", defense=" + defense + ", penalty=" + penalty + ", specialTeam="
				+ specialTeam + "]";
	}
	
	
}
