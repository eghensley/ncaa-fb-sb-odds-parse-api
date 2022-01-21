package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats.TeamStatDefensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats.TeamStatOffensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats.TeamStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats.TeamStatSpecialTeamPojo;

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
		return Objects.hash(defense, offense, penalty, specialTeam);
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
		return Objects.equals(defense, other.defense) && Objects.equals(offense, other.offense)
				&& Objects.equals(penalty, other.penalty) && Objects.equals(specialTeam, other.specialTeam);
	}

	@Override
	public String toString() {
		return "TeamStatPojo [offense=" + offense + ", defense=" + defense + ", penalty=" + penalty + ", specialTeam="
				+ specialTeam + "]";
	}
	
	
}
