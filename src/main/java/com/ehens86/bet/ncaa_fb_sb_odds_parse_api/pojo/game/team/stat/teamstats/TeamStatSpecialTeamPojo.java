package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats;

import java.util.Objects;

public class TeamStatSpecialTeamPojo {
	private TeamStatPuntPojo punt;
	private TeamStatPuntPojo puntReturn;
	private TeamStatKickoffPojo kickoff;
	private TeamStatKickoffPojo kickoffReturn;
	
	public TeamStatSpecialTeamPojo() {
		this.punt = new TeamStatPuntPojo();
		this.puntReturn = new TeamStatPuntPojo();
		this.kickoff = new TeamStatKickoffPojo();
		this.kickoffReturn = new TeamStatKickoffPojo();
	}

	public TeamStatSpecialTeamPojo(TeamStatPuntPojo punt, TeamStatPuntPojo puntReturn, TeamStatKickoffPojo kickoff,
			TeamStatKickoffPojo kickoffReturn) {
		super();
		this.punt = punt;
		this.puntReturn = puntReturn;
		this.kickoff = kickoff;
		this.kickoffReturn = kickoffReturn;
	}

	/**
	 * @return the punt
	 */
	public TeamStatPuntPojo getPunt() {
		return punt;
	}

	/**
	 * @param punt the punt to set
	 */
	public void setPunt(TeamStatPuntPojo punt) {
		this.punt = punt;
	}

	/**
	 * @return the puntReturn
	 */
	public TeamStatPuntPojo getPuntReturn() {
		return puntReturn;
	}

	/**
	 * @param puntReturn the puntReturn to set
	 */
	public void setPuntReturn(TeamStatPuntPojo puntReturn) {
		this.puntReturn = puntReturn;
	}

	/**
	 * @return the kickoff
	 */
	public TeamStatKickoffPojo getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(TeamStatKickoffPojo kickoff) {
		this.kickoff = kickoff;
	}

	/**
	 * @return the kickoffReturn
	 */
	public TeamStatKickoffPojo getKickoffReturn() {
		return kickoffReturn;
	}

	/**
	 * @param kickoffReturn the kickoffReturn to set
	 */
	public void setKickoffReturn(TeamStatKickoffPojo kickoffReturn) {
		this.kickoffReturn = kickoffReturn;
	}

	@Override
	public int hashCode() {
		return Objects.hash(kickoff, kickoffReturn, punt, puntReturn);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatSpecialTeamPojo)) {
			return false;
		}
		TeamStatSpecialTeamPojo other = (TeamStatSpecialTeamPojo) obj;
		return Objects.equals(kickoff, other.kickoff) && Objects.equals(kickoffReturn, other.kickoffReturn)
				&& Objects.equals(punt, other.punt) && Objects.equals(puntReturn, other.puntReturn);
	}

	@Override
	public String toString() {
		return "TeamStatSpecialTeam [punt=" + punt + ", puntReturn=" + puntReturn + ", kickoff=" + kickoff
				+ ", kickoffReturn=" + kickoffReturn + "]";
	}
	
	
}
