package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((kickoff == null) ? 0 : kickoff.hashCode());
		result = prime * result + ((kickoffReturn == null) ? 0 : kickoffReturn.hashCode());
		result = prime * result + ((punt == null) ? 0 : punt.hashCode());
		result = prime * result + ((puntReturn == null) ? 0 : puntReturn.hashCode());
		return result;
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
		if (kickoff == null) {
			if (other.kickoff != null) {
				return false;
			}
		} else if (!kickoff.equals(other.kickoff)) {
			return false;
		}
		if (kickoffReturn == null) {
			if (other.kickoffReturn != null) {
				return false;
			}
		} else if (!kickoffReturn.equals(other.kickoffReturn)) {
			return false;
		}
		if (punt == null) {
			if (other.punt != null) {
				return false;
			}
		} else if (!punt.equals(other.punt)) {
			return false;
		}
		if (puntReturn == null) {
			if (other.puntReturn != null) {
				return false;
			}
		} else if (!puntReturn.equals(other.puntReturn)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatSpecialTeam [punt=" + punt + ", puntReturn=" + puntReturn + ", kickoff=" + kickoff
				+ ", kickoffReturn=" + kickoffReturn + "]";
	}
	
	
}
