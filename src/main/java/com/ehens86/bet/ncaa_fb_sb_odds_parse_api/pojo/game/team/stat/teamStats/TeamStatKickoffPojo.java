package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

public class TeamStatKickoffPojo {
	private Integer kickoff;
	private Integer kickoffYards;
	private Integer kickoffReturnYards;
	private Integer kickoffReturnTouchdown;
	
	public TeamStatKickoffPojo() {
		this.kickoffReturnTouchdown = 0;
	}

	public TeamStatKickoffPojo(Integer kickoff, Integer kickoffYards, Integer kickoffReturnYards,
			Integer kickoffReturnTouchdown) {
		super();
		this.kickoff = kickoff;
		this.kickoffYards = kickoffYards;
		this.kickoffReturnYards = kickoffReturnYards;
		this.kickoffReturnTouchdown = kickoffReturnTouchdown;
	}

	/**
	 * @return the kickoff
	 */
	public Integer getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(Integer kickoff) {
		this.kickoff = kickoff;
	}

	/**
	 * @return the kickoffYards
	 */
	public Integer getKickoffYards() {
		return kickoffYards;
	}

	/**
	 * @param kickoffYards the kickoffYards to set
	 */
	public void setKickoffYards(Integer kickoffYards) {
		this.kickoffYards = kickoffYards;
	}

	/**
	 * @return the kickoffReturnYards
	 */
	public Integer getKickoffReturnYards() {
		return kickoffReturnYards;
	}

	/**
	 * @param kickoffReturnYards the kickoffReturnYards to set
	 */
	public void setKickoffReturnYards(Integer kickoffReturnYards) {
		this.kickoffReturnYards = kickoffReturnYards;
	}

	/**
	 * @return the kickoffReturnTouchdown
	 */
	public Integer getKickoffReturnTouchdown() {
		return kickoffReturnTouchdown;
	}

	/**
	 * @param kickoffReturnTd the kickoffReturnTouchdown to set
	 */
	public void setKickoffReturnTouchdown(Integer kickoffReturnTouchdown) {
		this.kickoffReturnTouchdown = kickoffReturnTouchdown;
	}

	/**
	 * @param kickoffReturnTd the kickoffReturnTouchdown to set
	 */
	public void addKickoffReturnTouchdown(Integer kickoffReturnTouchdown) {
		this.kickoffReturnTouchdown += kickoffReturnTouchdown;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((kickoff == null) ? 0 : kickoff.hashCode());
		result = prime * result + ((kickoffReturnTouchdown == null) ? 0 : kickoffReturnTouchdown.hashCode());
		result = prime * result + ((kickoffReturnYards == null) ? 0 : kickoffReturnYards.hashCode());
		result = prime * result + ((kickoffYards == null) ? 0 : kickoffYards.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatKickoffPojo)) {
			return false;
		}
		TeamStatKickoffPojo other = (TeamStatKickoffPojo) obj;
		if (kickoff == null) {
			if (other.kickoff != null) {
				return false;
			}
		} else if (!kickoff.equals(other.kickoff)) {
			return false;
		}
		if (kickoffReturnTouchdown == null) {
			if (other.kickoffReturnTouchdown != null) {
				return false;
			}
		} else if (!kickoffReturnTouchdown.equals(other.kickoffReturnTouchdown)) {
			return false;
		}
		if (kickoffReturnYards == null) {
			if (other.kickoffReturnYards != null) {
				return false;
			}
		} else if (!kickoffReturnYards.equals(other.kickoffReturnYards)) {
			return false;
		}
		if (kickoffYards == null) {
			if (other.kickoffYards != null) {
				return false;
			}
		} else if (!kickoffYards.equals(other.kickoffYards)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatKickoffPojo [kickoff=" + kickoff + ", kickoffYards=" + kickoffYards + ", kickoffReturnYards="
				+ kickoffReturnYards + ", kickoffReturnTouchdown=" + kickoffReturnTouchdown + "]";
	}
	
	
}
