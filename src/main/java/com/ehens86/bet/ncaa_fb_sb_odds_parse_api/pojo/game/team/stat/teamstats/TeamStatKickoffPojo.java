package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats;

import java.util.Objects;

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
		return Objects.hash(kickoff, kickoffReturnTouchdown, kickoffReturnYards, kickoffYards);
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
		return Objects.equals(kickoff, other.kickoff)
				&& Objects.equals(kickoffReturnTouchdown, other.kickoffReturnTouchdown)
				&& Objects.equals(kickoffReturnYards, other.kickoffReturnYards)
				&& Objects.equals(kickoffYards, other.kickoffYards);
	}

	@Override
	public String toString() {
		return "TeamStatKickoffPojo [kickoff=" + kickoff + ", kickoffYards=" + kickoffYards + ", kickoffReturnYards="
				+ kickoffReturnYards + ", kickoffReturnTouchdown=" + kickoffReturnTouchdown + "]";
	}
	
	
}
