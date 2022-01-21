package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats;

import java.util.Objects;

public class TeamStatRushingPojo {
	private Integer rushingFirstDown;
	private Integer rushingYard;
	private Integer rushingAttempt;
	private Integer rushingTouchdown;
	
	public TeamStatRushingPojo() {
		
	}

	public TeamStatRushingPojo(Integer rushingFirstDown, Integer rushingYard, Integer rushingAttempt,
			Integer rushingTouchdown) {
		super();
		this.rushingFirstDown = rushingFirstDown;
		this.rushingYard = rushingYard;
		this.rushingAttempt = rushingAttempt;
		this.rushingTouchdown = rushingTouchdown;
	}

	/**
	 * @return the rushingFirstDown
	 */
	public Integer getRushingFirstDown() {
		return rushingFirstDown;
	}

	/**
	 * @param rushingFirstDown the rushingFirstDown to set
	 */
	public void setRushingFirstDown(Integer rushingFirstDown) {
		this.rushingFirstDown = rushingFirstDown;
	}

	/**
	 * @return the rushingYard
	 */
	public Integer getRushingYard() {
		return rushingYard;
	}

	/**
	 * @param rushingYards the rushingYard to set
	 */
	public void setRushingYard(Integer rushingYard) {
		this.rushingYard = rushingYard;
	}

	/**
	 * @return the rushingAttempt
	 */
	public Integer getRushingAttempt() {
		return rushingAttempt;
	}

	/**
	 * @param rushingAttempt the rushingAttempt to set
	 */
	public void setRushingAttempt(Integer rushingAttempt) {
		this.rushingAttempt = rushingAttempt;
	}

	/**
	 * @return the rushingTouchdown
	 */
	public Integer getRushingTouchdown() {
		return rushingTouchdown;
	}

	/**
	 * @param rushingTouchdown the rushingTouchdown to set
	 */
	public void setRushingTouchdown(Integer rushingTouchdown) {
		this.rushingTouchdown = rushingTouchdown;
	}

	@Override
	public int hashCode() {
		return Objects.hash(rushingAttempt, rushingFirstDown, rushingTouchdown, rushingYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatRushingPojo)) {
			return false;
		}
		TeamStatRushingPojo other = (TeamStatRushingPojo) obj;
		return Objects.equals(rushingAttempt, other.rushingAttempt)
				&& Objects.equals(rushingFirstDown, other.rushingFirstDown)
				&& Objects.equals(rushingTouchdown, other.rushingTouchdown)
				&& Objects.equals(rushingYard, other.rushingYard);
	}

	@Override
	public String toString() {
		return "TeamStatRushingPojo [rushingFirstDown=" + rushingFirstDown + ", rushingYard=" + rushingYard
				+ ", rushingAttempt=" + rushingAttempt + ", rushingTouchdown=" + rushingTouchdown + "]";
	}


	
	
}
