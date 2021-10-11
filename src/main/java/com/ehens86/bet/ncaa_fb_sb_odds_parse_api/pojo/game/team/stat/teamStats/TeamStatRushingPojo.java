package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((rushingAttempt == null) ? 0 : rushingAttempt.hashCode());
		result = prime * result + ((rushingFirstDown == null) ? 0 : rushingFirstDown.hashCode());
		result = prime * result + ((rushingTouchdown == null) ? 0 : rushingTouchdown.hashCode());
		result = prime * result + ((rushingYard == null) ? 0 : rushingYard.hashCode());
		return result;
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
		if (rushingAttempt == null) {
			if (other.rushingAttempt != null) {
				return false;
			}
		} else if (!rushingAttempt.equals(other.rushingAttempt)) {
			return false;
		}
		if (rushingFirstDown == null) {
			if (other.rushingFirstDown != null) {
				return false;
			}
		} else if (!rushingFirstDown.equals(other.rushingFirstDown)) {
			return false;
		}
		if (rushingTouchdown == null) {
			if (other.rushingTouchdown != null) {
				return false;
			}
		} else if (!rushingTouchdown.equals(other.rushingTouchdown)) {
			return false;
		}
		if (rushingYard == null) {
			if (other.rushingYard != null) {
				return false;
			}
		} else if (!rushingYard.equals(other.rushingYard)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatRushingPojo [rushingFirstDown=" + rushingFirstDown + ", rushingYard=" + rushingYard
				+ ", rushingAttempt=" + rushingAttempt + ", rushingTouchdown=" + rushingTouchdown + "]";
	}


	
	
}
