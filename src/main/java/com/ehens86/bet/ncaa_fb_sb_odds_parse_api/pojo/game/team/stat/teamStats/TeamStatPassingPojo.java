package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

public class TeamStatPassingPojo {
	private Integer passingFirstDown;
	private Integer passingYard;
	private Integer passingAttempt;
	private Integer passingCompletion;
	private Integer passingTouchdown;
	private Integer passingInterception;
	private Integer passingInterceptionYard;
	private Integer passingInterceptionTouchdown;
	private Integer passingBreakUp;

	public TeamStatPassingPojo() {
		this.passingInterceptionTouchdown = 0;
		this.passingInterceptionYard = 0;
	}

	public TeamStatPassingPojo(Integer passingFirstDown, Integer passingYard, Integer passingAttempt,
			Integer passingCompletion, Integer passingTouchdown, Integer passingInterception,
			Integer passingInterceptionYard, Integer passingInterceptionTouchdown, Integer passingBreakUp) {
		super();
		this.passingFirstDown = passingFirstDown;
		this.passingYard = passingYard;
		this.passingAttempt = passingAttempt;
		this.passingCompletion = passingCompletion;
		this.passingTouchdown = passingTouchdown;
		this.passingInterception = passingInterception;
		this.passingInterceptionYard = passingInterceptionYard;
		this.passingInterceptionTouchdown = passingInterceptionTouchdown;
		this.passingBreakUp = passingBreakUp;
	}

	/**
	 * @return the passingFirstDown
	 */
	public Integer getPassingFirstDown() {
		return passingFirstDown;
	}

	/**
	 * @param passingFirstDown the passingFirstDown to set
	 */
	public void setPassingFirstDown(Integer passingFirstDown) {
		this.passingFirstDown = passingFirstDown;
	}

	/**
	 * @return the passingYard
	 */
	public Integer getPassingYard() {
		return passingYard;
	}

	/**
	 * @param passingYard the passingYard to set
	 */
	public void setPassingYard(Integer passingYard) {
		this.passingYard = passingYard;
	}

	/**
	 * @return the passingAttempt
	 */
	public Integer getPassingAttempt() {
		return passingAttempt;
	}

	/**
	 * @param passingAttempt the passingAttempt to set
	 */
	public void setPassingAttempt(Integer passingAttempt) {
		this.passingAttempt = passingAttempt;
	}

	/**
	 * @return the passingCompletion
	 */
	public Integer getPassingCompletion() {
		return passingCompletion;
	}

	/**
	 * @param passingCompletion the passingCompletion to set
	 */
	public void setPassingCompletion(Integer passingCompletion) {
		this.passingCompletion = passingCompletion;
	}

	/**
	 * @return the passingInterception
	 */
	public Integer getPassingInterception() {
		return passingInterception;
	}

	/**
	 * @param passingInterception the passingInterception to set
	 */
	public void setPassingInterception(Integer passingInterception) {
		this.passingInterception = passingInterception;
	}

	/**
	 * @return the passingInterceptionYard
	 */
	public Integer getPassingInterceptionYard() {
		return passingInterceptionYard;
	}

	/**
	 * @param passingInterceptionYard the passingInterceptionYard to set
	 */
	public void setPassingInterceptionYard(Integer passingInterceptionYard) {
		this.passingInterceptionYard = passingInterceptionYard;
	}

	/**
	 * @return the passingInterceptionTouchdown
	 */
	public Integer getPassingInterceptionTouchdown() {
		return passingInterceptionTouchdown;
	}

	/**
	 * @param passingInterceptionTouchdown the passingInterceptionTouchdown to set
	 */
	public void setPassingInterceptionTouchdown(Integer passingInterceptionTouchdown) {
		this.passingInterceptionTouchdown = passingInterceptionTouchdown;
	}

	/**
	 * @return the passingTouchdown
	 */
	public Integer getPassingTouchdown() {
		return passingTouchdown;
	}

	/**
	 * @param passingTouchdown the passingTouchdown to set
	 */
	public void setPassingTouchdown(Integer passingTouchdown) {
		this.passingTouchdown = passingTouchdown;
	}

	/**
	 * @param passingTouchdown the passingTouchdown to set
	 */
	public void addPassingInterceptionTouchdownAndYard(Integer passingInterceptionTouchdown,
			Integer passingInterceptionYard) {
		this.passingInterceptionTouchdown += passingInterceptionTouchdown;
		this.passingInterceptionYard += passingInterceptionYard;
	}

	/**
	 * @return the passingBreakUp
	 */
	public Integer getPassingBreakUp() {
		return passingBreakUp;
	}

	/**
	 * @param passingBreakUp the passingBreakUp to set
	 */
	public void setPassingBreakUp(Integer passingBreakUp) {
		this.passingBreakUp = passingBreakUp;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((passingAttempt == null) ? 0 : passingAttempt.hashCode());
		result = prime * result + ((passingBreakUp == null) ? 0 : passingBreakUp.hashCode());
		result = prime * result + ((passingCompletion == null) ? 0 : passingCompletion.hashCode());
		result = prime * result + ((passingFirstDown == null) ? 0 : passingFirstDown.hashCode());
		result = prime * result + ((passingInterception == null) ? 0 : passingInterception.hashCode());
		result = prime * result
				+ ((passingInterceptionTouchdown == null) ? 0 : passingInterceptionTouchdown.hashCode());
		result = prime * result + ((passingInterceptionYard == null) ? 0 : passingInterceptionYard.hashCode());
		result = prime * result + ((passingTouchdown == null) ? 0 : passingTouchdown.hashCode());
		result = prime * result + ((passingYard == null) ? 0 : passingYard.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatPassingPojo)) {
			return false;
		}
		TeamStatPassingPojo other = (TeamStatPassingPojo) obj;
		if (passingAttempt == null) {
			if (other.passingAttempt != null) {
				return false;
			}
		} else if (!passingAttempt.equals(other.passingAttempt)) {
			return false;
		}
		if (passingBreakUp == null) {
			if (other.passingBreakUp != null) {
				return false;
			}
		} else if (!passingBreakUp.equals(other.passingBreakUp)) {
			return false;
		}
		if (passingCompletion == null) {
			if (other.passingCompletion != null) {
				return false;
			}
		} else if (!passingCompletion.equals(other.passingCompletion)) {
			return false;
		}
		if (passingFirstDown == null) {
			if (other.passingFirstDown != null) {
				return false;
			}
		} else if (!passingFirstDown.equals(other.passingFirstDown)) {
			return false;
		}
		if (passingInterception == null) {
			if (other.passingInterception != null) {
				return false;
			}
		} else if (!passingInterception.equals(other.passingInterception)) {
			return false;
		}
		if (passingInterceptionTouchdown == null) {
			if (other.passingInterceptionTouchdown != null) {
				return false;
			}
		} else if (!passingInterceptionTouchdown.equals(other.passingInterceptionTouchdown)) {
			return false;
		}
		if (passingInterceptionYard == null) {
			if (other.passingInterceptionYard != null) {
				return false;
			}
		} else if (!passingInterceptionYard.equals(other.passingInterceptionYard)) {
			return false;
		}
		if (passingTouchdown == null) {
			if (other.passingTouchdown != null) {
				return false;
			}
		} else if (!passingTouchdown.equals(other.passingTouchdown)) {
			return false;
		}
		if (passingYard == null) {
			if (other.passingYard != null) {
				return false;
			}
		} else if (!passingYard.equals(other.passingYard)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatPassingPojo [passingFirstDown=" + passingFirstDown + ", passingYard=" + passingYard
				+ ", passingAttempt=" + passingAttempt + ", passingCompletion=" + passingCompletion
				+ ", passingTouchdown=" + passingTouchdown + ", passingInterception=" + passingInterception
				+ ", passingInterceptionYard=" + passingInterceptionYard + ", passingInterceptionTouchdown="
				+ passingInterceptionTouchdown + ", passingBreakUp=" + passingBreakUp + "]";
	}

}
