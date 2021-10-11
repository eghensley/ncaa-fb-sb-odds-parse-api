package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense;

public class PlayerStatPassingPojo {
	private String playerName;
	private Integer passingCompletion;
	private Integer passingAttempt;
	private Integer passingInterception;
	private Integer passingInterceptionYard;
	private Integer passingInterceptionTouchdown;
	private Integer passingYard;
	private Integer passingTouchdown;
	private Integer passingLong;
	private Integer passingBreakup;
	private Integer passingFirstDown;

	public PlayerStatPassingPojo() {

	}

	public PlayerStatPassingPojo(String playerName, Integer passingCompletion, Integer passingAttempt,
			Integer passingInterception, Integer passingInterceptionYard, Integer passingInterceptionTouchdown,
			Integer passingYard, Integer passingTouchdown, Integer passingLong, Integer passingBreakup,
			Integer passingFirstDown) {
		super();
		this.playerName = playerName;
		this.passingCompletion = passingCompletion;
		this.passingAttempt = passingAttempt;
		this.passingInterception = passingInterception;
		this.passingInterceptionYard = passingInterceptionYard;
		this.passingInterceptionTouchdown = passingInterceptionTouchdown;
		this.passingYard = passingYard;
		this.passingTouchdown = passingTouchdown;
		this.passingLong = passingLong;
		this.passingBreakup = passingBreakup;
		this.passingFirstDown = passingFirstDown;
	}

	/**
	 * @return the playerName
	 */
	public String getPlayerName() {
		return playerName;
	}

	/**
	 * @param playerName the playerName to set
	 */
	public void setPlayerName(String playerName) {
		this.playerName = playerName;
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
	 * @return the passingLong
	 */
	public Integer getPassingLong() {
		return passingLong;
	}

	/**
	 * @param passingLong the passingLong to set
	 */
	public void setPassingLong(Integer passingLong) {
		this.passingLong = passingLong;
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
	 * @return the passingBreakup
	 */
	public Integer getPassingBreakup() {
		return passingBreakup;
	}

	/**
	 * @param passingBreakup the passingBreakup to set
	 */
	public void setPassingBreakup(Integer passingBreakup) {
		this.passingBreakup = passingBreakup;
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((passingAttempt == null) ? 0 : passingAttempt.hashCode());
		result = prime * result + ((passingBreakup == null) ? 0 : passingBreakup.hashCode());
		result = prime * result + ((passingCompletion == null) ? 0 : passingCompletion.hashCode());
		result = prime * result + ((passingFirstDown == null) ? 0 : passingFirstDown.hashCode());
		result = prime * result + ((passingInterception == null) ? 0 : passingInterception.hashCode());
		result = prime * result
				+ ((passingInterceptionTouchdown == null) ? 0 : passingInterceptionTouchdown.hashCode());
		result = prime * result + ((passingInterceptionYard == null) ? 0 : passingInterceptionYard.hashCode());
		result = prime * result + ((passingLong == null) ? 0 : passingLong.hashCode());
		result = prime * result + ((passingTouchdown == null) ? 0 : passingTouchdown.hashCode());
		result = prime * result + ((passingYard == null) ? 0 : passingYard.hashCode());
		result = prime * result + ((playerName == null) ? 0 : playerName.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatPassingPojo)) {
			return false;
		}
		PlayerStatPassingPojo other = (PlayerStatPassingPojo) obj;
		if (passingAttempt == null) {
			if (other.passingAttempt != null) {
				return false;
			}
		} else if (!passingAttempt.equals(other.passingAttempt)) {
			return false;
		}
		if (passingBreakup == null) {
			if (other.passingBreakup != null) {
				return false;
			}
		} else if (!passingBreakup.equals(other.passingBreakup)) {
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
		if (passingLong == null) {
			if (other.passingLong != null) {
				return false;
			}
		} else if (!passingLong.equals(other.passingLong)) {
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
		if (playerName == null) {
			if (other.playerName != null) {
				return false;
			}
		} else if (!playerName.equals(other.playerName)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatPassingPojo [playerName=" + playerName + ", passingCompletion=" + passingCompletion
				+ ", passingAttempt=" + passingAttempt + ", passingInterception=" + passingInterception
				+ ", passingInterceptionYard=" + passingInterceptionYard + ", passingInterceptionTouchdown="
				+ passingInterceptionTouchdown + ", passingYard=" + passingYard + ", passingTouchdown="
				+ passingTouchdown + ", passingLong=" + passingLong + ", passingBreakup=" + passingBreakup
				+ ", passingFirstDown=" + passingFirstDown + "]";
	}

}
