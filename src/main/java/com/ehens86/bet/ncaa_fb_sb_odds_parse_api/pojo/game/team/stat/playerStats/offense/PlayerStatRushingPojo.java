package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense;

public class PlayerStatRushingPojo {
	private String playerName;
	private Integer rushingAttempt;
	private Integer rushingYard;
	private Integer rushingTouchdown;
	private Integer rushingLong;
private Integer rushingFirstDown;

	public PlayerStatRushingPojo() {

	}



	public PlayerStatRushingPojo(String playerName, Integer rushingAttempt, Integer rushingYard,
			Integer rushingTouchdown, Integer rushingLong, Integer rushingFirstDown) {
		super();
		this.playerName = playerName;
		this.rushingAttempt = rushingAttempt;
		this.rushingYard = rushingYard;
		this.rushingTouchdown = rushingTouchdown;
		this.rushingLong = rushingLong;
		this.rushingFirstDown = rushingFirstDown;
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
	 * @return the rushingYard
	 */
	public Integer getRushingYard() {
		return rushingYard;
	}

	/**
	 * @param rushingYard the rushingYard to set
	 */
	public void setRushingYard(Integer rushingYard) {
		this.rushingYard = rushingYard;
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

	/**
	 * @return the rushingLong
	 */
	public Integer getRushingLong() {
		return rushingLong;
	}

	/**
	 * @param rushingLong the rushingLong to set
	 */
	public void setRushingLong(Integer rushingLong) {
		this.rushingLong = rushingLong;
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



	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((playerName == null) ? 0 : playerName.hashCode());
		result = prime * result + ((rushingAttempt == null) ? 0 : rushingAttempt.hashCode());
		result = prime * result + ((rushingFirstDown == null) ? 0 : rushingFirstDown.hashCode());
		result = prime * result + ((rushingLong == null) ? 0 : rushingLong.hashCode());
		result = prime * result + ((rushingTouchdown == null) ? 0 : rushingTouchdown.hashCode());
		result = prime * result + ((rushingYard == null) ? 0 : rushingYard.hashCode());
		return result;
	}



	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatRushingPojo)) {
			return false;
		}
		PlayerStatRushingPojo other = (PlayerStatRushingPojo) obj;
		if (playerName == null) {
			if (other.playerName != null) {
				return false;
			}
		} else if (!playerName.equals(other.playerName)) {
			return false;
		}
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
		if (rushingLong == null) {
			if (other.rushingLong != null) {
				return false;
			}
		} else if (!rushingLong.equals(other.rushingLong)) {
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
		return "PlayerStatRushingPojo [playerName=" + playerName + ", rushingAttempt=" + rushingAttempt
				+ ", rushingYard=" + rushingYard + ", rushingTouchdown=" + rushingTouchdown + ", rushingLong="
				+ rushingLong + ", rushingFirstDown=" + rushingFirstDown + "]";
	}



}
