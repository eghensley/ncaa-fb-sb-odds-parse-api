package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense;

public class PlayerStatReceivingPojo {
	private String playerName;
	private Integer receivingReception;
	private Integer receivingYard;
	private Integer receivingTouchdown;
	private Integer receivingLong;
	private Integer receivingDrop;
	private Integer receivingYardAfterCatch;
	private Integer recievingFirstDown;

	public PlayerStatReceivingPojo() {

	}

	public PlayerStatReceivingPojo(String playerName, Integer receivingReception, Integer receivingYard,
			Integer receivingTouchdown, Integer receivingLong, Integer receivingDrop, Integer receivingYardAfterCatch,
			Integer recievingFirstDown) {
		super();
		this.playerName = playerName;
		this.receivingReception = receivingReception;
		this.receivingYard = receivingYard;
		this.receivingTouchdown = receivingTouchdown;
		this.receivingLong = receivingLong;
		this.receivingDrop = receivingDrop;
		this.receivingYardAfterCatch = receivingYardAfterCatch;
		this.recievingFirstDown = recievingFirstDown;
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
	 * @return the receivingReception
	 */
	public Integer getReceivingReception() {
		return receivingReception;
	}

	/**
	 * @param receivingReception the receivingReception to set
	 */
	public void setReceivingReception(Integer receivingReception) {
		this.receivingReception = receivingReception;
	}

	/**
	 * @return the receivingYard
	 */
	public Integer getReceivingYard() {
		return receivingYard;
	}

	/**
	 * @param receivingYard the receivingYard to set
	 */
	public void setReceivingYard(Integer receivingYard) {
		this.receivingYard = receivingYard;
	}

	/**
	 * @return the receivingTouchdown
	 */
	public Integer getReceivingTouchdown() {
		return receivingTouchdown;
	}

	/**
	 * @param receivingTouchdown the receivingTouchdown to set
	 */
	public void setReceivingTouchdown(Integer receivingTouchdown) {
		this.receivingTouchdown = receivingTouchdown;
	}

	/**
	 * @return the receivingLong
	 */
	public Integer getReceivingLong() {
		return receivingLong;
	}

	/**
	 * @param receivingLong the receivingLong to set
	 */
	public void setReceivingLong(Integer receivingLong) {
		this.receivingLong = receivingLong;
	}

	/**
	 * @return the receivingDrop
	 */
	public Integer getReceivingDrop() {
		return receivingDrop;
	}

	/**
	 * @param receivingDrop the receivingDrop to set
	 */
	public void setReceivingDrop(Integer receivingDrop) {
		this.receivingDrop = receivingDrop;
	}

	/**
	 * @return the receivingYardAfterCatch
	 */
	public Integer getReceivingYardAfterCatch() {
		return receivingYardAfterCatch;
	}

	/**
	 * @param receivingYardAfterCatch the receivingYardAfterCatch to set
	 */
	public void setReceivingYardAfterCatch(Integer receivingYardAfterCatch) {
		this.receivingYardAfterCatch = receivingYardAfterCatch;
	}

	/**
	 * @return the recievingFirstDown
	 */
	public Integer getRecievingFirstDown() {
		return recievingFirstDown;
	}

	/**
	 * @param recievingFirstDown the recievingFirstDown to set
	 */
	public void setRecievingFirstDown(Integer recievingFirstDown) {
		this.recievingFirstDown = recievingFirstDown;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((playerName == null) ? 0 : playerName.hashCode());
		result = prime * result + ((receivingDrop == null) ? 0 : receivingDrop.hashCode());
		result = prime * result + ((receivingLong == null) ? 0 : receivingLong.hashCode());
		result = prime * result + ((receivingReception == null) ? 0 : receivingReception.hashCode());
		result = prime * result + ((receivingTouchdown == null) ? 0 : receivingTouchdown.hashCode());
		result = prime * result + ((receivingYard == null) ? 0 : receivingYard.hashCode());
		result = prime * result + ((receivingYardAfterCatch == null) ? 0 : receivingYardAfterCatch.hashCode());
		result = prime * result + ((recievingFirstDown == null) ? 0 : recievingFirstDown.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatReceivingPojo)) {
			return false;
		}
		PlayerStatReceivingPojo other = (PlayerStatReceivingPojo) obj;
		if (playerName == null) {
			if (other.playerName != null) {
				return false;
			}
		} else if (!playerName.equals(other.playerName)) {
			return false;
		}
		if (receivingDrop == null) {
			if (other.receivingDrop != null) {
				return false;
			}
		} else if (!receivingDrop.equals(other.receivingDrop)) {
			return false;
		}
		if (receivingLong == null) {
			if (other.receivingLong != null) {
				return false;
			}
		} else if (!receivingLong.equals(other.receivingLong)) {
			return false;
		}
		if (receivingReception == null) {
			if (other.receivingReception != null) {
				return false;
			}
		} else if (!receivingReception.equals(other.receivingReception)) {
			return false;
		}
		if (receivingTouchdown == null) {
			if (other.receivingTouchdown != null) {
				return false;
			}
		} else if (!receivingTouchdown.equals(other.receivingTouchdown)) {
			return false;
		}
		if (receivingYard == null) {
			if (other.receivingYard != null) {
				return false;
			}
		} else if (!receivingYard.equals(other.receivingYard)) {
			return false;
		}
		if (receivingYardAfterCatch == null) {
			if (other.receivingYardAfterCatch != null) {
				return false;
			}
		} else if (!receivingYardAfterCatch.equals(other.receivingYardAfterCatch)) {
			return false;
		}
		if (recievingFirstDown == null) {
			if (other.recievingFirstDown != null) {
				return false;
			}
		} else if (!recievingFirstDown.equals(other.recievingFirstDown)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatReceivingPojo [playerName=" + playerName + ", receivingReception=" + receivingReception
				+ ", receivingYard=" + receivingYard + ", receivingTouchdown=" + receivingTouchdown + ", receivingLong="
				+ receivingLong + ", receivingDrop=" + receivingDrop + ", receivingYardAfterCatch="
				+ receivingYardAfterCatch + ", recievingFirstDown=" + recievingFirstDown + "]";
	}

}
