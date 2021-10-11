package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams;

public class PlayerStatKickingPojo {
	private String playerName;
	private Integer fieldGoal;
	private Integer fieldGoalAttempt;
	private Integer fieldGoalLong;
	private Integer extraPoint;
	private Integer totalPoint;

	public PlayerStatKickingPojo() {

	}

	public PlayerStatKickingPojo(String playerName) {
		this.playerName = playerName;
//		this.fieldGoal = 0;
//		this.fieldGoalAttempt = 0;
//		this.fieldGoalLong = 0;
//		this.extraPoint = 0;
//		this.totalPoint = 0;
//		this.kickoffYard = 0;
//		this.kickoffTouchback = 0;
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
	 * @return the fieldGoal
	 */
	public Integer getFieldGoal() {
		return fieldGoal;
	}

	/**
	 * @param fieldGoal the fieldGoal to set
	 */
	public void setFieldGoal(Integer fieldGoal) {
		this.fieldGoal = fieldGoal;
	}

	/**
	 * @return the fieldGoalAttempt
	 */
	public Integer getFieldGoalAttempt() {
		return fieldGoalAttempt;
	}

	/**
	 * @param fieldGoalAttempt the fieldGoalAttempt to set
	 */
	public void setFieldGoalAttempt(Integer fieldGoalAttempt) {
		this.fieldGoalAttempt = fieldGoalAttempt;
	}

	/**
	 * @return the fieldGoalLong
	 */
	public Integer getFieldGoalLong() {
		return fieldGoalLong;
	}

	/**
	 * @param fieldGoalLong the fieldGoalLong to set
	 */
	public void setFieldGoalLong(Integer fieldGoalLong) {
		this.fieldGoalLong = fieldGoalLong;
	}

	/**
	 * @return the extraPoint
	 */
	public Integer getExtraPoint() {
		return extraPoint;
	}

	/**
	 * @param extraPoint the extraPoint to set
	 */
	public void setExtraPoint(Integer extraPoint) {
		this.extraPoint = extraPoint;
	}

	/**
	 * @return the totalPoint
	 */
	public Integer getTotalPoint() {
		return totalPoint;
	}

	/**
	 * @param totalPoint the totalPoint to set
	 */
	public void setTotalPoint(Integer totalPoint) {
		this.totalPoint = totalPoint;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((extraPoint == null) ? 0 : extraPoint.hashCode());
		result = prime * result + ((fieldGoal == null) ? 0 : fieldGoal.hashCode());
		result = prime * result + ((fieldGoalAttempt == null) ? 0 : fieldGoalAttempt.hashCode());
		result = prime * result + ((fieldGoalLong == null) ? 0 : fieldGoalLong.hashCode());
		result = prime * result + ((playerName == null) ? 0 : playerName.hashCode());
		result = prime * result + ((totalPoint == null) ? 0 : totalPoint.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatKickingPojo)) {
			return false;
		}
		PlayerStatKickingPojo other = (PlayerStatKickingPojo) obj;
		if (extraPoint == null) {
			if (other.extraPoint != null) {
				return false;
			}
		} else if (!extraPoint.equals(other.extraPoint)) {
			return false;
		}
		if (fieldGoal == null) {
			if (other.fieldGoal != null) {
				return false;
			}
		} else if (!fieldGoal.equals(other.fieldGoal)) {
			return false;
		}
		if (fieldGoalAttempt == null) {
			if (other.fieldGoalAttempt != null) {
				return false;
			}
		} else if (!fieldGoalAttempt.equals(other.fieldGoalAttempt)) {
			return false;
		}
		if (fieldGoalLong == null) {
			if (other.fieldGoalLong != null) {
				return false;
			}
		} else if (!fieldGoalLong.equals(other.fieldGoalLong)) {
			return false;
		}
		if (playerName == null) {
			if (other.playerName != null) {
				return false;
			}
		} else if (!playerName.equals(other.playerName)) {
			return false;
		}
		if (totalPoint == null) {
			if (other.totalPoint != null) {
				return false;
			}
		} else if (!totalPoint.equals(other.totalPoint)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatKickingPojo [playerName=" + playerName + ", fieldGoal=" + fieldGoal + ", fieldGoalAttempt="
				+ fieldGoalAttempt + ", fieldGoalLong=" + fieldGoalLong + ", extraPoint=" + extraPoint + ", totalPoint="
				+ totalPoint + "]";
	}


}
