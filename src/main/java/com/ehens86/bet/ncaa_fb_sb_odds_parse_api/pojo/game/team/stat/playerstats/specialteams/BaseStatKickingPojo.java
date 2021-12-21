package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams;

import java.util.Objects;

public class BaseStatKickingPojo {
	protected String playerName;
	protected Integer fieldGoal;
	protected Integer fieldGoalAttempt;
	protected Integer extraPoint;
	protected Integer totalPoint;

	public BaseStatKickingPojo() {

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
		return Objects.hash(extraPoint, fieldGoal, fieldGoalAttempt, playerName, totalPoint);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseStatKickingPojo)) {
			return false;
		}
		BaseStatKickingPojo other = (BaseStatKickingPojo) obj;
		return Objects.equals(extraPoint, other.extraPoint) && Objects.equals(fieldGoal, other.fieldGoal)
				&& Objects.equals(fieldGoalAttempt, other.fieldGoalAttempt)
				&& Objects.equals(playerName, other.playerName) && Objects.equals(totalPoint, other.totalPoint);
	}

	@Override
	public String toString() {
		return "BaseStatKickingPojo [playerName=" + playerName + ", fieldGoal=" + fieldGoal + ", fieldGoalAttempt="
				+ fieldGoalAttempt + ", extraPoint=" + extraPoint + ", totalPoint=" + totalPoint + "]";
	}
	
	
}
