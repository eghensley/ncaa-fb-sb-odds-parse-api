package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatKickingPojo;

public class PlayerStatKickingPojo extends BaseStatKickingPojo {
	protected Integer fieldGoalLong;

	public PlayerStatKickingPojo() {

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

	@Override
	public int hashCode() {
		return Objects.hash(extraPoint, fieldGoal, fieldGoalAttempt, fieldGoalLong, playerName, totalPoint);
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
		return Objects.equals(extraPoint, other.extraPoint) && Objects.equals(fieldGoal, other.fieldGoal)
				&& Objects.equals(fieldGoalAttempt, other.fieldGoalAttempt)
				&& Objects.equals(fieldGoalLong, other.fieldGoalLong) && Objects.equals(playerName, other.playerName)
				&& Objects.equals(totalPoint, other.totalPoint);
	}

	@Override
	public String toString() {
		return "PlayerStatKickingPojo [playerName=" + playerName + ", fieldGoal=" + fieldGoal + ", fieldGoalAttempt="
				+ fieldGoalAttempt + ", fieldGoalLong=" + fieldGoalLong + ", extraPoint=" + extraPoint + ", totalPoint="
				+ totalPoint + "]";
	}
	
	



	
}
