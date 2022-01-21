package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PenaltyEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats.TeamStatPenaltyPojo;

public class PlayerStatPenaltyPojo extends TeamStatPenaltyPojo {
	private Integer penaltyFirstDown;
	private String playerName;
	private PenaltyEnum penaltyName;
	
	public PlayerStatPenaltyPojo() {
		this.penalty = 1;
	}
	
	public PlayerStatPenaltyPojo(String name) {
		this.playerName = name;
		this.penalty = 1;
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
	 * @return the penaltyName
	 */
	public PenaltyEnum getPenaltyName() {
		return penaltyName;
	}

	/**
	 * @param penaltyName the penaltyName to set
	 */
	public void setPenaltyName(PenaltyEnum penaltyName) {
		this.penaltyName = penaltyName;
	}

	/**
	 * @return the penaltyFirstDown
	 */
	public Integer getPenaltyFirstDown() {
		return penaltyFirstDown;
	}

	/**
	 * @param penaltyFirstDown the penaltyFirstDown to set
	 */
	public void setPenaltyFirstDown(Integer penaltyFirstDown) {
		this.penaltyFirstDown = penaltyFirstDown;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(penaltyFirstDown, penaltyName, playerName);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (!(obj instanceof PlayerStatPenaltyPojo)) {
			return false;
		}
		PlayerStatPenaltyPojo other = (PlayerStatPenaltyPojo) obj;
		return Objects.equals(penaltyFirstDown, other.penaltyFirstDown) && penaltyName == other.penaltyName
				&& Objects.equals(playerName, other.playerName);
	}

	@Override
	public String toString() {
		return "PlayerStatPenaltyPojo [penaltyFirstDown=" + penaltyFirstDown + ", playerName=" + playerName
				+ ", penaltyName=" + penaltyName + "]";
	}
	
	
	
}
