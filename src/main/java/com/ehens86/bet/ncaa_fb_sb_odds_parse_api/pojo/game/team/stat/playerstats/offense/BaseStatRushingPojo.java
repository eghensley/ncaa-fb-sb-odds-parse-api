package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense;

import java.util.Objects;

public class BaseStatRushingPojo {
	protected String playerName;
	protected Integer rushingAttempt;
	protected Integer rushingYard;
	protected Integer rushingTouchdown;

	public BaseStatRushingPojo() {

	}

	public BaseStatRushingPojo(String playerName, Integer rushingAttempt, Integer rushingYard,
			Integer rushingTouchdown) {
		super();
		this.playerName = playerName;
		this.rushingAttempt = rushingAttempt;
		this.rushingYard = rushingYard;
		this.rushingTouchdown = rushingTouchdown;
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
	 * @param rushingYard the rushingYard to set
	 */
	public void addRushingYard(Integer rushingYard) {
		this.rushingYard += rushingYard;
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
		return Objects.hash(playerName, rushingAttempt, rushingTouchdown, rushingYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseStatRushingPojo)) {
			return false;
		}
		BaseStatRushingPojo other = (BaseStatRushingPojo) obj;
		return Objects.equals(playerName, other.playerName) && Objects.equals(rushingAttempt, other.rushingAttempt)
				&& Objects.equals(rushingTouchdown, other.rushingTouchdown)
				&& Objects.equals(rushingYard, other.rushingYard);
	}

	@Override
	public String toString() {
		return "BaseStatRushingPojo [playerName=" + playerName + ", rushingAttempt=" + rushingAttempt + ", rushingYard="
				+ rushingYard + ", rushingTouchdown=" + rushingTouchdown + "]";
	}

}
