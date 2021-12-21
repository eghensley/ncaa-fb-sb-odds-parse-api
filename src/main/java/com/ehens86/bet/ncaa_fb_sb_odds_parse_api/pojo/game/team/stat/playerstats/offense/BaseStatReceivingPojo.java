package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense;

import java.util.Objects;

public class BaseStatReceivingPojo {
	protected String playerName;
	protected Integer receivingReception;
	protected Integer receivingYard;
	protected Integer receivingTouchdown;

	public BaseStatReceivingPojo() {

	}

	public BaseStatReceivingPojo(String playerName, Integer receivingReception, Integer receivingYard,
			Integer receivingTouchdown) {
		super();
		this.playerName = playerName;
		this.receivingReception = receivingReception;
		this.receivingYard = receivingYard;
		this.receivingTouchdown = receivingTouchdown;
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

	@Override
	public int hashCode() {
		return Objects.hash(playerName, receivingReception, receivingTouchdown, receivingYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseStatReceivingPojo)) {
			return false;
		}
		BaseStatReceivingPojo other = (BaseStatReceivingPojo) obj;
		return Objects.equals(playerName, other.playerName)
				&& Objects.equals(receivingReception, other.receivingReception)
				&& Objects.equals(receivingTouchdown, other.receivingTouchdown)
				&& Objects.equals(receivingYard, other.receivingYard);
	}

	@Override
	public String toString() {
		return "BaseStatReceivingPojo [playerName=" + playerName + ", receivingReception=" + receivingReception
				+ ", receivingYard=" + receivingYard + ", receivingTouchdown=" + receivingTouchdown + "]";
	}

}
