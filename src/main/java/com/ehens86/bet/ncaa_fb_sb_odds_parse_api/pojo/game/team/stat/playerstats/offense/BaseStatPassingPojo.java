package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense;

import java.util.Objects;


public class BaseStatPassingPojo {
	protected String playerName;
	protected Integer passingCompletion;
	protected Integer passingAttempt;
	protected Integer passingInterception;
	protected Integer passingYard;
	protected Integer passingTouchdown;

	public BaseStatPassingPojo() {

	}

	public BaseStatPassingPojo(String playerName, Integer passingCompletion, Integer passingAttempt,
			Integer passingInterception, Integer passingYard, Integer passingTouchdown) {
		super();
		this.playerName = playerName;
		this.passingCompletion = passingCompletion;
		this.passingAttempt = passingAttempt;
		this.passingInterception = passingInterception;
		this.passingYard = passingYard;
		this.passingTouchdown = passingTouchdown;
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

	@Override
	public int hashCode() {
		return Objects.hash(passingAttempt, passingCompletion, passingInterception, passingTouchdown, passingYard,
				playerName);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseStatPassingPojo)) {
			return false;
		}
		BaseStatPassingPojo other = (BaseStatPassingPojo) obj;
		return Objects.equals(passingAttempt, other.passingAttempt)
				&& Objects.equals(passingCompletion, other.passingCompletion)
				&& Objects.equals(passingInterception, other.passingInterception)
				&& Objects.equals(passingTouchdown, other.passingTouchdown)
				&& Objects.equals(passingYard, other.passingYard) && Objects.equals(playerName, other.playerName);
	}

	@Override
	public String toString() {
		return "BaseStatPassingPojo [playerName=" + playerName + ", passingCompletion=" + passingCompletion
				+ ", passingAttempt=" + passingAttempt + ", passingInterception=" + passingInterception
				+ ", passingYard=" + passingYard + ", passingTouchdown=" + passingTouchdown + "]";
	}

}
