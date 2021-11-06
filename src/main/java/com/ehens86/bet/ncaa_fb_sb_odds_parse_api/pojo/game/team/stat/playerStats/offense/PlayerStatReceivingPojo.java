package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;

public class PlayerStatReceivingPojo {
	private String playerName;
	private Integer receivingReception;
	private Integer receivingTarget;
	private Integer receivingYard;
	private Integer receivingTouchdown;
	@JsonIgnore
	private Integer receivingLong;
	private Integer receivingDrop;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Integer receivingYardAfterCatch;
	private Integer recievingFirstDown;
	private Integer receivingFumble;
	private Integer receivingFumbleLost;
	private Integer receivingSafety;
	private Integer receivingTwoPointConversion;

	public PlayerStatReceivingPojo() {

	}

	public PlayerStatReceivingPojo(String name, Integer reception, Integer yards) {
		this.playerName = name;
		this.receivingTarget = 1;
		this.receivingReception = reception;
		this.receivingYard = yards;
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

	/**
	 * @return the receivingTarget
	 */
	public Integer getReceivingTarget() {
		return receivingTarget;
	}

	/**
	 * @param receivingTarget the receivingTarget to set
	 */
	public void setReceivingTarget(Integer receivingTarget) {
		this.receivingTarget = receivingTarget;
	}

	/**
	 * @return the receivingFumble
	 */
	public Integer getReceivingFumble() {
		return receivingFumble;
	}

	/**
	 * @param receivingFumble the receivingFumble to set
	 */
	public void setReceivingFumble(Integer receivingFumble) {
		this.receivingFumble = receivingFumble;
	}

	/**
	 * @return the receivingFumbleLost
	 */
	public Integer getReceivingFumbleLost() {
		return receivingFumbleLost;
	}

	/**
	 * @param receivingFumbleLost the receivingFumbleLost to set
	 */
	public void setReceivingFumbleLost(Integer receivingFumbleLost) {
		this.receivingFumbleLost = receivingFumbleLost;
	}

	/**
	 * @param receivingFumble the receivingFumble to set
	 */
	public void applyReceivingFumble(boolean turnover) {
		this.receivingFumble = 1;
		if (turnover) {
			this.receivingFumbleLost = 1;
		} else {
			this.receivingFumbleLost = 0;
		}
	}

	public void applyNoReceivingFumble() {
		this.receivingFumble = 0;
		this.receivingFumbleLost = 0;
	}

	/**
	 * @return the receivingSafety
	 */
	public Integer getReceivingSafety() {
		return receivingSafety;
	}

	/**
	 * @param receivingSafety the receivingSafety to set
	 */
	public void setReceivingSafety(Integer receivingSafety) {
		this.receivingSafety = receivingSafety;
	}

	/**
	 * @return the receivingTwoPointConversion
	 */
	public Integer getReceivingTwoPointConversion() {
		return receivingTwoPointConversion;
	}

	/**
	 * @param receivingTwoPointConversion the receivingTwoPointConversion to set
	 */
	public void setReceivingTwoPointConversion(Integer receivingTwoPointConversion) {
		this.receivingTwoPointConversion = receivingTwoPointConversion;
	}
	
	

}
