package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class PlayerStatRushingPojo {
	private String playerName;
	private Integer rushingAttempt;
	private Integer rushingYard;
	private Integer rushingTouchdown;
	@JsonIgnore
	private Integer rushingLong;
	private Integer rushingFirstDown;
	private Integer rushingFumble;
	private Integer rushingFumbleLost;
	private PlayDirectionEnum rushingDirection;
	private Integer rushingSafety;
	private Integer rushingKneel;
	private Integer rushingTwoPointConversion;

	public PlayerStatRushingPojo() {

	}

	public PlayerStatRushingPojo(String name) {
		this.playerName = name;
		this.rushingAttempt = 1;
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

	/**
	 * @return the rushingFumble
	 */
	public Integer getRushingFumble() {
		return rushingFumble;
	}

	/**
	 * @param rushingFumble the rushingFumble to set
	 */
	public void setRushingFumble(Integer rushingFumble) {
		this.rushingFumble = rushingFumble;
	}

	/**
	 * @param rushingFumble the rushingFumble to set
	 */
	public void applyRushingFumble(boolean turnover) {
		this.rushingFumble = 1;
		if (turnover) {
			this.rushingFumbleLost = 1;
		} else {
			this.rushingFumbleLost = 0;
		}
	}

	public void applyNoRushingFumble() {
		this.rushingFumble = 0;
		this.rushingFumbleLost = 0;
	}

	/**
	 * @return the rushingFumbleLost
	 */
	public Integer getRushingFumbleLost() {
		return rushingFumbleLost;
	}

	/**
	 * @param rushingFumbleLost the rushingFumbleLost to set
	 */
	public void setRushingFumbleLost(Integer rushingFumbleLost) {
		this.rushingFumbleLost = rushingFumbleLost;
	}

	/**
	 * @return the rushingDirection
	 */
	public PlayDirectionEnum getRushingDirection() {
		return rushingDirection;
	}

	/**
	 * @param rushingDirection the rushingDirection to set
	 */
	public void setRushingDirection(PlayDirectionEnum rushingDirection) {
		this.rushingDirection = rushingDirection;
	}

	/**
	 * @return the rushingSafety
	 */
	public Integer getRushingSafety() {
		return rushingSafety;
	}

	/**
	 * @param rushingSafety the rushingSafety to set
	 */
	public void setRushingSafety(Integer rushingSafety) {
		this.rushingSafety = rushingSafety;
	}

	/**
	 * @return the rushingKneel
	 */
	public Integer getRushingKneel() {
		return rushingKneel;
	}

	/**
	 * @param rushingKneel the rushingKneel to set
	 */
	public void setRushingKneel(Integer rushingKneel) {
		this.rushingKneel = rushingKneel;
	}

	/**
	 * @return the rushingTwoPointConversion
	 */
	public Integer getRushingTwoPointConversion() {
		return rushingTwoPointConversion;
	}

	/**
	 * @param rushingTwoPointConversion the rushingTwoPointConversion to set
	 */
	public void setRushingTwoPointConversion(Integer rushingTwoPointConversion) {
		this.rushingTwoPointConversion = rushingTwoPointConversion;
	}
	
	
	
}
