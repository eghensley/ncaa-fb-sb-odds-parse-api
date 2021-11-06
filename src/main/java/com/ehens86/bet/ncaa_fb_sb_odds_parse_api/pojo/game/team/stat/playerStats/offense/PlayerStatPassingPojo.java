package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;

public class PlayerStatPassingPojo {
	private String playerName;
	private Integer passingCompletion;
	private Integer passingAttempt;
	private Integer passingInterception;
	private Integer passingInterceptionYard;
	private Integer passingInterceptionTouchdown;
	private Integer passingYard;
	private Integer passingTouchdown;
	@JsonIgnore
	private Integer passingLong;
	private Integer passingBreakup;
	private Integer passingFirstDown;
	private PlayDirectionEnum passingDirection;
	private Integer passingDrop;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Integer passingYardAfterCatch;
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Integer passingYardThrownTo;
	private Integer passingHurry;
	private Integer passingSack;
	private Integer passingFumble;
	private Integer passingFumbleLost;
	private Integer passingSackYard;
	private Integer passingSafety;
	private Integer passingSpike;
private Integer passingTwoPointConversion;

	public PlayerStatPassingPojo() {

	}

	public PlayerStatPassingPojo(String name) {
		this.playerName = name;
		this.passingAttempt = 1;
	}

	public PlayerStatPassingPojo(String playerName, Integer passingCompletion, Integer passingAttempt,
			Integer passingInterception, Integer passingInterceptionYard, Integer passingInterceptionTouchdown,
			Integer passingYard, Integer passingTouchdown, Integer passingLong, Integer passingBreakup,
			Integer passingFirstDown) {
		super();
		this.playerName = playerName;
		this.passingCompletion = passingCompletion;
		this.passingAttempt = passingAttempt;
		this.passingInterception = passingInterception;
		this.passingInterceptionYard = passingInterceptionYard;
		this.passingInterceptionTouchdown = passingInterceptionTouchdown;
		this.passingYard = passingYard;
		this.passingTouchdown = passingTouchdown;
		this.passingLong = passingLong;
		this.passingBreakup = passingBreakup;
		this.passingFirstDown = passingFirstDown;
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
	 * @return the passingLong
	 */
	public Integer getPassingLong() {
		return passingLong;
	}

	/**
	 * @param passingLong the passingLong to set
	 */
	public void setPassingLong(Integer passingLong) {
		this.passingLong = passingLong;
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

	/**
	 * @return the passingBreakup
	 */
	public Integer getPassingBreakup() {
		return passingBreakup;
	}

	/**
	 * @param passingBreakup the passingBreakup to set
	 */
	public void setPassingBreakup(Integer passingBreakup) {
		this.passingBreakup = passingBreakup;
	}

	/**
	 * @return the passingInterceptionYard
	 */
	public Integer getPassingInterceptionYard() {
		return passingInterceptionYard;
	}

	/**
	 * @param passingInterceptionYard the passingInterceptionYard to set
	 */
	public void setPassingInterceptionYard(Integer passingInterceptionYard) {
		this.passingInterceptionYard = passingInterceptionYard;
	}

	/**
	 * @return the passingInterceptionTouchdown
	 */
	public Integer getPassingInterceptionTouchdown() {
		return passingInterceptionTouchdown;
	}

	/**
	 * @param passingInterceptionTouchdown the passingInterceptionTouchdown to set
	 */
	public void setPassingInterceptionTouchdown(Integer passingInterceptionTouchdown) {
		this.passingInterceptionTouchdown = passingInterceptionTouchdown;
	}

	/**
	 * @return the passingFirstDown
	 */
	public Integer getPassingFirstDown() {
		return passingFirstDown;
	}

	/**
	 * @param passingFirstDown the passingFirstDown to set
	 */
	public void setPassingFirstDown(Integer passingFirstDown) {
		this.passingFirstDown = passingFirstDown;
	}

	/**
	 * @return the passingDirection
	 */
	public PlayDirectionEnum getPassingDirection() {
		return passingDirection;
	}

	/**
	 * @param passingDirection the passingDirection to set
	 */
	public void setPassingDirection(PlayDirectionEnum passingDirection) {
		this.passingDirection = passingDirection;
	}

	/**
	 * @return the passingDrop
	 */
	public Integer getPassingDrop() {
		return passingDrop;
	}

	/**
	 * @param passingDrop the passingDrop to set
	 */
	public void setPassingDrop(Integer passingDrop) {
		this.passingDrop = passingDrop;
	}

	/**
	 * @return the passingYardAfterCatch
	 */
	public Integer getPassingYardAfterCatch() {
		return passingYardAfterCatch;
	}

	/**
	 * @param passingYardAfterCatch the passingYardAfterCatch to set
	 */
	public void setPassingYardAfterCatch(Integer passingYardAfterCatch) {
		this.passingYardAfterCatch = passingYardAfterCatch;
	}

	/**
	 * @return the passingYardThrownTo
	 */
	public Integer getPassingYardThrownTo() {
		return passingYardThrownTo;
	}

	/**
	 * @param passingYardThrownTo the passingYardThrownTo to set
	 */
	public void setPassingYardThrownTo(Integer passingYardThrownTo) {
		this.passingYardThrownTo = passingYardThrownTo;
	}

	/**
	 * @return the passingHurry
	 */
	public Integer getPassingHurry() {
		return passingHurry;
	}

	/**
	 * @param passingHurry the passingHurry to set
	 */
	public void setPassingHurry(Integer passingHurry) {
		this.passingHurry = passingHurry;
	}

	/**
	 * @return the passingSack
	 */
	public Integer getPassingSack() {
		return passingSack;
	}

	/**
	 * @param passingSack the passingSack to set
	 */
	public void setPassingSack(Integer passingSack) {
		this.passingSack = passingSack;
	}

	/**
	 * @return the passingFumble
	 */
	public Integer getPassingFumble() {
		return passingFumble;
	}

	/**
	 * @param passingFumble the passingFumble to set
	 */
	public void setPassingFumble(Integer passingFumble) {
		this.passingFumble = passingFumble;
	}

	/**
	 * @return the passingFumbleLost
	 */
	public Integer getPassingFumbleLost() {
		return passingFumbleLost;
	}

	/**
	 * @param passingFumbleLost the passingFumbleLost to set
	 */
	public void setPassingFumbleLost(Integer passingFumbleLost) {
		this.passingFumbleLost = passingFumbleLost;
	}

	/**
	 * @param passingFumble the rushingFumble to set
	 */
	public void applyPassingFumble(boolean turnover) {
		this.passingFumble = 1;
		if (turnover) {
			this.passingFumbleLost = 1;
		} else {
			this.passingFumbleLost = 0;
		}
	}

	public void applyNoPassingFumble() {
		this.passingFumble = 0;
		this.passingFumbleLost = 0;
	}

	/**
	 * @return the passingSackYard
	 */
	public Integer getPassingSackYard() {
		return passingSackYard;
	}

	/**
	 * @param passingSackYard the passingSackYard to set
	 */
	public void setPassingSackYard(Integer passingSackYard) {
		this.passingSackYard = passingSackYard;
	}

	/**
	 * @return the passingSafety
	 */
	public Integer getPassingSafety() {
		return passingSafety;
	}

	/**
	 * @param passingSafety the passingSafety to set
	 */
	public void setPassingSafety(Integer passingSafety) {
		this.passingSafety = passingSafety;
	}

	/**
	 * @return the passingSpike
	 */
	public Integer getPassingSpike() {
		return passingSpike;
	}

	/**
	 * @param passingSpike the passingSpike to set
	 */
	public void setPassingSpike(Integer passingSpike) {
		this.passingSpike = passingSpike;
	}

	/**
	 * @return the passingTwoPointConversion
	 */
	public Integer getPassingTwoPointConversion() {
		return passingTwoPointConversion;
	}

	/**
	 * @param passingTwoPointConversion the passingTwoPointConversion to set
	 */
	public void setPassingTwoPointConversion(Integer passingTwoPointConversion) {
		this.passingTwoPointConversion = passingTwoPointConversion;
	}

}
