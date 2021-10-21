package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.offense;

public class PlayerStatRushingPojo {
	private String playerName;
	private Integer rushingAttempt;
	private Integer rushingYard;
	private Integer rushingTouchdown;
	private Integer rushingLong;
	private Integer rushingFirstDown;
private Integer rushingFumble;
private Integer rushingFumbleLost;

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



}
