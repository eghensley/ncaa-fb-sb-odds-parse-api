package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams;

public class PlayerStatPuntingPojo {
	private String playerName;
	private Integer punt;
	private Integer puntYard;
	private Integer puntLong;
	private Integer puntBlocked;
	private Integer puntReturnYard;
//	private Integer puntOutOfBounds;
	private Integer puntReturnTouchdown;
	private Integer puntFairCatch;
	private Integer puntTouchback;
	private Integer puntLandYard;

	public PlayerStatPuntingPojo() {

	}

	/**
	 * @return the puntBlocked
	 */
	public Integer getPuntBlocked() {
		return puntBlocked;
	}

	/**
	 * @param puntBlocked the puntBlocked to set
	 */
	public void setPuntBlocked(Integer puntBlocked) {
		this.puntBlocked = puntBlocked;
	}

	/**
	 * @return the puntReturnYard
	 */
	public Integer getPuntReturnYard() {
		return puntReturnYard;
	}

	/**
	 * @param puntReturnYard the puntReturnYard to set
	 */
	public void setPuntReturnYard(Integer puntReturnYard) {
		this.puntReturnYard = puntReturnYard;
	}

//	/**
//	 * @return the puntOutOfBounds
//	 */
//	public Integer getPuntOutOfBounds() {
//		return puntOutOfBounds;
//	}
//
//	/**
//	 * @param puntOutOfBounds the puntOutOfBounds to set
//	 */
//	public void setPuntOutOfBounds(Integer puntOutOfBounds) {
//		this.puntOutOfBounds = puntOutOfBounds;
//	}

	/**
	 * @return the puntReturnTouchdown
	 */
	public Integer getPuntReturnTouchdown() {
		return puntReturnTouchdown;
	}

	/**
	 * @param puntReturnTouchdown the puntReturnTouchdown to set
	 */
	public void setPuntReturnTouchdown(Integer puntReturnTouchdown) {
		this.puntReturnTouchdown = puntReturnTouchdown;
	}

	/**
	 * @return the puntFairCatch
	 */
	public Integer getPuntFairCatch() {
		return puntFairCatch;
	}

	/**
	 * @param puntFairCatch the puntFairCatch to set
	 */
	public void setPuntFairCatch(Integer puntFairCatch) {
		this.puntFairCatch = puntFairCatch;
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
	 * @return the punt
	 */
	public Integer getPunt() {
		return punt;
	}

	/**
	 * @param punt the punt to set
	 */
	public void setPunt(Integer punt) {
		this.punt = punt;
	}

	/**
	 * @return the puntYard
	 */
	public Integer getPuntYard() {
		return puntYard;
	}

	/**
	 * @param puntYard the puntYard to set
	 */
	public void setPuntYard(Integer puntYard) {
		this.puntYard = puntYard;
	}

	/**
	 * @return the puntLong
	 */
	public Integer getPuntLong() {
		return puntLong;
	}

	/**
	 * @param puntLong the puntLong to set
	 */
	public void setPuntLong(Integer puntLong) {
		this.puntLong = puntLong;
	}

	/**
	 * @return the puntTouchback
	 */
	public Integer getPuntTouchback() {
		return puntTouchback;
	}

	/**
	 * @param puntTouchback the puntTouchback to set
	 */
	public void setPuntTouchback(Integer puntTouchback) {
		this.puntTouchback = puntTouchback;
	}
	
	public void applyPuntTouchback() {
		this.puntTouchback = 1;
		this.puntFairCatch = 0;
		//this.puntOutOfBounds = 0;
		this.puntReturnYard = 25;
		this.puntReturnTouchdown = 0;
		this.puntBlocked = 0;
	}
	
	public void applyPuntFairCatch() {
		this.puntTouchback = 0;
		this.puntFairCatch = 1;
		//this.puntOutOfBounds = 0;
		this.puntReturnYard = 0;
		this.puntReturnTouchdown = 0;
		this.puntBlocked = 0;
	}
	
	public void applyPuntReturn(Integer returnYard) {
		this.puntTouchback = 0;
		this.puntFairCatch = 0;
		//this.puntOutOfBounds = 0;
		this.puntReturnYard = returnYard;
		this.puntBlocked = 0;
	}

	/**
	 * @return the puntLandYard
	 */
	public Integer getPuntLandYard() {
		return puntLandYard;
	}

	/**
	 * @param puntLandYard the puntLandYard to set
	 */
	public void setPuntLandYard(Integer puntLandYard) {
		this.puntLandYard = puntLandYard;
	}

}
