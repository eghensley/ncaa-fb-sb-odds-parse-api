package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnore;

public class PlayerStatPuntReturnPojo {
	private String playerName;
	private Integer puntReturn;
	private Integer puntReturnYard;
	@JsonIgnore
	private Integer puntReturnLong;
	private Integer puntReturnTouchdown;
	private Integer puntReturnFairCatch;
	private Integer puntReturnStartYard;
	private Integer puntReturnFumble;
	private Integer puntReturnFumbleLost;
	private Integer puntReturnBlock;
	private Integer puntReturnSafety;

	public PlayerStatPuntReturnPojo() {
		this.puntReturnTouchdown = 0;
	}

	public PlayerStatPuntReturnPojo(String playerName) {
		this.playerName = playerName;
		this.puntReturn = 1;
		this.puntReturnTouchdown = 0;
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
	 * @return the puntReturn
	 */
	public Integer getPuntReturn() {
		return puntReturn;
	}

	/**
	 * @param puntReturn the puntReturn to set
	 */
	public void setPuntReturn(Integer puntReturn) {
		this.puntReturn = puntReturn;
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

	/**
	 * @return the puntReturnLong
	 */
	public Integer getPuntReturnLong() {
		return puntReturnLong;
	}

	/**
	 * @param puntReturnLong the puntReturnLong to set
	 */
	public void setPuntReturnLong(Integer puntReturnLong) {
		this.puntReturnLong = puntReturnLong;
	}

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
	 * @param puntReturnTouchdown the puntReturnTouchdown to set
	 */
	public void addPuntReturnTouchdown(Integer puntReturnTouchdown) {
		this.puntReturnTouchdown += puntReturnTouchdown;
	}

	/**
	 * @return the puntReturnFairCatch
	 */
	public Integer getPuntReturnFairCatch() {
		return puntReturnFairCatch;
	}

	/**
	 * @param puntReturnFairCatch the puntReturnFairCatch to set
	 */
	public void setPuntReturnFairCatch(Integer puntReturnFairCatch) {
		this.puntReturnFairCatch = puntReturnFairCatch;
	}

	/**
	 * @return the puntReturnStartYard
	 */
	public Integer getPuntReturnStartYard() {
		return puntReturnStartYard;
	}

	/**
	 * @param puntReturnStartYard the puntReturnStartYard to set
	 */
	public void setPuntReturnStartYard(Integer puntReturnStartYard) {
		this.puntReturnStartYard = puntReturnStartYard;
	}

	/**
	 * @return the puntReturnFumble
	 */
	public Integer getPuntReturnFumble() {
		return puntReturnFumble;
	}

	/**
	 * @param puntReturnFumble the puntReturnFumble to set
	 */
	public void setPuntReturnFumble(Integer puntReturnFumble) {
		this.puntReturnFumble = puntReturnFumble;
	}

	/**
	 * @return the puntReturnBlock
	 */
	public Integer getPuntReturnBlock() {
		return puntReturnBlock;
	}

	/**
	 * @param puntReturnBlock the puntReturnBlock to set
	 */
	public void setPuntReturnBlock(Integer puntReturnBlock) {
		this.puntReturnBlock = puntReturnBlock;
	}

	public void applyReturnFairCatch() {
		this.puntReturnYard = 0;
		this.puntReturnFairCatch = 1;
	}

	public void applyReturnMuff() {
		this.puntReturnYard = 0;
		this.puntReturnFumble = 1;
	}

	/**
	 * @return the puntReturnFumbleLost
	 */
	public Integer getPuntReturnFumbleLost() {
		return puntReturnFumbleLost;
	}

	/**
	 * @param puntReturnFumbleLost the puntReturnFumbleLost to set
	 */
	public void setPuntReturnFumbleLost(Integer puntReturnFumbleLost) {
		this.puntReturnFumbleLost = puntReturnFumbleLost;
	}

	public void applyNoPuntReturnFumble() {
		this.puntReturnFumble = 0;
		this.puntReturnFumbleLost = 0;
	}

	public void applyPuntReturnFumble(boolean turnover) {
		this.puntReturnFumble = 1;
		if (turnover) {
			this.puntReturnFumbleLost = 1;
		} else {
			this.puntReturnFumbleLost = 0;
		}
	}

	public void applyBlockReturn(Integer puntReturnYard, Integer startYard) {
		this.puntReturnYard = puntReturnYard;
		if (Objects.isNull(this.puntReturnBlock)) {
			this.puntReturnBlock = 0;
		}
		this.puntReturnFairCatch = 0;
		this.puntReturnStartYard = startYard;
	}

	/**
	 * @return the puntReturnSafety
	 */
	public Integer getPuntReturnSafety() {
		return puntReturnSafety;
	}

	/**
	 * @param puntReturnSafety the puntReturnSafety to set
	 */
	public void setPuntReturnSafety(Integer puntReturnSafety) {
		this.puntReturnSafety = puntReturnSafety;
	}

}
