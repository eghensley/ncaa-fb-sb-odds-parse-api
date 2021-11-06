package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PenaltyEnum;

public class TeamStatPenaltyPojo {
	private Integer penalty;
	private Integer penaltyYards;
	private Integer penaltyFirstDown;
	private String playerName;
	private PenaltyEnum penaltyName;
	
	public TeamStatPenaltyPojo() {
		
	}

	public TeamStatPenaltyPojo(String name) {
		this.playerName = name;
		this.penalty = 1;
	}
	
	public TeamStatPenaltyPojo(Integer penalty, Integer penaltyYards) {
		super();
		this.penalty = penalty;
		this.penaltyYards = penaltyYards;
	}

	/**
	 * @return the penalty
	 */
	public Integer getPenalty() {
		return penalty;
	}

	/**
	 * @param penalty the penalty to set
	 */
	public void setPenalty(Integer penalty) {
		this.penalty = penalty;
	}

	/**
	 * @return the penaltyYards
	 */
	public Integer getPenaltyYards() {
		return penaltyYards;
	}

	/**
	 * @param penaltyYards the penaltyYards to set
	 */
	public void setPenaltyYards(Integer penaltyYards) {
		this.penaltyYards = penaltyYards;
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

}
