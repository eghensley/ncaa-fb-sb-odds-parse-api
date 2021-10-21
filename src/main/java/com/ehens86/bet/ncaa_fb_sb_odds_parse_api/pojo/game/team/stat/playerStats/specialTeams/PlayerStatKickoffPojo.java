package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams;

public class PlayerStatKickoffPojo {
	private String playerName;
	private Integer kickoffYard;
	private Integer kickoffTouchback;
	private Integer kickoffOnsideAttempt;
	private Integer kickoffOnsideSuccess;
	private Integer kickoffReturnYard;
	private Integer kickoffOutOfBounds;
	private Integer kickoffReturnTouchdown;
	private Integer kickoffFairCatch;
	private Integer kickoff;
	private Integer kickoffLandYard;

	public PlayerStatKickoffPojo() {
		
	}
	
	public PlayerStatKickoffPojo(String playerName) {
		this.playerName = playerName;
		this.kickoff = 1;
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
	 * @return the kickoffYard
	 */
	public Integer getKickoffYard() {
		return kickoffYard;
	}

	/**
	 * @param kickoffYard the kickoffYard to set
	 */
	public void setKickoffYard(Integer kickoffYard) {
		this.kickoffYard = kickoffYard;
	}

	/**
	 * @return the kickoffTouchback
	 */
	public Integer getKickoffTouchback() {
		return kickoffTouchback;
	}

	/**
	 * @param kickoffTouchback the kickoffTouchback to set
	 */
	public void setKickoffTouchback(Integer kickoffTouchback) {
		this.kickoffTouchback = kickoffTouchback;
	}

	/**
	 * @return the kickoffOnsideAttempt
	 */
	public Integer getKickoffOnsideAttempt() {
		return kickoffOnsideAttempt;
	}

	/**
	 * @param kickoffOnsideAttempt the kickoffOnsideAttempt to set
	 */
	public void setKickoffOnsideAttempt(Integer kickoffOnsideAttempt) {
		this.kickoffOnsideAttempt = kickoffOnsideAttempt;
	}

	/**
	 * @return the kickoffOnsideSuccess
	 */
	public Integer getKickoffOnsideSuccess() {
		return kickoffOnsideSuccess;
	}

	/**
	 * @param kickoffOnsideSuccess the kickoffOnsideSuccess to set
	 */
	public void setKickoffOnsideSuccess(Integer kickoffOnsideSuccess) {
		this.kickoffOnsideSuccess = kickoffOnsideSuccess;
	}

	/**
	 * @return the kickoff
	 */
	public Integer getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(Integer kickoff) {
		this.kickoff = kickoff;
	}

	/**
	 * @return the kickoffReturnYard
	 */
	public Integer getKickoffReturnYard() {
		return kickoffReturnYard;
	}

	/**
	 * @param kickoffReturnYard the kickoffReturnYard to set
	 */
	public void setKickoffReturnYard(Integer kickoffReturnYard) {
		this.kickoffReturnYard = kickoffReturnYard;
	}

	/**
	 * @return the kickoffFairCatch
	 */
	public Integer getKickoffFairCatch() {
		return kickoffFairCatch;
	}

	/**
	 * @param kickoffFairCatch the kickoffFairCatch to set
	 */
	public void setKickoffFairCatch(Integer kickoffFairCatch) {
		this.kickoffFairCatch = kickoffFairCatch;
	}

	/**
	 * @return the kickoffOutOfBounds
	 */
	public Integer getKickoffOutOfBounds() {
		return kickoffOutOfBounds;
	}

	/**
	 * @param kickoffOutOfBounds the kickoffOutOfBounds to set
	 */
	public void setKickoffOutOfBounds(Integer kickoffOutOfBounds) {
		this.kickoffOutOfBounds = kickoffOutOfBounds;
	}

	/**
	 * @return the kickoffReturnTouchdown
	 */
	public Integer getKickoffReturnTouchdown() {
		return kickoffReturnTouchdown;
	}

	/**
	 * @param kickoffReturnTouchdown the kickoffReturnTouchdown to set
	 */
	public void setKickoffReturnTouchdown(Integer kickoffReturnTouchdown) {
		this.kickoffReturnTouchdown = kickoffReturnTouchdown;
	}
	
	
	/**
	 * @return the kickoffLandYard
	 */
	public Integer getKickoffLandYard() {
		return kickoffLandYard;
	}

	/**
	 * @param kickoffLandYard the kickoffLandYard to set
	 */
	public void setKickoffLandYard(Integer kickoffLandYard) {
		this.kickoffLandYard = kickoffLandYard;
	}

	public void applyTouchback() {
		this.kickoffTouchback = 1;
		this.kickoffFairCatch = 0;
		this.kickoffReturnTouchdown = 0;
		this.kickoffReturnYard = 25;
	}
	
	public void applyOutOfBounds() {
		this.kickoffTouchback = 0;
		this.kickoffFairCatch = 0;
		this.kickoffReturnTouchdown = 0;
		this.kickoffReturnYard = 35;
		this.kickoffOutOfBounds = 1;
	}
	
	public void applyFairCatch() {
		this.kickoffTouchback = 0;
		this.kickoffFairCatch = 1;
		this.kickoffOutOfBounds = 0;
		this.kickoffReturnYard = 25 - (100 - this.kickoffLandYard);
		this.kickoffReturnTouchdown = 0;
	}
}
