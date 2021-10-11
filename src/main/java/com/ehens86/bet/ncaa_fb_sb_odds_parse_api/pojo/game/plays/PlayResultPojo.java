package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

public class PlayResultPojo {
	private Integer playResultYard;
	private Integer playResultPoints;
	private Boolean playResultFirstDown;
	private Boolean playResultTurnover;
	private Integer playResultExpectedPointsAdded;
	
	public PlayResultPojo() {
		
	}

	/**
	 * @return the playResultYard
	 */
	public Integer getPlayResultYard() {
		return playResultYard;
	}

	/**
	 * @param playResultYard the playResultYard to set
	 */
	public void setPlayResultYard(Integer playResultYard) {
		this.playResultYard = playResultYard;
	}

	/**
	 * @return the playResultPoints
	 */
	public Integer getPlayResultPoints() {
		return playResultPoints;
	}

	/**
	 * @param playResultPoints the playResultPoints to set
	 */
	public void setPlayResultPoints(Integer playResultPoints) {
		this.playResultPoints = playResultPoints;
	}

	/**
	 * @return the playResultFirstDown
	 */
	public Boolean isPlayResultFirstDown() {
		return playResultFirstDown;
	}

	/**
	 * @param playResultFirstDown the playResultFirstDown to set
	 */
	public void setPlayResultFirstDown(Boolean playResultFirstDown) {
		this.playResultFirstDown = playResultFirstDown;
	}

	/**
	 * @return the playResultTurnover
	 */
	public Boolean isPlayResultTurnover() {
		return playResultTurnover;
	}

	/**
	 * @param playResultTurnover the playResultTurnover to set
	 */
	public void setPlayResultTurnover(Boolean playResultTurnover) {
		this.playResultTurnover = playResultTurnover;
	}

	/**
	 * @return the playResultExpectedPointsAdded
	 */
	public Integer getPlayResultExpectedPointsAdded() {
		return playResultExpectedPointsAdded;
	}

	/**
	 * @param playResultExpectedPointsAdded the playResultExpectedPointsAdded to set
	 */
	public void setPlayResultExpectedPointsAdded(Integer playResultExpectedPointsAdded) {
		this.playResultExpectedPointsAdded = playResultExpectedPointsAdded;
	}
	
	
}
