package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import com.fasterxml.jackson.annotation.JsonIgnore;

public class PlayResultPojo {
	@JsonIgnore
	private Integer playResultYard;
	private Integer playResultPoints;
	private Boolean playResultFirstDown;
	private Boolean playResultTurnover;
	@JsonIgnore
	private Integer playResultExpectedPointsAdded;
	private Integer playResultYardLine;
	
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
		if (this.playResultYard != null && this.playResultYard != 0 && playResultYard != 0) {
			if (this.playResultYard > 0 && playResultYard > 0) {
				this.playResultYard += playResultYard;
			} else if (this.playResultYard < 0 && playResultYard < 0) {
				this.playResultYard += playResultYard;
			} else if (this.playResultYard < 0 && playResultYard > 0) {
				this.playResultYard += playResultYard;
			} else {
				this.playResultYard = playResultYard;
			}
		} else {
			this.playResultYard = playResultYard;
		}
	}

	public void updatePenaltyPlayResultYard(Integer playResultYard) {
		if (this.playResultYard == null) {
			this.playResultYard = 0;
		}
		this.playResultYard += playResultYard;
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
		if (this.playResultFirstDown != null && this.playResultFirstDown != playResultFirstDown) {
			if (playResultFirstDown) {
				this.playResultFirstDown = playResultFirstDown;
			}
//			throw new IllegalArgumentException("Handle this - first down");
		} else {
			this.playResultFirstDown = playResultFirstDown;
		}
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

	/**
	 * @return the playResultYardLine
	 */
	public Integer getPlayResultYardLine() {
		return playResultYardLine;
	}

	/**
	 * @param playResultYardLine the playResultYardLine to set
	 */
	public void setPlayResultYardLine(Integer playResultYardLine) {
		this.playResultYardLine = playResultYardLine;
	}

	
}
