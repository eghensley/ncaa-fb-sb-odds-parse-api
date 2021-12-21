package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import java.util.Objects;


public class PlayResultPojo {
	protected Integer playResultYard;
	protected Integer playResultPoints;
	protected Boolean playResultFirstDown;
	protected Boolean playResultTurnover;
	protected Integer playResultYardLine;
	protected String playResultPossessionTeamId;
	protected Boolean playResultSuccess;

	protected Integer playResultHomeScore;
	protected Integer playResultAwayScore;
	
	
	
	
	public PlayResultPojo() {
		// constructor
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
		if (this.playResultFirstDown != null && !this.playResultFirstDown.equals(playResultFirstDown)) {
			if (Boolean.TRUE.equals(playResultFirstDown)) {
				this.playResultFirstDown = playResultFirstDown;
			}
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

	/**
	 * @return the playResultPossessionTeamId
	 */
	public String getPlayResultPossessionTeamId() {
		return playResultPossessionTeamId;
	}

	/**
	 * @param playResultPossessionTeamId the playResultPossessionTeamId to set
	 */
	public void setPlayResultPossessionTeamId(String playResultPossessionTeamId) {
		this.playResultPossessionTeamId = playResultPossessionTeamId;
	}

	/**
	 * @return the playResultHomeScore
	 */
	public Integer getPlayResultHomeScore() {
		return playResultHomeScore;
	}

	/**
	 * @param playResultHomeScore the playResultHomeScore to set
	 */
	public void setPlayResultHomeScore(Integer playResultHomeScore) {
		this.playResultHomeScore = playResultHomeScore;
	}

	/**
	 * @return the playResultAwayScore
	 */
	public Integer getPlayResultAwayScore() {
		return playResultAwayScore;
	}

	/**
	 * @param playResultAwayScore the playResultAwayScore to set
	 */
	public void setPlayResultAwayScore(Integer playResultAwayScore) {
		this.playResultAwayScore = playResultAwayScore;
	}

	/**
	 * @return the playResultSuccess
	 */
	public Boolean getPlayResultSuccess() {
		return playResultSuccess;
	}

	/**
	 * @param playResultSuccess the playResultSuccess to set
	 */
	public void setPlayResultSuccess(Boolean playResultSuccess) {
		this.playResultSuccess = playResultSuccess;
	}

	@Override
	public int hashCode() {
		return Objects.hash(playResultAwayScore, playResultFirstDown, playResultHomeScore, playResultPoints,
				playResultPossessionTeamId, playResultSuccess, playResultTurnover, playResultYard, playResultYardLine);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayResultPojo)) {
			return false;
		}
		PlayResultPojo other = (PlayResultPojo) obj;
		return Objects.equals(playResultAwayScore, other.playResultAwayScore)
				&& Objects.equals(playResultFirstDown, other.playResultFirstDown)
				&& Objects.equals(playResultHomeScore, other.playResultHomeScore)
				&& Objects.equals(playResultPoints, other.playResultPoints)
				&& Objects.equals(playResultPossessionTeamId, other.playResultPossessionTeamId)
				&& Objects.equals(playResultSuccess, other.playResultSuccess)
				&& Objects.equals(playResultTurnover, other.playResultTurnover)
				&& Objects.equals(playResultYard, other.playResultYard)
				&& Objects.equals(playResultYardLine, other.playResultYardLine);
	}

	@Override
	public String toString() {
		return "PlayResultPojo [playResultYard=" + playResultYard + ", playResultPoints=" + playResultPoints
				+ ", playResultFirstDown=" + playResultFirstDown + ", playResultTurnover=" + playResultTurnover
				+ ", playResultYardLine=" + playResultYardLine + ", playResultPossessionTeamId="
				+ playResultPossessionTeamId + ", playResultSuccess=" + playResultSuccess + ", playResultHomeScore="
				+ playResultHomeScore + ", playResultAwayScore=" + playResultAwayScore + "]";
	}



}
