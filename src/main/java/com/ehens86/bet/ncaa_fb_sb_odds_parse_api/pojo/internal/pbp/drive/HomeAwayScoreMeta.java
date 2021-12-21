package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive;

public class HomeAwayScoreMeta {
	private Integer homeStartScore;
	private Integer awayStartScore;
	private Integer homeResultScore;
	private Integer awayResultScore;
	private Integer homeScore;
	private Integer awayScore;

	public HomeAwayScoreMeta() {
		this.homeStartScore = 0;
		this.awayStartScore = 0;
		this.homeResultScore = 0;
		this.awayResultScore = 0;
		this.homeScore = 0;
		this.awayScore = 0;
	}

	/**
	 * @return the homeStartScore
	 */
	public Integer getHomeStartScore() {
		return homeStartScore;
	}

	/**
	 * @param homeStartScore the homeStartScore to set
	 */
	public void setHomeStartScore(Integer homeStartScore) {
		this.homeStartScore = homeStartScore;
	}

	/**
	 * @return the awayStartScore
	 */
	public Integer getAwayStartScore() {
		return awayStartScore;
	}

	/**
	 * @param awayStartScore the awayStartScore to set
	 */
	public void setAwayStartScore(Integer awayStartScore) {
		this.awayStartScore = awayStartScore;
	}

	/**
	 * @return the homeResultScore
	 */
	public Integer getHomeResultScore() {
		return homeResultScore;
	}

	/**
	 * @param homeResultScore the homeResultScore to set
	 */
	public void setHomeResultScore(Integer homeResultScore) {
		this.homeResultScore = homeResultScore;
	}

	/**
	 * @return the awayResultScore
	 */
	public Integer getAwayResultScore() {
		return awayResultScore;
	}

	/**
	 * @param awayResultScore the awayResultScore to set
	 */
	public void setAwayResultScore(Integer awayResultScore) {
		this.awayResultScore = awayResultScore;
	}

	/**
	 * @return the homeScore
	 */
	public Integer getHomeScore() {
		return homeScore;
	}

	/**
	 * @param homeScore the homeScore to set
	 */
	public void setHomeScore(Integer homeScore) {
		this.homeScore = homeScore;
	}

	/**
	 * @return the awayScore
	 */
	public Integer getAwayScore() {
		return awayScore;
	}

	/**
	 * @param awayScore the awayScore to set
	 */
	public void setAwayScore(Integer awayScore) {
		this.awayScore = awayScore;
	}

	public Integer calcAbsScoreDiff() {
		return Math.abs(this.awayScore - this.homeScore);
	}
}
