package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.scoringsummary;

import java.util.Objects;

public class ScoringSummarySummaryPojo {
    private String teamId;
    private String time;
    private String scoreType;
    private String scoreText;
    private String driveText;
    private String visitingScore;
    private String homeScore;
    
    public ScoringSummarySummaryPojo() {
    	
    }

	public ScoringSummarySummaryPojo(String teamId, String time, String scoreType, String scoreText, String driveText,
			String visitingScore, String homeScore) {
		super();
		this.teamId = teamId;
		this.time = time;
		this.scoreType = scoreType;
		this.scoreText = scoreText;
		this.driveText = driveText;
		this.visitingScore = visitingScore;
		this.homeScore = homeScore;
	}

	/**
	 * @return the teamId
	 */
	public String getTeamId() {
		return teamId;
	}

	/**
	 * @param teamId the teamId to set
	 */
	public void setTeamId(String teamId) {
		this.teamId = teamId;
	}

	/**
	 * @return the time
	 */
	public String getTime() {
		return time;
	}

	/**
	 * @param time the time to set
	 */
	public void setTime(String time) {
		this.time = time;
	}

	/**
	 * @return the scoreType
	 */
	public String getScoreType() {
		return scoreType;
	}

	/**
	 * @param scoreType the scoreType to set
	 */
	public void setScoreType(String scoreType) {
		this.scoreType = scoreType;
	}

	/**
	 * @return the scoreText
	 */
	public String getScoreText() {
		return scoreText;
	}

	/**
	 * @param scoreText the scoreText to set
	 */
	public void setScoreText(String scoreText) {
		this.scoreText = scoreText;
	}

	/**
	 * @return the driveText
	 */
	public String getDriveText() {
		return driveText;
	}

	/**
	 * @param driveText the driveText to set
	 */
	public void setDriveText(String driveText) {
		this.driveText = driveText;
	}

	/**
	 * @return the visitingScore
	 */
	public String getVisitingScore() {
		return visitingScore;
	}

	/**
	 * @param visitingScore the visitingScore to set
	 */
	public void setVisitingScore(String visitingScore) {
		this.visitingScore = visitingScore;
	}

	/**
	 * @return the homeScore
	 */
	public String getHomeScore() {
		return homeScore;
	}

	/**
	 * @param homeScore the homeScore to set
	 */
	public void setHomeScore(String homeScore) {
		this.homeScore = homeScore;
	}

	@Override
	public int hashCode() {
		return Objects.hash(driveText, homeScore, scoreText, scoreType, teamId, time, visitingScore);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof ScoringSummarySummaryPojo)) {
			return false;
		}
		ScoringSummarySummaryPojo other = (ScoringSummarySummaryPojo) obj;
		return Objects.equals(driveText, other.driveText) && Objects.equals(homeScore, other.homeScore)
				&& Objects.equals(scoreText, other.scoreText) && Objects.equals(scoreType, other.scoreType)
				&& Objects.equals(teamId, other.teamId) && Objects.equals(time, other.time)
				&& Objects.equals(visitingScore, other.visitingScore);
	}

	@Override
	public String toString() {
		return "ScoringSummarySummaryPojo [teamId=" + teamId + ", time=" + time + ", scoreType=" + scoreType
				+ ", scoreText=" + scoreText + ", driveText=" + driveText + ", visitingScore=" + visitingScore
				+ ", homeScore=" + homeScore + "]";
	}
    
    
}
