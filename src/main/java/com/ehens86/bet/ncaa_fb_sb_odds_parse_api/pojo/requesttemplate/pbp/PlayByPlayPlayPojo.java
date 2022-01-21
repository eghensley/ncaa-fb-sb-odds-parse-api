package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp;

import java.util.Objects;

public class PlayByPlayPlayPojo {
    private String scoreText;
    private String driveText;
    private String teamId;
    private String visitingScore;
    private String homeScore;
    
    public PlayByPlayPlayPojo() {
    	
    }

	public PlayByPlayPlayPojo(String scoreText, String driveText, String teamId, String visitingScore,
			String homeScore) {
		super();
		this.scoreText = scoreText;
		this.driveText = driveText;
		this.teamId = teamId;
		this.visitingScore = visitingScore;
		this.homeScore = homeScore;
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
		return Objects.hash(driveText, homeScore, scoreText, teamId, visitingScore);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayByPlayPlayPojo)) {
			return false;
		}
		PlayByPlayPlayPojo other = (PlayByPlayPlayPojo) obj;
		return Objects.equals(driveText, other.driveText) && Objects.equals(homeScore, other.homeScore)
				&& Objects.equals(scoreText, other.scoreText) && Objects.equals(teamId, other.teamId)
				&& Objects.equals(visitingScore, other.visitingScore);
	}

	@Override
	public String toString() {
		return "PlayByPlayPlayPojo [scoreText=" + scoreText + ", driveText=" + driveText + ", teamId=" + teamId
				+ ", visitingScore=" + visitingScore + ", homeScore=" + homeScore + "]";
	}
    
    
}
