package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.scoringSummary;

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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((driveText == null) ? 0 : driveText.hashCode());
		result = prime * result + ((homeScore == null) ? 0 : homeScore.hashCode());
		result = prime * result + ((scoreText == null) ? 0 : scoreText.hashCode());
		result = prime * result + ((scoreType == null) ? 0 : scoreType.hashCode());
		result = prime * result + ((teamId == null) ? 0 : teamId.hashCode());
		result = prime * result + ((time == null) ? 0 : time.hashCode());
		result = prime * result + ((visitingScore == null) ? 0 : visitingScore.hashCode());
		return result;
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
		if (driveText == null) {
			if (other.driveText != null) {
				return false;
			}
		} else if (!driveText.equals(other.driveText)) {
			return false;
		}
		if (homeScore == null) {
			if (other.homeScore != null) {
				return false;
			}
		} else if (!homeScore.equals(other.homeScore)) {
			return false;
		}
		if (scoreText == null) {
			if (other.scoreText != null) {
				return false;
			}
		} else if (!scoreText.equals(other.scoreText)) {
			return false;
		}
		if (scoreType == null) {
			if (other.scoreType != null) {
				return false;
			}
		} else if (!scoreType.equals(other.scoreType)) {
			return false;
		}
		if (teamId == null) {
			if (other.teamId != null) {
				return false;
			}
		} else if (!teamId.equals(other.teamId)) {
			return false;
		}
		if (time == null) {
			if (other.time != null) {
				return false;
			}
		} else if (!time.equals(other.time)) {
			return false;
		}
		if (visitingScore == null) {
			if (other.visitingScore != null) {
				return false;
			}
		} else if (!visitingScore.equals(other.visitingScore)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "ScoringSummarySummaryPojo [teamId=" + teamId + ", time=" + time + ", scoreType=" + scoreType
				+ ", scoreText=" + scoreText + ", driveText=" + driveText + ", visitingScore=" + visitingScore
				+ ", homeScore=" + homeScore + "]";
	}
    
    
}
