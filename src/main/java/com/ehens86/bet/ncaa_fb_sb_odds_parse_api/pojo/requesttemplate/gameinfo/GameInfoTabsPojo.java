package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

public class GameInfoTabsPojo {
    private boolean preview;
    private boolean boxscore;
    private boolean recap;
    private boolean scoringSummary;
    private boolean pbp;
    private boolean teamStats;
    
    public GameInfoTabsPojo() {
    	
    }

	public GameInfoTabsPojo(boolean preview, boolean boxscore, boolean recap, boolean scoringSummary, boolean pbp,
			boolean teamStats) {
		super();
		this.preview = preview;
		this.boxscore = boxscore;
		this.recap = recap;
		this.scoringSummary = scoringSummary;
		this.pbp = pbp;
		this.teamStats = teamStats;
	}

	/**
	 * @return the preview
	 */
	public boolean isPreview() {
		return preview;
	}

	/**
	 * @param preview the preview to set
	 */
	public void setPreview(boolean preview) {
		this.preview = preview;
	}

	/**
	 * @return the boxscore
	 */
	public boolean isBoxscore() {
		return boxscore;
	}

	/**
	 * @param boxscore the boxscore to set
	 */
	public void setBoxscore(boolean boxscore) {
		this.boxscore = boxscore;
	}

	/**
	 * @return the recap
	 */
	public boolean isRecap() {
		return recap;
	}

	/**
	 * @param recap the recap to set
	 */
	public void setRecap(boolean recap) {
		this.recap = recap;
	}

	/**
	 * @return the scoringSummary
	 */
	public boolean isScoringSummary() {
		return scoringSummary;
	}

	/**
	 * @param scoringSummary the scoringSummary to set
	 */
	public void setScoringSummary(boolean scoringSummary) {
		this.scoringSummary = scoringSummary;
	}

	/**
	 * @return the pbp
	 */
	public boolean isPbp() {
		return pbp;
	}

	/**
	 * @param pbp the pbp to set
	 */
	public void setPbp(boolean pbp) {
		this.pbp = pbp;
	}

	/**
	 * @return the teamStats
	 */
	public boolean isTeamStats() {
		return teamStats;
	}

	/**
	 * @param teamStats the teamStats to set
	 */
	public void setTeamStats(boolean teamStats) {
		this.teamStats = teamStats;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (boxscore ? 1231 : 1237);
		result = prime * result + (pbp ? 1231 : 1237);
		result = prime * result + (preview ? 1231 : 1237);
		result = prime * result + (recap ? 1231 : 1237);
		result = prime * result + (scoringSummary ? 1231 : 1237);
		result = prime * result + (teamStats ? 1231 : 1237);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameInfoTabsPojo)) {
			return false;
		}
		GameInfoTabsPojo other = (GameInfoTabsPojo) obj;
		if (boxscore != other.boxscore) {
			return false;
		}
		if (pbp != other.pbp) {
			return false;
		}
		if (preview != other.preview) {
			return false;
		}
		if (recap != other.recap) {
			return false;
		}
		if (scoringSummary != other.scoringSummary) {
			return false;
		}
		if (teamStats != other.teamStats) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "GameInfoTabsPojo [preview=" + preview + ", boxscore=" + boxscore + ", recap=" + recap
				+ ", scoringSummary=" + scoringSummary + ", pbp=" + pbp + ", teamStats=" + teamStats + "]";
	}
    
    
}
