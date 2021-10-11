package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.scoringSummary;

import java.util.ArrayList;

public class ScoringSummaryMetaPojo {
    private String title;
    private ArrayList<ScoringSummaryTeamPojo> teams;
    
    public ScoringSummaryMetaPojo() {
    	
    }

	public ScoringSummaryMetaPojo(String title, ArrayList<ScoringSummaryTeamPojo> teams) {
		super();
		this.title = title;
		this.teams = teams;
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * @param title the title to set
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * @return the teams
	 */
	public ArrayList<ScoringSummaryTeamPojo> getTeams() {
		return teams;
	}

	/**
	 * @param teams the teams to set
	 */
	public void setTeams(ArrayList<ScoringSummaryTeamPojo> teams) {
		this.teams = teams;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((teams == null) ? 0 : teams.hashCode());
		result = prime * result + ((title == null) ? 0 : title.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof ScoringSummaryMetaPojo)) {
			return false;
		}
		ScoringSummaryMetaPojo other = (ScoringSummaryMetaPojo) obj;
		if (teams == null) {
			if (other.teams != null) {
				return false;
			}
		} else if (!teams.equals(other.teams)) {
			return false;
		}
		if (title == null) {
			if (other.title != null) {
				return false;
			}
		} else if (!title.equals(other.title)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "ScoringSummaryMetaPojo [title=" + title + ", teams=" + teams + "]";
	}
    
    
}
