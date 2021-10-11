package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.teamStats;

import java.util.List;


public class TeamStatsMetaPojo {
    private String title;
    private List<TeamStatsTeamPojo> teams;
    
    public TeamStatsMetaPojo() {
    	
    }

	public TeamStatsMetaPojo(String title, List<TeamStatsTeamPojo> teams) {
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
	public List<TeamStatsTeamPojo> getTeams() {
		return teams;
	}

	/**
	 * @param teams the teams to set
	 */
	public void setTeams(List<TeamStatsTeamPojo> teams) {
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
		if (!(obj instanceof TeamStatsMetaPojo)) {
			return false;
		}
		TeamStatsMetaPojo other = (TeamStatsMetaPojo) obj;
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
		return "TeamStatsMetaPojo [title=" + title + ", teams=" + teams + "]";
	}
    
    
}
