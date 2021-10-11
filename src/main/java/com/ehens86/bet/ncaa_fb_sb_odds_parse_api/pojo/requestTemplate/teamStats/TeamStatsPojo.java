package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.teamStats;

import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

public class TeamStatsPojo {
    private TeamStatsMetaPojo meta;
    public Collection<TeamStatsTeamPojo> teams;
    
    public TeamStatsPojo() {
    	
    }

	public TeamStatsPojo(TeamStatsMetaPojo meta, ArrayList<TeamStatsTeamPojo> teams) {
		super();
		this.meta = meta;
		this.teams = teams;
	}

	/**
	 * @return the meta
	 */
	public TeamStatsMetaPojo getMeta() {
		return meta;
	}

	/**
	 * @param meta the meta to set
	 */
	public void setMeta(TeamStatsMetaPojo meta) {
		this.meta = meta;
	}

	/**
	 * @return the teams
	 */
	public Collection<TeamStatsTeamPojo> getTeams() {
		return teams;
	}

	/**
	 * @param teams the teams to set
	 */
	public void setTeams(ArrayList<TeamStatsTeamPojo> teams) {
		this.teams = teams;
	}

	public TeamStatsTeamPojo pullAwayTeam() {
		return this.meta.getTeams().stream().filter(team -> "false".equals(team.getHomeTeam())).collect(Collectors.toList()).get(0);
	}

	public TeamStatsTeamPojo pullHomeTeam() {
		return this.meta.getTeams().stream().filter(team -> "true".equals(team.getHomeTeam())).collect(Collectors.toList()).get(0);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((meta == null) ? 0 : meta.hashCode());
		result = prime * result + ((teams == null) ? 0 : teams.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatsPojo)) {
			return false;
		}
		TeamStatsPojo other = (TeamStatsPojo) obj;
		if (meta == null) {
			if (other.meta != null) {
				return false;
			}
		} else if (!meta.equals(other.meta)) {
			return false;
		}
		if (teams == null) {
			if (other.teams != null) {
				return false;
			}
		} else if (!teams.equals(other.teams)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatsPojo [meta=" + meta + ", teams=" + teams + "]";
	}
    
    
}
