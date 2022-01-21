package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.teamstats;

import java.util.List;
import java.util.Objects;

public class TeamStatsTeamPojo {
    private String homeTeam;
    private String id;
    private String color;
    private String shortname;
    private String seoName;
    private String sixCharAbbr;
    private String teamId;
    private List<TeamStatsStatPojo> stats;
    
    public TeamStatsTeamPojo() {
    	// Base constructor
    }

	/**
	 * @return the homeTeam
	 */
	public String getHomeTeam() {
		return homeTeam;
	}

	/**
	 * @param homeTeam the homeTeam to set
	 */
	public void setHomeTeam(String homeTeam) {
		this.homeTeam = homeTeam;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the color
	 */
	public String getColor() {
		return color;
	}

	/**
	 * @param color the color to set
	 */
	public void setColor(String color) {
		this.color = color;
	}

	/**
	 * @return the shortname
	 */
	public String getShortname() {
		return shortname;
	}

	/**
	 * @param shortname the shortname to set
	 */
	public void setShortname(String shortname) {
		this.shortname = shortname;
	}

	/**
	 * @return the seoName
	 */
	public String getSeoName() {
		return seoName;
	}

	/**
	 * @param seoName the seoName to set
	 */
	public void setSeoName(String seoName) {
		this.seoName = seoName;
	}

	/**
	 * @return the sixCharAbbr
	 */
	public String getSixCharAbbr() {
		return sixCharAbbr;
	}

	/**
	 * @param sixCharAbbr the sixCharAbbr to set
	 */
	public void setSixCharAbbr(String sixCharAbbr) {
		this.sixCharAbbr = sixCharAbbr;
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
	 * @return the stats
	 */
	public List<TeamStatsStatPojo> getStats() {
		return stats;
	}

	/**
	 * @param stats the stats to set
	 */
	public void setStats(List<TeamStatsStatPojo> stats) {
		this.stats = stats;
	}

	@Override
	public int hashCode() {
		return Objects.hash(color, homeTeam, id, seoName, shortname, sixCharAbbr, stats, teamId);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatsTeamPojo)) {
			return false;
		}
		TeamStatsTeamPojo other = (TeamStatsTeamPojo) obj;
		return Objects.equals(color, other.color) && Objects.equals(homeTeam, other.homeTeam)
				&& Objects.equals(id, other.id) && Objects.equals(seoName, other.seoName)
				&& Objects.equals(shortname, other.shortname) && Objects.equals(sixCharAbbr, other.sixCharAbbr)
				&& Objects.equals(stats, other.stats) && Objects.equals(teamId, other.teamId);
	}

	@Override
	public String toString() {
		return "TeamStatsTeamPojo [homeTeam=" + homeTeam + ", id=" + id + ", color=" + color + ", shortname="
				+ shortname + ", seoName=" + seoName + ", sixCharAbbr=" + sixCharAbbr + ", teamId=" + teamId
				+ ", stats=" + stats + "]";
	}
    
    
}
