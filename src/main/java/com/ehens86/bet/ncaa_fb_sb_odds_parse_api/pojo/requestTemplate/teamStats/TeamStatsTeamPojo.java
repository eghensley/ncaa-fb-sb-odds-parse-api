package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.teamStats;

import java.util.ArrayList;

public class TeamStatsTeamPojo {
    private String homeTeam;
    private String id;
    private String color;
    private String shortname;
    private String seoName;
    private String sixCharAbbr;
    private String teamId;
    private ArrayList<TeamStatsStatPojo> stats;
    
    public TeamStatsTeamPojo() {
    	
    }

	public TeamStatsTeamPojo(String homeTeam, String id, String color, String shortname, String seoName,
			String sixCharAbbr, String teamId, ArrayList<TeamStatsStatPojo> stats) {
		super();
		this.homeTeam = homeTeam;
		this.id = id;
		this.color = color;
		this.shortname = shortname;
		this.seoName = seoName;
		this.sixCharAbbr = sixCharAbbr;
		this.teamId = teamId;
		this.stats = stats;
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
	public ArrayList<TeamStatsStatPojo> getStats() {
		return stats;
	}

	/**
	 * @param stats the stats to set
	 */
	public void setStats(ArrayList<TeamStatsStatPojo> stats) {
		this.stats = stats;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((color == null) ? 0 : color.hashCode());
		result = prime * result + ((homeTeam == null) ? 0 : homeTeam.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((seoName == null) ? 0 : seoName.hashCode());
		result = prime * result + ((shortname == null) ? 0 : shortname.hashCode());
		result = prime * result + ((sixCharAbbr == null) ? 0 : sixCharAbbr.hashCode());
		result = prime * result + ((stats == null) ? 0 : stats.hashCode());
		result = prime * result + ((teamId == null) ? 0 : teamId.hashCode());
		return result;
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
		if (color == null) {
			if (other.color != null) {
				return false;
			}
		} else if (!color.equals(other.color)) {
			return false;
		}
		if (homeTeam == null) {
			if (other.homeTeam != null) {
				return false;
			}
		} else if (!homeTeam.equals(other.homeTeam)) {
			return false;
		}
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if (!id.equals(other.id)) {
			return false;
		}
		if (seoName == null) {
			if (other.seoName != null) {
				return false;
			}
		} else if (!seoName.equals(other.seoName)) {
			return false;
		}
		if (shortname == null) {
			if (other.shortname != null) {
				return false;
			}
		} else if (!shortname.equals(other.shortname)) {
			return false;
		}
		if (sixCharAbbr == null) {
			if (other.sixCharAbbr != null) {
				return false;
			}
		} else if (!sixCharAbbr.equals(other.sixCharAbbr)) {
			return false;
		}
		if (stats == null) {
			if (other.stats != null) {
				return false;
			}
		} else if (!stats.equals(other.stats)) {
			return false;
		}
		if (teamId == null) {
			if (other.teamId != null) {
				return false;
			}
		} else if (!teamId.equals(other.teamId)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatsTeamPojo [homeTeam=" + homeTeam + ", id=" + id + ", color=" + color + ", shortname="
				+ shortname + ", seoName=" + seoName + ", sixCharAbbr=" + sixCharAbbr + ", teamId=" + teamId
				+ ", stats=" + stats + "]";
	}
    
    
}
