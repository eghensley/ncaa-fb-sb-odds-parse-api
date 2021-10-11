package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore;

import java.util.ArrayList;

public class BoxScoreMetaPojo {
    private String title;
    private String description;
    private String sport;
    private String division;
    private String gametype;
    private String status;
    private String period;
    private String minutes;
    private String seconds;
    private ArrayList<BoxScoreTeamPojo> teams;
    
    public BoxScoreMetaPojo() {
    	
    }

	public BoxScoreMetaPojo(String title, String description, String sport, String division, String gametype,
			String status, String period, String minutes, String seconds, ArrayList<BoxScoreTeamPojo> teams) {
		super();
		this.title = title;
		this.description = description;
		this.sport = sport;
		this.division = division;
		this.gametype = gametype;
		this.status = status;
		this.period = period;
		this.minutes = minutes;
		this.seconds = seconds;
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
	 * @return the description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * @param description the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * @return the sport
	 */
	public String getSport() {
		return sport;
	}

	/**
	 * @param sport the sport to set
	 */
	public void setSport(String sport) {
		this.sport = sport;
	}

	/**
	 * @return the division
	 */
	public String getDivision() {
		return division;
	}

	/**
	 * @param division the division to set
	 */
	public void setDivision(String division) {
		this.division = division;
	}

	/**
	 * @return the gametype
	 */
	public String getGametype() {
		return gametype;
	}

	/**
	 * @param gametype the gametype to set
	 */
	public void setGametype(String gametype) {
		this.gametype = gametype;
	}

	/**
	 * @return the status
	 */
	public String getStatus() {
		return status;
	}

	/**
	 * @param status the status to set
	 */
	public void setStatus(String status) {
		this.status = status;
	}

	/**
	 * @return the period
	 */
	public String getPeriod() {
		return period;
	}

	/**
	 * @param period the period to set
	 */
	public void setPeriod(String period) {
		this.period = period;
	}

	/**
	 * @return the minutes
	 */
	public String getMinutes() {
		return minutes;
	}

	/**
	 * @param minutes the minutes to set
	 */
	public void setMinutes(String minutes) {
		this.minutes = minutes;
	}

	/**
	 * @return the seconds
	 */
	public String getSeconds() {
		return seconds;
	}

	/**
	 * @param seconds the seconds to set
	 */
	public void setSeconds(String seconds) {
		this.seconds = seconds;
	}

	/**
	 * @return the teams
	 */
	public ArrayList<BoxScoreTeamPojo> getTeams() {
		return teams;
	}

	/**
	 * @param teams the teams to set
	 */
	public void setTeams(ArrayList<BoxScoreTeamPojo> teams) {
		this.teams = teams;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((description == null) ? 0 : description.hashCode());
		result = prime * result + ((division == null) ? 0 : division.hashCode());
		result = prime * result + ((gametype == null) ? 0 : gametype.hashCode());
		result = prime * result + ((minutes == null) ? 0 : minutes.hashCode());
		result = prime * result + ((period == null) ? 0 : period.hashCode());
		result = prime * result + ((seconds == null) ? 0 : seconds.hashCode());
		result = prime * result + ((sport == null) ? 0 : sport.hashCode());
		result = prime * result + ((status == null) ? 0 : status.hashCode());
		result = prime * result + ((teams == null) ? 0 : teams.hashCode());
		result = prime * result + ((title == null) ? 0 : title.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BoxScoreMetaPojo)) {
			return false;
		}
		BoxScoreMetaPojo other = (BoxScoreMetaPojo) obj;
		if (description == null) {
			if (other.description != null) {
				return false;
			}
		} else if (!description.equals(other.description)) {
			return false;
		}
		if (division == null) {
			if (other.division != null) {
				return false;
			}
		} else if (!division.equals(other.division)) {
			return false;
		}
		if (gametype == null) {
			if (other.gametype != null) {
				return false;
			}
		} else if (!gametype.equals(other.gametype)) {
			return false;
		}
		if (minutes == null) {
			if (other.minutes != null) {
				return false;
			}
		} else if (!minutes.equals(other.minutes)) {
			return false;
		}
		if (period == null) {
			if (other.period != null) {
				return false;
			}
		} else if (!period.equals(other.period)) {
			return false;
		}
		if (seconds == null) {
			if (other.seconds != null) {
				return false;
			}
		} else if (!seconds.equals(other.seconds)) {
			return false;
		}
		if (sport == null) {
			if (other.sport != null) {
				return false;
			}
		} else if (!sport.equals(other.sport)) {
			return false;
		}
		if (status == null) {
			if (other.status != null) {
				return false;
			}
		} else if (!status.equals(other.status)) {
			return false;
		}
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
		return "BoxScoreMetaPojo [title=" + title + ", description=" + description + ", sport=" + sport + ", division="
				+ division + ", gametype=" + gametype + ", status=" + status + ", period=" + period + ", minutes="
				+ minutes + ", seconds=" + seconds + ", teams=" + teams + "]";
	}
    
    
}
