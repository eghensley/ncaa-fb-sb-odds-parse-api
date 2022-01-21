package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore;

import java.util.List;
import java.util.Objects;

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
    private List<BoxScoreTeamPojo> teams;
    
    public BoxScoreMetaPojo() {
    	// Base constructor
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
	public List<BoxScoreTeamPojo> getTeams() {
		return teams;
	}

	/**
	 * @param teams the teams to set
	 */
	public void setTeams(List<BoxScoreTeamPojo> teams) {
		this.teams = teams;
	}

	@Override
	public int hashCode() {
		return Objects.hash(description, division, gametype, minutes, period, seconds, sport, status, teams, title);
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
		return Objects.equals(description, other.description) && Objects.equals(division, other.division)
				&& Objects.equals(gametype, other.gametype) && Objects.equals(minutes, other.minutes)
				&& Objects.equals(period, other.period) && Objects.equals(seconds, other.seconds)
				&& Objects.equals(sport, other.sport) && Objects.equals(status, other.status)
				&& Objects.equals(teams, other.teams) && Objects.equals(title, other.title);
	}

	@Override
	public String toString() {
		return "BoxScoreMetaPojo [title=" + title + ", description=" + description + ", sport=" + sport + ", division="
				+ division + ", gametype=" + gametype + ", status=" + status + ", period=" + period + ", minutes="
				+ minutes + ", seconds=" + seconds + ", teams=" + teams + "]";
	}
    
    
}
