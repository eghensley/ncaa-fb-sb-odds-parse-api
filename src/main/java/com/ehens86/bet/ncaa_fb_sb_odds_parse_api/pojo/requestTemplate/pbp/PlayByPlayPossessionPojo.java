package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp;

import java.util.List;

public class PlayByPlayPossessionPojo {
	private String teamId;
	private String time;
	private List<PlayByPlayPlayPojo> plays;

	public PlayByPlayPossessionPojo() {

	}

	public PlayByPlayPossessionPojo(String teamId, String time, List<PlayByPlayPlayPojo> plays) {
		super();
		this.teamId = teamId;
		this.time = time;
		this.plays = plays;
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
		if ("00:00".equals(this.time)) {
			return "15:00";
		} else {
			return time;
		}
	}

	/**
	 * @param time the time to set
	 */
	public void setTime(String time) {
		this.time = time;
	}

	/**
	 * @return the plays
	 */
	public List<PlayByPlayPlayPojo> getPlays() {
		return plays;
	}

	/**
	 * @param plays the plays to set
	 */
	public void setPlays(List<PlayByPlayPlayPojo> plays) {
		this.plays = plays;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((plays == null) ? 0 : plays.hashCode());
		result = prime * result + ((teamId == null) ? 0 : teamId.hashCode());
		result = prime * result + ((time == null) ? 0 : time.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayByPlayPossessionPojo)) {
			return false;
		}
		PlayByPlayPossessionPojo other = (PlayByPlayPossessionPojo) obj;
		if (plays == null) {
			if (other.plays != null) {
				return false;
			}
		} else if (!plays.equals(other.plays)) {
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
		return true;
	}

	@Override
	public String toString() {
		return "PlayByPlayPossessionPojo [teamId=" + teamId + ", time=" + time + ", plays=" + plays + "]";
	}

}
