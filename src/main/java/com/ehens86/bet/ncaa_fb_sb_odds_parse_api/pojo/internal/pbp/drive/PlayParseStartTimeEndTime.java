package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive;

import java.util.Objects;

public class PlayParseStartTimeEndTime {
	private Integer startTime;
	private Integer endTime;

	public PlayParseStartTimeEndTime() {

	}

	public PlayParseStartTimeEndTime(Integer startTime, Integer endTime) {
		super();
		this.startTime = startTime;
		this.endTime = endTime;
	}

	/**
	 * @return the startTime
	 */
	public Integer getStartTime() {
		return startTime;
	}

	/**
	 * @param startTime the startTime to set
	 */
	public void setStartTime(Integer startTime) {
		this.startTime = startTime;
	}

	/**
	 * @return the endTime
	 */
	public Integer getEndTime() {
		return endTime;
	}

	/**
	 * @param endTime the endTime to set
	 */
	public void setEndTime(Integer endTime) {
		this.endTime = endTime;
	}

	@Override
	public int hashCode() {
		return Objects.hash(endTime, startTime);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayParseStartTimeEndTime)) {
			return false;
		}
		PlayParseStartTimeEndTime other = (PlayParseStartTimeEndTime) obj;
		return Objects.equals(endTime, other.endTime) && Objects.equals(startTime, other.startTime);
	}

	@Override
	public String toString() {
		return "PlayParseStartTimeEndTime [startTime=" + startTime + ", endTime=" + endTime + "]";
	}

}
