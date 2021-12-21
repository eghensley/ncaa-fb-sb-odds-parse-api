package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;

public class ParseRequest {

	private DivisionEnum target;
	private Integer year;
	private Integer week;
	private Integer gameId;
	
	public ParseRequest() {
		
	}

	public ParseRequest(DivisionEnum target, Integer year, Integer week, Integer gameId) {
		super();
		this.target = target;
		this.year = year;
		this.week = week;
		this.gameId = gameId;
	}

	/**
	 * @return the target
	 */
	public DivisionEnum getTarget() {
		return target;
	}

	/**
	 * @param target the target to set
	 */
	public void setTarget(DivisionEnum target) {
		this.target = target;
	}

	/**
	 * @return the year
	 */
	public Integer getYear() {
		return year;
	}

	/**
	 * @param year the year to set
	 */
	public void setYear(Integer year) {
		this.year = year;
	}

	/**
	 * @return the week
	 */
	public Integer getWeek() {
		return week;
	}

	/**
	 * @param week the week to set
	 */
	public void setWeek(Integer week) {
		this.week = week;
	}

	/**
	 * @return the gameId
	 */
	public Integer getGameId() {
		return gameId;
	}

	/**
	 * @param gameId the gameId to set
	 */
	public void setGameId(Integer gameId) {
		this.gameId = gameId;
	}

	@Override
	public int hashCode() {
		return Objects.hash(gameId, target, week, year);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof ParseRequest)) {
			return false;
		}
		ParseRequest other = (ParseRequest) obj;
		return Objects.equals(gameId, other.gameId) && target == other.target && Objects.equals(week, other.week)
				&& Objects.equals(year, other.year);
	}

	@Override
	public String toString() {
		return "ParseRequest [target=" + target + ", year=" + year + ", week=" + week + ", gameId=" + gameId + "]";
	}

}
