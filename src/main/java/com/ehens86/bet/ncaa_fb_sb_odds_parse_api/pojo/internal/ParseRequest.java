package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;

public class ParseRequest {

	private DivisionEnum target;
	private Integer year;
	private Integer week;
	
	public ParseRequest() {
		
	}

	public ParseRequest(DivisionEnum target, Integer year, Integer week) {
		super();
		this.target = target;
		this.year = year;
		this.week = week;
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
	


}
