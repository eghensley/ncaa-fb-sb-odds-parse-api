package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

import java.util.List;
import java.util.Objects;

public class GameInfoChampionshipPojo {

	private String division;
	private String year;
	private String bracketRound;
	private List<Object> champLinks;
	private String championshipId;
	private String bracketId;
	private String title;
	private String sport;
	private String bracketRegion;
	
	public GameInfoChampionshipPojo() {
		// Base constructor
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
	 * @return the year
	 */
	public String getYear() {
		return year;
	}

	/**
	 * @param year the year to set
	 */
	public void setYear(String year) {
		this.year = year;
	}

	/**
	 * @return the bracketRound
	 */
	public String getBracketRound() {
		return bracketRound;
	}

	/**
	 * @param bracketRound the bracketRound to set
	 */
	public void setBracketRound(String bracketRound) {
		this.bracketRound = bracketRound;
	}

	/**
	 * @return the champLinks
	 */
	public List<Object> getChampLinks() {
		return champLinks;
	}

	/**
	 * @param champLinks the champLinks to set
	 */
	public void setChampLinks(List<Object> champLinks) {
		this.champLinks = champLinks;
	}

	/**
	 * @return the championshipId
	 */
	public String getChampionshipId() {
		return championshipId;
	}

	/**
	 * @param championshipId the championshipId to set
	 */
	public void setChampionshipId(String championshipId) {
		this.championshipId = championshipId;
	}

	/**
	 * @return the bracketId
	 */
	public String getBracketId() {
		return bracketId;
	}

	/**
	 * @param bracketId the bracketId to set
	 */
	public void setBracketId(String bracketId) {
		this.bracketId = bracketId;
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
	 * @return the bracketRegion
	 */
	public String getBracketRegion() {
		return bracketRegion;
	}

	/**
	 * @param bracketRegion the bracketRegion to set
	 */
	public void setBracketRegion(String bracketRegion) {
		this.bracketRegion = bracketRegion;
	}

	@Override
	public int hashCode() {
		return Objects.hash(bracketId, bracketRegion, bracketRound, champLinks, championshipId, division, sport, title,
				year);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameInfoChampionshipPojo)) {
			return false;
		}
		GameInfoChampionshipPojo other = (GameInfoChampionshipPojo) obj;
		return Objects.equals(bracketId, other.bracketId) && Objects.equals(bracketRegion, other.bracketRegion)
				&& Objects.equals(bracketRound, other.bracketRound) && Objects.equals(champLinks, other.champLinks)
				&& Objects.equals(championshipId, other.championshipId) && Objects.equals(division, other.division)
				&& Objects.equals(sport, other.sport) && Objects.equals(title, other.title)
				&& Objects.equals(year, other.year);
	}

	@Override
	public String toString() {
		return "GameInfoChampionshipPojo [division=" + division + ", year=" + year + ", bracketRound=" + bracketRound
				+ ", champLinks=" + champLinks + ", championshipId=" + championshipId + ", bracketId=" + bracketId
				+ ", title=" + title + ", sport=" + sport + ", bracketRegion=" + bracketRegion + "]";
	}
	
	

}
