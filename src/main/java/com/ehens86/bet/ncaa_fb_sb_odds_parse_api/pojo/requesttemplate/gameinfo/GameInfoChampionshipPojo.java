package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

import java.util.List;

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
		
	}

	public GameInfoChampionshipPojo(String division, String year, String bracketRound, List<Object> champLinks,
			String championshipId, String bracketId, String title, String sport, String bracketRegion) {
		super();
		this.division = division;
		this.year = year;
		this.bracketRound = bracketRound;
		this.champLinks = champLinks;
		this.championshipId = championshipId;
		this.bracketId = bracketId;
		this.title = title;
		this.sport = sport;
		this.bracketRegion = bracketRegion;
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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((bracketId == null) ? 0 : bracketId.hashCode());
		result = prime * result + ((bracketRegion == null) ? 0 : bracketRegion.hashCode());
		result = prime * result + ((bracketRound == null) ? 0 : bracketRound.hashCode());
		result = prime * result + ((champLinks == null) ? 0 : champLinks.hashCode());
		result = prime * result + ((championshipId == null) ? 0 : championshipId.hashCode());
		result = prime * result + ((division == null) ? 0 : division.hashCode());
		result = prime * result + ((sport == null) ? 0 : sport.hashCode());
		result = prime * result + ((title == null) ? 0 : title.hashCode());
		result = prime * result + ((year == null) ? 0 : year.hashCode());
		return result;
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
		if (bracketId == null) {
			if (other.bracketId != null) {
				return false;
			}
		} else if (!bracketId.equals(other.bracketId)) {
			return false;
		}
		if (bracketRegion == null) {
			if (other.bracketRegion != null) {
				return false;
			}
		} else if (!bracketRegion.equals(other.bracketRegion)) {
			return false;
		}
		if (bracketRound == null) {
			if (other.bracketRound != null) {
				return false;
			}
		} else if (!bracketRound.equals(other.bracketRound)) {
			return false;
		}
		if (champLinks == null) {
			if (other.champLinks != null) {
				return false;
			}
		} else if (!champLinks.equals(other.champLinks)) {
			return false;
		}
		if (championshipId == null) {
			if (other.championshipId != null) {
				return false;
			}
		} else if (!championshipId.equals(other.championshipId)) {
			return false;
		}
		if (division == null) {
			if (other.division != null) {
				return false;
			}
		} else if (!division.equals(other.division)) {
			return false;
		}
		if (sport == null) {
			if (other.sport != null) {
				return false;
			}
		} else if (!sport.equals(other.sport)) {
			return false;
		}
		if (title == null) {
			if (other.title != null) {
				return false;
			}
		} else if (!title.equals(other.title)) {
			return false;
		}
		if (year == null) {
			if (other.year != null) {
				return false;
			}
		} else if (!year.equals(other.year)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "GameInfoChampionshipPojo [division=" + division + ", year=" + year + ", bracketRound=" + bracketRound
				+ ", champLinks=" + champLinks + ", championshipId=" + championshipId + ", bracketId=" + bracketId
				+ ", title=" + title + ", sport=" + sport + ", bracketRegion=" + bracketRegion + "]";
	}
	
	

}
