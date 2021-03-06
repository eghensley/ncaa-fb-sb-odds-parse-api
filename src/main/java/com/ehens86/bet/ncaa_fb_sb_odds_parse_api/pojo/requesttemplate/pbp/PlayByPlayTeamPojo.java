package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;

public class PlayByPlayTeamPojo {
    private String homeTeam;
    private String id;
    private String color;
    private String shortname;
    private String seoName;
    private String sixCharAbbr;
    
    public PlayByPlayTeamPojo() {
    	
    }

	public PlayByPlayTeamPojo(String homeTeam, String id, String color, String shortname, String seoName,
			String sixCharAbbr) {
		super();
		this.homeTeam = homeTeam;
		this.id = id;
		this.color = color;
		this.shortname = shortname;
		this.seoName = seoName;
		this.sixCharAbbr = sixCharAbbr;
	}

	/**
	 * @return the homeTeam
	 */
	public String getHomeTeam() {
		return homeTeam;
	}
	
	/**
	 * @return the homeTeam
	 */
	public HomeAwayEnum pullHomeAwayEnum() {
		if ("false".equals(this.homeTeam)) {
			return HomeAwayEnum.AWAY;
		} else if ("true".equals(this.homeTeam)) {
			return HomeAwayEnum.HOME;
		} else {
			throw new IllegalArgumentException(String.format("Value of %s could not be cast to HomeAwayEnum", this.homeTeam));
		}
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

	@Override
	public int hashCode() {
		return Objects.hash(color, homeTeam, id, seoName, shortname, sixCharAbbr);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayByPlayTeamPojo)) {
			return false;
		}
		PlayByPlayTeamPojo other = (PlayByPlayTeamPojo) obj;
		return Objects.equals(color, other.color) && Objects.equals(homeTeam, other.homeTeam)
				&& Objects.equals(id, other.id) && Objects.equals(seoName, other.seoName)
				&& Objects.equals(shortname, other.shortname) && Objects.equals(sixCharAbbr, other.sixCharAbbr);
	}

	@Override
	public String toString() {
		return "PlayByPlayTeamPojo [homeTeam=" + homeTeam + ", id=" + id + ", color=" + color + ", shortname="
				+ shortname + ", seoName=" + seoName + ", sixCharAbbr=" + sixCharAbbr + "]";
	}
    
    
}
