package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore;

import java.util.Objects;

public class BoxScoreTeamPojo {
    private String homeTeam;
    private String id;
    private String seoName;
    private String sixCharAbbr;
    private String shortName;
    private String nickName;
    private String color;
    
    public BoxScoreTeamPojo() {
    	
    }

	public BoxScoreTeamPojo(String homeTeam, String id, String seoName, String sixCharAbbr, String shortName,
			String nickName, String color) {
		super();
		this.homeTeam = homeTeam;
		this.id = id;
		this.seoName = seoName;
		this.sixCharAbbr = sixCharAbbr;
		this.shortName = shortName;
		this.nickName = nickName;
		this.color = color;
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
	 * @return the shortName
	 */
	public String getShortName() {
		return shortName;
	}

	/**
	 * @param shortName the shortName to set
	 */
	public void setShortName(String shortName) {
		this.shortName = shortName;
	}

	/**
	 * @return the nickName
	 */
	public String getNickName() {
		return nickName;
	}

	/**
	 * @param nickName the nickName to set
	 */
	public void setNickName(String nickName) {
		this.nickName = nickName;
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

	@Override
	public int hashCode() {
		return Objects.hash(color, homeTeam, id, nickName, seoName, shortName, sixCharAbbr);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BoxScoreTeamPojo)) {
			return false;
		}
		BoxScoreTeamPojo other = (BoxScoreTeamPojo) obj;
		return Objects.equals(color, other.color) && Objects.equals(homeTeam, other.homeTeam)
				&& Objects.equals(id, other.id) && Objects.equals(nickName, other.nickName)
				&& Objects.equals(seoName, other.seoName) && Objects.equals(shortName, other.shortName)
				&& Objects.equals(sixCharAbbr, other.sixCharAbbr);
	}

	@Override
	public String toString() {
		return "BoxScoreTeamPojo [homeTeam=" + homeTeam + ", id=" + id + ", seoName=" + seoName + ", sixCharAbbr="
				+ sixCharAbbr + ", shortName=" + shortName + ", nickName=" + nickName + ", color=" + color + "]";
	}
    
    
    
}
