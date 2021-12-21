package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GameInfoTeamNamePojo {
    @JsonProperty("6Char") 
    private String sixChar;
    @JsonProperty("short") 
    private String nameShort;
    private String seo;
    private String full;
    
    public GameInfoTeamNamePojo() {
    	
    }
    
	public GameInfoTeamNamePojo(String sixChar, String nameShort, String seo, String full) {
		super();
		this.sixChar = sixChar;
		this.nameShort = nameShort;
		this.seo = seo;
		this.full = full;
	}

	/**
	 * @return the sixChar
	 */
	public String getSixChar() {
		return sixChar;
	}

	/**
	 * @param sixChar the sixChar to set
	 */
	public void setSixChar(String sixChar) {
		this.sixChar = sixChar;
	}

	/**
	 * @return the nameShort
	 */
	public String getNameShort() {
		return nameShort;
	}

	/**
	 * @param nameShort the nameShort to set
	 */
	public void setNameShort(String nameShort) {
		this.nameShort = nameShort;
	}

	/**
	 * @return the seo
	 */
	public String getSeo() {
		return seo;
	}

	/**
	 * @param seo the seo to set
	 */
	public void setSeo(String seo) {
		this.seo = seo;
	}

	/**
	 * @return the full
	 */
	public String getFull() {
		return full;
	}

	/**
	 * @param full the full to set
	 */
	public void setFull(String full) {
		this.full = full;
	}

	@Override
	public int hashCode() {
		return Objects.hash(full, nameShort, seo, sixChar);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameInfoTeamNamePojo)) {
			return false;
		}
		GameInfoTeamNamePojo other = (GameInfoTeamNamePojo) obj;
		return Objects.equals(full, other.full) && Objects.equals(nameShort, other.nameShort)
				&& Objects.equals(seo, other.seo) && Objects.equals(sixChar, other.sixChar);
	}

	@Override
	public String toString() {
		return "GameInfoTeamNamePojo [sixChar=" + sixChar + ", nameShort=" + nameShort + ", seo=" + seo + ", full="
				+ full + "]";
	}


    
}
