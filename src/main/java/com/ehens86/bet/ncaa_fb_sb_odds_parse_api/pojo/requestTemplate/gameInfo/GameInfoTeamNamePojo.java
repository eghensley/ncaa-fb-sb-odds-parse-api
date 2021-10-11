package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.gameInfo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GameInfoTeamNamePojo {
    @JsonProperty("6Char") 
    private String _6Char;
    @JsonProperty("short") 
    private String nameShort;
    private String seo;
    private String full;
    
    public GameInfoTeamNamePojo() {
    	
    }
    
	public GameInfoTeamNamePojo(String _6Char, String nameShort, String seo, String full) {
		super();
		this._6Char = _6Char;
		this.nameShort = nameShort;
		this.seo = seo;
		this.full = full;
	}

	/**
	 * @return the _6Char
	 */
	public String get_6Char() {
		return _6Char;
	}

	/**
	 * @param _6Char the _6Char to set
	 */
	public void set_6Char(String _6Char) {
		this._6Char = _6Char;
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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((_6Char == null) ? 0 : _6Char.hashCode());
		result = prime * result + ((full == null) ? 0 : full.hashCode());
		result = prime * result + ((nameShort == null) ? 0 : nameShort.hashCode());
		result = prime * result + ((seo == null) ? 0 : seo.hashCode());
		return result;
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
		if (_6Char == null) {
			if (other._6Char != null) {
				return false;
			}
		} else if (!_6Char.equals(other._6Char)) {
			return false;
		}
		if (full == null) {
			if (other.full != null) {
				return false;
			}
		} else if (!full.equals(other.full)) {
			return false;
		}
		if (nameShort == null) {
			if (other.nameShort != null) {
				return false;
			}
		} else if (!nameShort.equals(other.nameShort)) {
			return false;
		}
		if (seo == null) {
			if (other.seo != null) {
				return false;
			}
		} else if (!seo.equals(other.seo)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "GameInfoTeamName [_6Char=" + _6Char + ", nameShort=" + nameShort + ", seo=" + seo + ", full=" + full
				+ "]";
	}
    
    
}
