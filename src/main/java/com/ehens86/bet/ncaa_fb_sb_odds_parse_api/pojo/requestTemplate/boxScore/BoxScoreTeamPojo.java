package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore;

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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((color == null) ? 0 : color.hashCode());
		result = prime * result + ((homeTeam == null) ? 0 : homeTeam.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((nickName == null) ? 0 : nickName.hashCode());
		result = prime * result + ((seoName == null) ? 0 : seoName.hashCode());
		result = prime * result + ((shortName == null) ? 0 : shortName.hashCode());
		result = prime * result + ((sixCharAbbr == null) ? 0 : sixCharAbbr.hashCode());
		return result;
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
		if (color == null) {
			if (other.color != null) {
				return false;
			}
		} else if (!color.equals(other.color)) {
			return false;
		}
		if (homeTeam == null) {
			if (other.homeTeam != null) {
				return false;
			}
		} else if (!homeTeam.equals(other.homeTeam)) {
			return false;
		}
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if (!id.equals(other.id)) {
			return false;
		}
		if (nickName == null) {
			if (other.nickName != null) {
				return false;
			}
		} else if (!nickName.equals(other.nickName)) {
			return false;
		}
		if (seoName == null) {
			if (other.seoName != null) {
				return false;
			}
		} else if (!seoName.equals(other.seoName)) {
			return false;
		}
		if (shortName == null) {
			if (other.shortName != null) {
				return false;
			}
		} else if (!shortName.equals(other.shortName)) {
			return false;
		}
		if (sixCharAbbr == null) {
			if (other.sixCharAbbr != null) {
				return false;
			}
		} else if (!sixCharAbbr.equals(other.sixCharAbbr)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "BoxScoreTeamPojo [homeTeam=" + homeTeam + ", id=" + id + ", seoName=" + seoName + ", sixCharAbbr="
				+ sixCharAbbr + ", shortName=" + shortName + ", nickName=" + nickName + ", color=" + color + "]";
	}
    
    
    
}
