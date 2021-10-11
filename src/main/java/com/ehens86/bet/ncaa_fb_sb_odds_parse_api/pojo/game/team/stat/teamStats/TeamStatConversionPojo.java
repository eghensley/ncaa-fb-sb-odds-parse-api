package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

public class TeamStatConversionPojo {
	private Integer thirdDownAttempt;
	private Integer thirdDownConversion;
	private Integer fourthDownAttempt;
	private Integer fourthDownConversion;
	
	public TeamStatConversionPojo() {
		
	}

	public TeamStatConversionPojo(Integer thirdDownAttempt, Integer thirdDownConversion, Integer fourthDownAttempt,
			Integer fourthDownConversion) {
		super();
		this.thirdDownAttempt = thirdDownAttempt;
		this.thirdDownConversion = thirdDownConversion;
		this.fourthDownAttempt = fourthDownAttempt;
		this.fourthDownConversion = fourthDownConversion;
	}

	/**
	 * @return the thirdDownAttempt
	 */
	public Integer getThirdDownAttempt() {
		return thirdDownAttempt;
	}

	/**
	 * @param thirdDownAttempt the thirdDownAttempt to set
	 */
	public void setThirdDownAttempt(Integer thirdDownAttempt) {
		this.thirdDownAttempt = thirdDownAttempt;
	}

	/**
	 * @return the thirdDownConversion
	 */
	public Integer getThirdDownConversion() {
		return thirdDownConversion;
	}

	/**
	 * @param thirdDownConversion the thirdDownConversion to set
	 */
	public void setThirdDownConversion(Integer thirdDownConversion) {
		this.thirdDownConversion = thirdDownConversion;
	}

	/**
	 * @return the fourthDownAttempt
	 */
	public Integer getFourthDownAttempt() {
		return fourthDownAttempt;
	}

	/**
	 * @param fourthDownAttempt the fourthDownAttempt to set
	 */
	public void setFourthDownAttempt(Integer fourthDownAttempt) {
		this.fourthDownAttempt = fourthDownAttempt;
	}

	/**
	 * @return the fourthDownConversion
	 */
	public Integer getFourthDownConversion() {
		return fourthDownConversion;
	}

	/**
	 * @param fourthDownConversion the fourthDownConversion to set
	 */
	public void setFourthDownConversion(Integer fourthDownConversion) {
		this.fourthDownConversion = fourthDownConversion;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((fourthDownAttempt == null) ? 0 : fourthDownAttempt.hashCode());
		result = prime * result + ((fourthDownConversion == null) ? 0 : fourthDownConversion.hashCode());
		result = prime * result + ((thirdDownAttempt == null) ? 0 : thirdDownAttempt.hashCode());
		result = prime * result + ((thirdDownConversion == null) ? 0 : thirdDownConversion.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatConversionPojo)) {
			return false;
		}
		TeamStatConversionPojo other = (TeamStatConversionPojo) obj;
		if (fourthDownAttempt == null) {
			if (other.fourthDownAttempt != null) {
				return false;
			}
		} else if (!fourthDownAttempt.equals(other.fourthDownAttempt)) {
			return false;
		}
		if (fourthDownConversion == null) {
			if (other.fourthDownConversion != null) {
				return false;
			}
		} else if (!fourthDownConversion.equals(other.fourthDownConversion)) {
			return false;
		}
		if (thirdDownAttempt == null) {
			if (other.thirdDownAttempt != null) {
				return false;
			}
		} else if (!thirdDownAttempt.equals(other.thirdDownAttempt)) {
			return false;
		}
		if (thirdDownConversion == null) {
			if (other.thirdDownConversion != null) {
				return false;
			}
		} else if (!thirdDownConversion.equals(other.thirdDownConversion)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatConversionPojo [thirdDownAttempt=" + thirdDownAttempt + ", thirdDownConversion="
				+ thirdDownConversion + ", fourthDownAttempt=" + fourthDownAttempt + ", fourthDownConversion="
				+ fourthDownConversion + "]";
	}
	
	
}
