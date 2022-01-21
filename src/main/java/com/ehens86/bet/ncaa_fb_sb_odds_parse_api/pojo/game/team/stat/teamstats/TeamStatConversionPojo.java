package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats;

import java.util.Objects;

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
		return Objects.hash(fourthDownAttempt, fourthDownConversion, thirdDownAttempt, thirdDownConversion);
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
		return Objects.equals(fourthDownAttempt, other.fourthDownAttempt)
				&& Objects.equals(fourthDownConversion, other.fourthDownConversion)
				&& Objects.equals(thirdDownAttempt, other.thirdDownAttempt)
				&& Objects.equals(thirdDownConversion, other.thirdDownConversion);
	}

	@Override
	public String toString() {
		return "TeamStatConversionPojo [thirdDownAttempt=" + thirdDownAttempt + ", thirdDownConversion="
				+ thirdDownConversion + ", fourthDownAttempt=" + fourthDownAttempt + ", fourthDownConversion="
				+ fourthDownConversion + "]";
	}
	
	
}
