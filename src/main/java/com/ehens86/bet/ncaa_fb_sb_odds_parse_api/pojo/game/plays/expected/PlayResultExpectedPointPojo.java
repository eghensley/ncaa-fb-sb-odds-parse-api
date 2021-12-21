package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.expected;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayResultPojo;

public class PlayResultExpectedPointPojo extends PlayResultPojo {
	private Integer playResultExpectedPointsAdded;

	
	
	public PlayResultExpectedPointPojo(Integer playResultExpectedPointsAdded) {
		super();
		this.playResultExpectedPointsAdded = playResultExpectedPointsAdded;
	}

	/**
	 * @return the playResultExpectedPointsAdded
	 */
	public Integer getPlayResultExpectedPointsAdded() {
		return playResultExpectedPointsAdded;
	}

	/**
	 * @param playResultExpectedPointsAdded the playResultExpectedPointsAdded to set
	 */
	public void setPlayResultExpectedPointsAdded(Integer playResultExpectedPointsAdded) {
		this.playResultExpectedPointsAdded = playResultExpectedPointsAdded;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(playResultExpectedPointsAdded);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (!(obj instanceof PlayResultExpectedPointPojo)) {
			return false;
		}
		PlayResultExpectedPointPojo other = (PlayResultExpectedPointPojo) obj;
		return Objects.equals(playResultExpectedPointsAdded, other.playResultExpectedPointsAdded);
	}

	@Override
	public String toString() {
		return "PlayResultExpectedPointPojo [playResultExpectedPointsAdded=" + playResultExpectedPointsAdded + "]";
	}
	
	
}
