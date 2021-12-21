package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.expected;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;

public class PlayExpectedPointPojo extends PlayPojo {
	private Integer playExpectedPoints;

	public PlayExpectedPointPojo() {
		
	}
	
	public PlayExpectedPointPojo(Integer playExpectedPoints) {
		super();
		this.playExpectedPoints = playExpectedPoints;
	}

	/**
	 * @return the playExpectedPoints
	 */
	public Integer getPlayExpectedPoints() {
		return playExpectedPoints;
	}

	/**
	 * @param playExpectedPoints the playExpectedPoints to set
	 */
	public void setPlayExpectedPoints(Integer playExpectedPoints) {
		this.playExpectedPoints = playExpectedPoints;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(playExpectedPoints);
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
		if (!(obj instanceof PlayExpectedPointPojo)) {
			return false;
		}
		PlayExpectedPointPojo other = (PlayExpectedPointPojo) obj;
		return Objects.equals(playExpectedPoints, other.playExpectedPoints);
	}

	@Override
	public String toString() {
		return "PlayExpectedPointPojo [playExpectedPoints=" + playExpectedPoints + "]";
	}
	
	
}
