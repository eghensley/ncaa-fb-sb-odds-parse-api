package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.BaseStatRushingPojo;

public class PlayerStatRushingPojo extends BaseStatRushingPojo {
	protected Integer rushingLong;

	public PlayerStatRushingPojo() {
		
	}
	
	public PlayerStatRushingPojo(Integer rushingLong) {
		super();
		this.rushingLong = rushingLong;
	}

	/**
	 * @return the rushingLong
	 */
	public Integer getRushingLong() {
		return rushingLong;
	}

	/**
	 * @param rushingLong the rushingLong to set
	 */
	public void setRushingLong(Integer rushingLong) {
		this.rushingLong = rushingLong;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(rushingLong);
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
		if (!(obj instanceof PlayerStatRushingPojo)) {
			return false;
		}
		PlayerStatRushingPojo other = (PlayerStatRushingPojo) obj;
		return Objects.equals(rushingLong, other.rushingLong);
	}

	@Override
	public String toString() {
		return "PlayerStatRushingPojo [rushingLong=" + rushingLong + "]";
	}
	
	
}
