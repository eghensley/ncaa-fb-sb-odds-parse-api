package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.BaseStatPassingPojo;

public class PlayerStatPassingPojo extends BaseStatPassingPojo {
	protected Integer passingLong;

	public PlayerStatPassingPojo() {

	}

	public PlayerStatPassingPojo(Integer passingLong) {
		super();
		this.passingLong = passingLong;
	}

	/**
	 * @return the passingLong
	 */
	public Integer getPassingLong() {
		return passingLong;
	}

	/**
	 * @param passingLong the passingLong to set
	 */
	public void setPassingLong(Integer passingLong) {
		this.passingLong = passingLong;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(passingLong);
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
		if (!(obj instanceof PlayerStatPassingPojo)) {
			return false;
		}
		PlayerStatPassingPojo other = (PlayerStatPassingPojo) obj;
		return Objects.equals(passingLong, other.passingLong);
	}

}
