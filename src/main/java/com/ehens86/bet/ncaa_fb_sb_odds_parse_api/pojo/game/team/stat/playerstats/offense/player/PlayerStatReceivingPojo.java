package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.BaseStatReceivingPojo;

public class PlayerStatReceivingPojo extends BaseStatReceivingPojo {
	protected Integer receivingLong;


	public PlayerStatReceivingPojo() {

	}




	public PlayerStatReceivingPojo(Integer receivingLong) {
		super();
		this.receivingLong = receivingLong;
	}




	/**
	 * @return the receivingLong
	 */
	public Integer getReceivingLong() {
		return receivingLong;
	}

	/**
	 * @param receivingLong the receivingLong to set
	 */
	public void setReceivingLong(Integer receivingLong) {
		this.receivingLong = receivingLong;
	}




	@Override
	public int hashCode() {
		return Objects.hash(receivingLong);
	}




	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatReceivingPojo)) {
			return false;
		}
		PlayerStatReceivingPojo other = (PlayerStatReceivingPojo) obj;
		return Objects.equals(receivingLong, other.receivingLong);
	}




	@Override
	public String toString() {
		return "PlayerStatReceivingPojo [receivingLong=" + receivingLong + "]";
	}



}
