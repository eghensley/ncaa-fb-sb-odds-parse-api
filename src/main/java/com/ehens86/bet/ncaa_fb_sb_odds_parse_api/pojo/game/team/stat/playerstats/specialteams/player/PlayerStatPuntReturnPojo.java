package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatPuntReturnPojo;

public class PlayerStatPuntReturnPojo extends BaseStatPuntReturnPojo {
	protected Integer puntReturnLong;


	public PlayerStatPuntReturnPojo() {
		this.puntReturnTouchdown = 0;
	}

	public PlayerStatPuntReturnPojo(String playerName) {
		this.playerName = playerName;
		this.puntReturn = 1;
		this.puntReturnTouchdown = 0;
	}

	/**
	 * @return the puntReturnLong
	 */
	public Integer getPuntReturnLong() {
		return puntReturnLong;
	}

	/**
	 * @param puntReturnLong the puntReturnLong to set
	 */
	public void setPuntReturnLong(Integer puntReturnLong) {
		this.puntReturnLong = puntReturnLong;
	}

	@Override
	public int hashCode() {
		return Objects.hash(playerName, puntReturn, puntReturnLong, puntReturnTouchdown, puntReturnYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatPuntReturnPojo)) {
			return false;
		}
		PlayerStatPuntReturnPojo other = (PlayerStatPuntReturnPojo) obj;
		return Objects.equals(playerName, other.playerName) && Objects.equals(puntReturn, other.puntReturn)
				&& Objects.equals(puntReturnLong, other.puntReturnLong)
				&& Objects.equals(puntReturnTouchdown, other.puntReturnTouchdown)
				&& Objects.equals(puntReturnYard, other.puntReturnYard);
	}

	@Override
	public String toString() {
		return "PlayerStatPuntReturnPojo [playerName=" + playerName + ", puntReturn=" + puntReturn + ", puntReturnYard="
				+ puntReturnYard + ", puntReturnLong=" + puntReturnLong + ", puntReturnTouchdown=" + puntReturnTouchdown
				+ "]";
	}




}
