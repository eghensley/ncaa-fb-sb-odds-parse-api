package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatPuntingPojo;

public class PlayerStatPuntingPojo extends BaseStatPuntingPojo {

	protected Integer puntLong;

	public PlayerStatPuntingPojo() {
		// Constructor
	}

	/**
	 * @return the puntLong
	 */
	public Integer getPuntLong() {
		return puntLong;
	}

	/**
	 * @param puntLong the puntLong to set
	 */
	public void setPuntLong(Integer puntLong) {
		this.puntLong = puntLong;
	}

	@Override
	public int hashCode() {
		return Objects.hash(playerName, punt, puntLong, puntYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatPuntingPojo)) {
			return false;
		}
		PlayerStatPuntingPojo other = (PlayerStatPuntingPojo) obj;
		return Objects.equals(playerName, other.playerName) && Objects.equals(punt, other.punt)
				&& Objects.equals(puntLong, other.puntLong) && Objects.equals(puntYard, other.puntYard);
	}

	@Override
	public String toString() {
		return "PlayerStatPuntingPojo [playerName=" + playerName + ", punt=" + punt + ", puntYard=" + puntYard
				+ ", puntLong=" + puntLong + "]";
	}

}
