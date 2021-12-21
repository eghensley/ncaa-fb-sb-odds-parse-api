package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams;

import java.util.Objects;

public class BaseStatPuntingPojo {
	protected String playerName;
	protected Integer punt;
	protected Integer puntYard;

	public BaseStatPuntingPojo() {
		
	}
	
	public BaseStatPuntingPojo(String playerName, Integer punt, Integer puntYard) {
		super();
		this.playerName = playerName;
		this.punt = punt;
		this.puntYard = puntYard;
	}

	/**
	 * @return the playerName
	 */
	public String getPlayerName() {
		return playerName;
	}

	/**
	 * @param playerName the playerName to set
	 */
	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}

	/**
	 * @return the punt
	 */
	public Integer getPunt() {
		return punt;
	}

	/**
	 * @param punt the punt to set
	 */
	public void setPunt(Integer punt) {
		this.punt = punt;
	}

	/**
	 * @return the puntYard
	 */
	public Integer getPuntYard() {
		return puntYard;
	}

	/**
	 * @param puntYard the puntYard to set
	 */
	public void setPuntYard(Integer puntYard) {
		this.puntYard = puntYard;
	}

	@Override
	public int hashCode() {
		return Objects.hash(playerName, punt, puntYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseStatPuntingPojo)) {
			return false;
		}
		BaseStatPuntingPojo other = (BaseStatPuntingPojo) obj;
		return Objects.equals(playerName, other.playerName) && Objects.equals(punt, other.punt)
				&& Objects.equals(puntYard, other.puntYard);
	}

	@Override
	public String toString() {
		return "BaseStatPuntingPojo [playerName=" + playerName + ", punt=" + punt + ", puntYard=" + puntYard + "]";
	}

}
