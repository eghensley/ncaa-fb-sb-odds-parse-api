package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams;

import java.util.Objects;

public class BaseStatPuntReturnPojo {
	protected String playerName;
	protected Integer puntReturn;
	protected Integer puntReturnYard;
	protected Integer puntReturnTouchdown;


	public BaseStatPuntReturnPojo() {
		this.puntReturnTouchdown = 0;
	}

	public BaseStatPuntReturnPojo(String playerName) {
		this.playerName = playerName;
		this.puntReturn = 1;
		this.puntReturnTouchdown = 0;
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
	 * @return the puntReturn
	 */
	public Integer getPuntReturn() {
		return puntReturn;
	}

	/**
	 * @param puntReturn the puntReturn to set
	 */
	public void setPuntReturn(Integer puntReturn) {
		this.puntReturn = puntReturn;
	}

	/**
	 * @return the puntReturnYard
	 */
	public Integer getPuntReturnYard() {
		return puntReturnYard;
	}

	/**
	 * @param puntReturnYard the puntReturnYard to set
	 */
	public void setPuntReturnYard(Integer puntReturnYard) {
		this.puntReturnYard = puntReturnYard;
	}

	/**
	 * @return the puntReturnTouchdown
	 */
	public Integer getPuntReturnTouchdown() {
		return puntReturnTouchdown;
	}

	/**
	 * @param puntReturnTouchdown the puntReturnTouchdown to set
	 */
	public void setPuntReturnTouchdown(Integer puntReturnTouchdown) {
		this.puntReturnTouchdown = puntReturnTouchdown;
	}

	/**
	 * @param puntReturnTouchdown the puntReturnTouchdown to set
	 */
	public void addPuntReturnTouchdown(Integer puntReturnTouchdown) {
		this.puntReturnTouchdown += puntReturnTouchdown;
	}

	@Override
	public int hashCode() {
		return Objects.hash(playerName, puntReturn, puntReturnTouchdown, puntReturnYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseStatPuntReturnPojo)) {
			return false;
		}
		BaseStatPuntReturnPojo other = (BaseStatPuntReturnPojo) obj;
		return Objects.equals(playerName, other.playerName) && Objects.equals(puntReturn, other.puntReturn)
				&& Objects.equals(puntReturnTouchdown, other.puntReturnTouchdown)
				&& Objects.equals(puntReturnYard, other.puntReturnYard);
	}

	@Override
	public String toString() {
		return "BaseStatPuntReturnPojo [playerName=" + playerName + ", puntReturn=" + puntReturn + ", puntReturnYard="
				+ puntReturnYard + ", puntReturnTouchdown=" + puntReturnTouchdown + "]";
	}



}
