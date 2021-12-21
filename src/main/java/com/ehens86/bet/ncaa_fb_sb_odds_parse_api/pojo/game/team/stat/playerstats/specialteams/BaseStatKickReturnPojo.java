package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams;

import java.util.Objects;

public class BaseStatKickReturnPojo {
	protected String playerName;
	protected Integer kickReturn;
	protected Integer kickReturnYard;
	protected Integer kickReturnTouchdown;


	public BaseStatKickReturnPojo() {
		this.kickReturnTouchdown = 0;
	}

	public BaseStatKickReturnPojo(String playerName) {
		this.playerName = playerName;
		this.kickReturn = 1;
		this.kickReturnTouchdown = 0;
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
	 * @return the kickReturn
	 */
	public Integer getKickReturn() {
		return kickReturn;
	}

	/**
	 * @param kickReturn the kickReturn to set
	 */
	public void setKickReturn(Integer kickReturn) {
		this.kickReturn = kickReturn;
	}

	/**
	 * @return the kickReturnYard
	 */
	public Integer getKickReturnYard() {
		return kickReturnYard;
	}

	/**
	 * @param kickReturnYard the kickReturnYard to set
	 */
	public void setKickReturnYard(Integer kickReturnYard) {
		this.kickReturnYard = kickReturnYard;
	}

	/**
	 * @return the kickReturnTouchdown
	 */
	public Integer getKickReturnTouchdown() {
		return kickReturnTouchdown;
	}

	/**
	 * @param kickReturnTouchdown the kickReturnTouchdown to set
	 */
	public void setKickReturnTouchdown(Integer kickReturnTouchdown) {
		this.kickReturnTouchdown = kickReturnTouchdown;
	}

	/**
	 * @param kickReturnTouchdown the kickReturnTouchdown to set
	 */
	public void addKickReturnTouchdown(Integer kickReturnTouchdown) {
		this.kickReturnTouchdown += kickReturnTouchdown;
	}

	@Override
	public int hashCode() {
		return Objects.hash(kickReturn, kickReturnTouchdown, kickReturnYard, playerName);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseStatKickReturnPojo)) {
			return false;
		}
		BaseStatKickReturnPojo other = (BaseStatKickReturnPojo) obj;
		return Objects.equals(kickReturn, other.kickReturn)
				&& Objects.equals(kickReturnTouchdown, other.kickReturnTouchdown)
				&& Objects.equals(kickReturnYard, other.kickReturnYard) && Objects.equals(playerName, other.playerName);
	}

	@Override
	public String toString() {
		return "BaseStatKickReturnPojo [playerName=" + playerName + ", kickReturn=" + kickReturn + ", kickReturnYard="
				+ kickReturnYard + ", kickReturnTouchdown=" + kickReturnTouchdown + "]";
	}


}
