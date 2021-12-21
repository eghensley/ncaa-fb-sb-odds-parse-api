package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.player;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatKickReturnPojo;

public class PlayerStatKickReturnPojo extends BaseStatKickReturnPojo {
	protected Integer kickReturnLong;

	public PlayerStatKickReturnPojo() {
		this.kickReturnTouchdown = 0;
	}

	public PlayerStatKickReturnPojo(String playerName) {
		this.playerName = playerName;
		this.kickReturn = 1;
		this.kickReturnTouchdown = 0;
	}

	/**
	 * @return the kickReturnLong
	 */
	public Integer getKickReturnLong() {
		return kickReturnLong;
	}

	/**
	 * @param kickReturnLong the kickReturnLong to set
	 */
	public void setKickReturnLong(Integer kickReturnLong) {
		this.kickReturnLong = kickReturnLong;
	}

	@Override
	public int hashCode() {
		return Objects.hash(kickReturn, kickReturnLong, kickReturnTouchdown, kickReturnYard, playerName);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatKickReturnPojo)) {
			return false;
		}
		PlayerStatKickReturnPojo other = (PlayerStatKickReturnPojo) obj;
		return Objects.equals(kickReturn, other.kickReturn) && Objects.equals(kickReturnLong, other.kickReturnLong)
				&& Objects.equals(kickReturnTouchdown, other.kickReturnTouchdown)
				&& Objects.equals(kickReturnYard, other.kickReturnYard) && Objects.equals(playerName, other.playerName);
	}

	@Override
	public String toString() {
		return "PlayerStatKickReturnPojo [playerName=" + playerName + ", kickReturn=" + kickReturn + ", kickReturnYard="
				+ kickReturnYard + ", kickReturnLong=" + kickReturnLong + ", kickReturnTouchdown=" + kickReturnTouchdown
				+ "]";
	}

}
