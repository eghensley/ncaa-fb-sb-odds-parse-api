package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams;

import java.util.Objects;

public class PlayerStatKickoffPojo {
	protected String playerName;
	protected Integer kickoffYard;

	public PlayerStatKickoffPojo() {
		// Constructor
	}

	public PlayerStatKickoffPojo(String playerName, Integer kickoffYard) {
		super();
		this.playerName = playerName;
		this.kickoffYard = kickoffYard;
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
	 * @return the kickoffYard
	 */
	public Integer getKickoffYard() {
		return kickoffYard;
	}

	/**
	 * @param kickoffYard the kickoffYard to set
	 */
	public void setKickoffYard(Integer kickoffYard) {
		this.kickoffYard = kickoffYard;
	}

	@Override
	public int hashCode() {
		return Objects.hash(kickoffYard, playerName);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatKickoffPojo)) {
			return false;
		}
		PlayerStatKickoffPojo other = (PlayerStatKickoffPojo) obj;
		return Objects.equals(kickoffYard, other.kickoffYard) && Objects.equals(playerName, other.playerName);
	}

	@Override
	public String toString() {
		return "PlayerStatKickoffPojo [playerName=" + playerName + ", kickoffYard=" + kickoffYard + "]";
	}

}
