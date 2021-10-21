package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams;

public class PlayerStatKickReturnPojo {
	private String playerName;
	private Integer kickReturn;
	private Integer kickReturnYard;
	private Integer kickReturnLong;
	private Integer kickReturnTouchdown;
	private Integer kickReturnFairCatch;
	private Integer kickReturnStartYard;
	private Integer kickReturnFumble;
	private Integer kickReturnFumbleLost;

	public PlayerStatKickReturnPojo() {
		this.kickReturnTouchdown = 0;
	}

	public PlayerStatKickReturnPojo(String playerName) {
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

	/**
	 * @return the kickReturnFairCatch
	 */
	public Integer getKickReturnFairCatch() {
		return kickReturnFairCatch;
	}

	/**
	 * @param kickReturnFairCatch the kickReturnFairCatch to set
	 */
	public void setKickReturnFairCatch(Integer kickReturnFairCatch) {
		this.kickReturnFairCatch = kickReturnFairCatch;
	}

	/**
	 * @return the kickReturnStartYard
	 */
	public Integer getKickReturnStartYard() {
		return kickReturnStartYard;
	}

	/**
	 * @param kickReturnStartYard the kickReturnStartYard to set
	 */
	public void setKickReturnStartYard(Integer kickReturnStartYard) {
		this.kickReturnStartYard = kickReturnStartYard;
	}

	/**
	 * @return the kickReturnFumble
	 */
	public Integer getKickReturnFumble() {
		return kickReturnFumble;
	}

	/**
	 * @param kickReturnFumble the kickReturnFumble to set
	 */
	public void setKickReturnFumble(Integer kickReturnFumble) {
		this.kickReturnFumble = kickReturnFumble;
	}

	/**
	 * @return the kickReturnFumbleLost
	 */
	public Integer getKickReturnFumbleLost() {
		return kickReturnFumbleLost;
	}

	/**
	 * @param kickReturnFumbleLost the kickReturnFumbleLost to set
	 */
	public void setKickReturnFumbleLost(Integer kickReturnFumbleLost) {
		this.kickReturnFumbleLost = kickReturnFumbleLost;
	}

	public void applyReturnFairCatch(Integer kickLandYard) {
		this.kickReturnFairCatch = 1;
		this.kickReturnStartYard = 100 - kickLandYard;
		this.kickReturnYard = 25 - this.kickReturnStartYard;
	}

	public void applyReturnMuff() {
		this.kickReturnYard = 0;
		this.kickReturnFumble = 1;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((kickReturn == null) ? 0 : kickReturn.hashCode());
		result = prime * result + ((kickReturnFairCatch == null) ? 0 : kickReturnFairCatch.hashCode());
		result = prime * result + ((kickReturnFumble == null) ? 0 : kickReturnFumble.hashCode());
		result = prime * result + ((kickReturnFumbleLost == null) ? 0 : kickReturnFumbleLost.hashCode());
		result = prime * result + ((kickReturnLong == null) ? 0 : kickReturnLong.hashCode());
		result = prime * result + ((kickReturnStartYard == null) ? 0 : kickReturnStartYard.hashCode());
		result = prime * result + ((kickReturnTouchdown == null) ? 0 : kickReturnTouchdown.hashCode());
		result = prime * result + ((kickReturnYard == null) ? 0 : kickReturnYard.hashCode());
		result = prime * result + ((playerName == null) ? 0 : playerName.hashCode());
		return result;
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
		if (kickReturn == null) {
			if (other.kickReturn != null) {
				return false;
			}
		} else if (!kickReturn.equals(other.kickReturn)) {
			return false;
		}
		if (kickReturnFairCatch == null) {
			if (other.kickReturnFairCatch != null) {
				return false;
			}
		} else if (!kickReturnFairCatch.equals(other.kickReturnFairCatch)) {
			return false;
		}
		if (kickReturnFumble == null) {
			if (other.kickReturnFumble != null) {
				return false;
			}
		} else if (!kickReturnFumble.equals(other.kickReturnFumble)) {
			return false;
		}
		if (kickReturnFumbleLost == null) {
			if (other.kickReturnFumbleLost != null) {
				return false;
			}
		} else if (!kickReturnFumbleLost.equals(other.kickReturnFumbleLost)) {
			return false;
		}
		if (kickReturnLong == null) {
			if (other.kickReturnLong != null) {
				return false;
			}
		} else if (!kickReturnLong.equals(other.kickReturnLong)) {
			return false;
		}
		if (kickReturnStartYard == null) {
			if (other.kickReturnStartYard != null) {
				return false;
			}
		} else if (!kickReturnStartYard.equals(other.kickReturnStartYard)) {
			return false;
		}
		if (kickReturnTouchdown == null) {
			if (other.kickReturnTouchdown != null) {
				return false;
			}
		} else if (!kickReturnTouchdown.equals(other.kickReturnTouchdown)) {
			return false;
		}
		if (kickReturnYard == null) {
			if (other.kickReturnYard != null) {
				return false;
			}
		} else if (!kickReturnYard.equals(other.kickReturnYard)) {
			return false;
		}
		if (playerName == null) {
			if (other.playerName != null) {
				return false;
			}
		} else if (!playerName.equals(other.playerName)) {
			return false;
		}
		return true;
	}

}
