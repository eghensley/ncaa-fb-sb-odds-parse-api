package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatKickReturnPojo;

public class PbpPlayerStatKickReturnPojo extends BaseStatKickReturnPojo {
	private Integer kickReturnFairCatch;
	private Integer kickReturnStartYard;
	private Integer kickReturnFumble;
	private Integer kickReturnFumbleLost;
	private Integer kickReturnSafety;

	public PbpPlayerStatKickReturnPojo() {
		this.kickReturn = 1;
	}

	public PbpPlayerStatKickReturnPojo(String playerName) {
		this.playerName = playerName;
		this.kickReturn = 1;
		this.kickReturnTouchdown = 0;
	}
	
	public PbpPlayerStatKickReturnPojo(Integer kickReturnFairCatch, Integer kickReturnStartYard,
			Integer kickReturnFumble, Integer kickReturnFumbleLost, Integer kickReturnSafety) {
		super();
		this.kickReturnFairCatch = kickReturnFairCatch;
		this.kickReturnStartYard = kickReturnStartYard;
		this.kickReturnFumble = kickReturnFumble;
		this.kickReturnFumbleLost = kickReturnFumbleLost;
		this.kickReturnSafety = kickReturnSafety;
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

	public void applyNoKickReturnFumble() {
		this.kickReturnFumble = 0;
		this.kickReturnFumbleLost = 0;
	}

	public void applyKickReturnFumble(boolean turnover) {
		this.kickReturnFumble = 1;
		if (turnover) {
			this.kickReturnFumbleLost = 1;
		} else {
			this.kickReturnFumbleLost = 0;
		}
	}

	/**
	 * @return the kickReturnSafety
	 */
	public Integer getKickReturnSafety() {
		return kickReturnSafety;
	}

	/**
	 * @param kickReturnSafety the kickReturnSafety to set
	 */
	public void setKickReturnSafety(Integer kickReturnSafety) {
		this.kickReturnSafety = kickReturnSafety;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(kickReturnFairCatch, kickReturnFumble, kickReturnFumbleLost,
				kickReturnSafety, kickReturnStartYard);
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
		if (!(obj instanceof PbpPlayerStatKickReturnPojo)) {
			return false;
		}
		PbpPlayerStatKickReturnPojo other = (PbpPlayerStatKickReturnPojo) obj;
		return Objects.equals(kickReturnFairCatch, other.kickReturnFairCatch)
				&& Objects.equals(kickReturnFumble, other.kickReturnFumble)
				&& Objects.equals(kickReturnFumbleLost, other.kickReturnFumbleLost)
				&& Objects.equals(kickReturnSafety, other.kickReturnSafety)
				&& Objects.equals(kickReturnStartYard, other.kickReturnStartYard);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatKickReturnPojo [kickReturnFairCatch=" + kickReturnFairCatch + ", kickReturnStartYard="
				+ kickReturnStartYard + ", kickReturnFumble=" + kickReturnFumble + ", kickReturnFumbleLost="
				+ kickReturnFumbleLost + ", kickReturnSafety=" + kickReturnSafety + "]";
	}

}
