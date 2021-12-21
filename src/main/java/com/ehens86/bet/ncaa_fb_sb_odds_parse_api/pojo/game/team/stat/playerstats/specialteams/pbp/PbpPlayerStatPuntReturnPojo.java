package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatPuntReturnPojo;

public class PbpPlayerStatPuntReturnPojo extends BaseStatPuntReturnPojo {
	private Integer puntReturnFairCatch;
	private Integer puntReturnStartYard;
	private Integer puntReturnFumble;
	private Integer puntReturnFumbleLost;
	private Integer puntReturnBlock;
	private Integer puntReturnSafety;

	public PbpPlayerStatPuntReturnPojo() {
		this.puntReturn = 1;
	}

	public PbpPlayerStatPuntReturnPojo(String playerName) {
		this.playerName = playerName;
		this.puntReturn = 1;
		this.puntReturnTouchdown = 0;
	}

	public PbpPlayerStatPuntReturnPojo(Integer puntReturnFairCatch, Integer puntReturnStartYard,
			Integer puntReturnFumble, Integer puntReturnFumbleLost, Integer puntReturnBlock, Integer puntReturnSafety) {
		super();
		this.puntReturnFairCatch = puntReturnFairCatch;
		this.puntReturnStartYard = puntReturnStartYard;
		this.puntReturnFumble = puntReturnFumble;
		this.puntReturnFumbleLost = puntReturnFumbleLost;
		this.puntReturnBlock = puntReturnBlock;
		this.puntReturnSafety = puntReturnSafety;
	}

	/**
	 * @return the puntReturnFairCatch
	 */
	public Integer getPuntReturnFairCatch() {
		return puntReturnFairCatch;
	}

	/**
	 * @param puntReturnFairCatch the puntReturnFairCatch to set
	 */
	public void setPuntReturnFairCatch(Integer puntReturnFairCatch) {
		this.puntReturnFairCatch = puntReturnFairCatch;
	}

	/**
	 * @return the puntReturnStartYard
	 */
	public Integer getPuntReturnStartYard() {
		return puntReturnStartYard;
	}

	/**
	 * @param puntReturnStartYard the puntReturnStartYard to set
	 */
	public void setPuntReturnStartYard(Integer puntReturnStartYard) {
		this.puntReturnStartYard = puntReturnStartYard;
	}

	/**
	 * @return the puntReturnFumble
	 */
	public Integer getPuntReturnFumble() {
		return puntReturnFumble;
	}

	/**
	 * @param puntReturnFumble the puntReturnFumble to set
	 */
	public void setPuntReturnFumble(Integer puntReturnFumble) {
		this.puntReturnFumble = puntReturnFumble;
	}

	/**
	 * @return the puntReturnBlock
	 */
	public Integer getPuntReturnBlock() {
		return puntReturnBlock;
	}

	/**
	 * @param puntReturnBlock the puntReturnBlock to set
	 */
	public void setPuntReturnBlock(Integer puntReturnBlock) {
		this.puntReturnBlock = puntReturnBlock;
	}

	public void applyReturnFairCatch() {
		this.puntReturnYard = 0;
		this.puntReturnFairCatch = 1;
	}

	public void applyReturnMuff() {
		this.puntReturnYard = 0;
		this.puntReturnFumble = 1;
	}

	/**
	 * @return the puntReturnFumbleLost
	 */
	public Integer getPuntReturnFumbleLost() {
		return puntReturnFumbleLost;
	}

	/**
	 * @param puntReturnFumbleLost the puntReturnFumbleLost to set
	 */
	public void setPuntReturnFumbleLost(Integer puntReturnFumbleLost) {
		this.puntReturnFumbleLost = puntReturnFumbleLost;
	}

	public void applyNoPuntReturnFumble() {
		this.puntReturnFumble = 0;
		this.puntReturnFumbleLost = 0;
	}

	public void applyPuntReturnFumble(boolean turnover) {
		this.puntReturnFumble = 1;
		if (turnover) {
			this.puntReturnFumbleLost = 1;
		} else {
			this.puntReturnFumbleLost = 0;
		}
	}

	public void applyBlockReturn(Integer puntReturnYard, Integer startYard) {
		this.puntReturnYard = puntReturnYard;
		if (Objects.isNull(this.puntReturnBlock)) {
			this.puntReturnBlock = 0;
		}
		this.puntReturnFairCatch = 0;
		this.puntReturnStartYard = startYard;
	}

	/**
	 * @return the puntReturnSafety
	 */
	public Integer getPuntReturnSafety() {
		return puntReturnSafety;
	}

	/**
	 * @param puntReturnSafety the puntReturnSafety to set
	 */
	public void setPuntReturnSafety(Integer puntReturnSafety) {
		this.puntReturnSafety = puntReturnSafety;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(puntReturnBlock, puntReturnFairCatch, puntReturnFumble,
				puntReturnFumbleLost, puntReturnSafety, puntReturnStartYard);
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
		if (!(obj instanceof PbpPlayerStatPuntReturnPojo)) {
			return false;
		}
		PbpPlayerStatPuntReturnPojo other = (PbpPlayerStatPuntReturnPojo) obj;
		return Objects.equals(puntReturnBlock, other.puntReturnBlock)
				&& Objects.equals(puntReturnFairCatch, other.puntReturnFairCatch)
				&& Objects.equals(puntReturnFumble, other.puntReturnFumble)
				&& Objects.equals(puntReturnFumbleLost, other.puntReturnFumbleLost)
				&& Objects.equals(puntReturnSafety, other.puntReturnSafety)
				&& Objects.equals(puntReturnStartYard, other.puntReturnStartYard);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatPuntReturnPojo [puntReturnFairCatch=" + puntReturnFairCatch + ", puntReturnStartYard="
				+ puntReturnStartYard + ", puntReturnFumble=" + puntReturnFumble + ", puntReturnFumbleLost="
				+ puntReturnFumbleLost + ", puntReturnBlock=" + puntReturnBlock + ", puntReturnSafety="
				+ puntReturnSafety + "]";
	}

}
