package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatPuntingPojo;

public class PbpPlayerStatPuntingPojo extends BaseStatPuntingPojo {
	private Integer puntBlocked;
	private Integer puntReturnYard;
	private Integer puntReturnTouchdown;
	private Integer puntFairCatch;
	private Integer puntTouchback;
	private Integer puntLandYard;
	
	public PbpPlayerStatPuntingPojo() {
		this.punt = 1;
	}
	
	
	public PbpPlayerStatPuntingPojo(Integer puntBlocked, Integer puntReturnYard, Integer puntReturnTouchdown,
			Integer puntFairCatch, Integer puntTouchback, Integer puntLandYard) {
		super();
		this.puntBlocked = puntBlocked;
		this.puntReturnYard = puntReturnYard;
		this.puntReturnTouchdown = puntReturnTouchdown;
		this.puntFairCatch = puntFairCatch;
		this.puntTouchback = puntTouchback;
		this.puntLandYard = puntLandYard;
	}

	/**
	 * @return the puntBlocked
	 */
	public Integer getPuntBlocked() {
		return puntBlocked;
	}

	/**
	 * @param puntBlocked the puntBlocked to set
	 */
	public void setPuntBlocked(Integer puntBlocked) {
		this.puntBlocked = puntBlocked;
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
	 * @return the puntFairCatch
	 */
	public Integer getPuntFairCatch() {
		return puntFairCatch;
	}

	/**
	 * @param puntFairCatch the puntFairCatch to set
	 */
	public void setPuntFairCatch(Integer puntFairCatch) {
		this.puntFairCatch = puntFairCatch;
	}
	
	/**
	 * @return the puntTouchback
	 */
	public Integer getPuntTouchback() {
		return puntTouchback;
	}

	/**
	 * @param puntTouchback the puntTouchback to set
	 */
	public void setPuntTouchback(Integer puntTouchback) {
		this.puntTouchback = puntTouchback;
	}
	
	public void applyPuntTouchback() {
		this.puntTouchback = 1;
		this.puntFairCatch = 0;
		this.puntReturnYard = 25;
		this.puntReturnTouchdown = 0;
		this.puntBlocked = 0;
	}
	
	public void applyPuntFairCatch() {
		this.puntTouchback = 0;
		this.puntFairCatch = 1;
		this.puntReturnYard = 0;
		this.puntReturnTouchdown = 0;
		this.puntBlocked = 0;
	}
	
	public void applyPuntReturn(Integer returnYard) {
		this.puntTouchback = 0;
		this.puntFairCatch = 0;
		this.puntReturnYard = returnYard;
		this.puntBlocked = 0;
	}

	/**
	 * @return the puntLandYard
	 */
	public Integer getPuntLandYard() {
		return puntLandYard;
	}

	/**
	 * @param puntLandYard the puntLandYard to set
	 */
	public void setPuntLandYard(Integer puntLandYard) {
		this.puntLandYard = puntLandYard;
	}


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(puntBlocked, puntFairCatch, puntLandYard, puntReturnTouchdown,
				puntReturnYard, puntTouchback);
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
		if (!(obj instanceof PbpPlayerStatPuntingPojo)) {
			return false;
		}
		PbpPlayerStatPuntingPojo other = (PbpPlayerStatPuntingPojo) obj;
		return Objects.equals(puntBlocked, other.puntBlocked) && Objects.equals(puntFairCatch, other.puntFairCatch)
				&& Objects.equals(puntLandYard, other.puntLandYard)
				&& Objects.equals(puntReturnTouchdown, other.puntReturnTouchdown)
				&& Objects.equals(puntReturnYard, other.puntReturnYard)
				&& Objects.equals(puntTouchback, other.puntTouchback);
	}


	@Override
	public String toString() {
		return "PbpPlayerStatPuntingPojo [puntBlocked=" + puntBlocked + ", puntReturnYard=" + puntReturnYard
				+ ", puntReturnTouchdown=" + puntReturnTouchdown + ", puntFairCatch=" + puntFairCatch
				+ ", puntTouchback=" + puntTouchback + ", puntLandYard=" + puntLandYard + "]";
	}
	
	
}
