package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.PlayerStatEntity;

@Entity
@Table(name = "STAT_PUNT_RETURN")
public class StatPuntReturnData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -1973160829504446440L;
	@OneToOne(mappedBy = "puntReturnStat")
	private TeamPlayStatData playStat;
	@Column(name = "PUNT_RETURN_ATTEMPT", nullable = false)
	private Integer puntReturn;
	@Column(name = "PUNT_RETURN_YARD", nullable = false)
	private Integer puntReturnYard;
	@Column(name = "PUNT_RETURN_TOUCHDOWN", nullable = false)
	private Integer puntReturnTouchdown;
	@Column(name = "PUNT_RETURN_FAIR_CATCH", nullable = false)
	private Integer puntReturnFairCatch;
	@Column(name = "PUNT_RETURN_START_YARD", nullable = false)
	private Integer puntReturnStartYard;
	@Column(name = "PUNT_RETURN_FUMBLE", nullable = false)
	private Integer puntReturnFumble;
	@Column(name = "PUNT_RETURN_FUMBLE_LOST", nullable = false)
	private Integer puntReturnFumbleLost;
	@Column(name = "PUNT_RETURN_BLOCK", nullable = false)
	private Integer puntReturnBlock;
	@Column(name = "PUNT_RETURN_SAFETY", nullable = false)
	private Integer puntReturnSafety;

	public StatPuntReturnData() {

	}

//	/**
//	 * @return the playStat
//	 */
//	public TeamPlayStatData getPlayStat() {
//		return playStat;
//	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
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
		result = prime * result + Objects.hash(playStat, puntReturn, puntReturnBlock, puntReturnFairCatch,
				puntReturnFumble, puntReturnFumbleLost, puntReturnSafety, puntReturnStartYard, puntReturnTouchdown,
				puntReturnYard);
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
		if (!(obj instanceof StatPuntReturnData)) {
			return false;
		}
		StatPuntReturnData other = (StatPuntReturnData) obj;
		return Objects.equals(playStat, other.playStat) && Objects.equals(puntReturn, other.puntReturn)
				&& Objects.equals(puntReturnBlock, other.puntReturnBlock)
				&& Objects.equals(puntReturnFairCatch, other.puntReturnFairCatch)
				&& Objects.equals(puntReturnFumble, other.puntReturnFumble)
				&& Objects.equals(puntReturnFumbleLost, other.puntReturnFumbleLost)
				&& Objects.equals(puntReturnSafety, other.puntReturnSafety)
				&& Objects.equals(puntReturnStartYard, other.puntReturnStartYard)
				&& Objects.equals(puntReturnTouchdown, other.puntReturnTouchdown)
				&& Objects.equals(puntReturnYard, other.puntReturnYard);
	}

	@Override
	public String toString() {
		return "StatPuntReturnData [playStat=" + playStat + ", puntReturn=" + puntReturn + ", puntReturnYard="
				+ puntReturnYard + ", puntReturnTouchdown=" + puntReturnTouchdown + ", puntReturnFairCatch="
				+ puntReturnFairCatch + ", puntReturnStartYard=" + puntReturnStartYard + ", puntReturnFumble="
				+ puntReturnFumble + ", puntReturnFumbleLost=" + puntReturnFumbleLost + ", puntReturnBlock="
				+ puntReturnBlock + ", puntReturnSafety=" + puntReturnSafety + "]";
	}

}
