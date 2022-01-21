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
@Table(name = "STAT_PUNT")
public class StatPuntData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -7374157476774962138L;
	@OneToOne(mappedBy = "puntStat")
	private TeamPlayStatData playStat;
	@Column(name = "PUNT_ATTEMPT", nullable = false)
	private Integer punt;
	@Column(name = "PUNT_YARD", nullable = false)
	private Integer puntYard;
	@Column(name = "PUNT_BLOCK", nullable = false)
	private Integer puntBlocked;
	@Column(name = "PUNT_RETURN_YARD", nullable = false)
	private Integer puntReturnYard;
	@Column(name = "PUNT_RETURN_TOUCHDOWN", nullable = false)
	private Integer puntReturnTouchdown;
	@Column(name = "PUNT_FAIR_CATCH", nullable = false)
	private Integer puntFairCatch;
	@Column(name = "PUNT_TOUCHBACK", nullable = false)
	private Integer puntTouchback;
	@Column(name = "PUNT_LAND_YARD", nullable = false)
	private Integer puntLandYard;
	@Column(name = "PUNT_INSIDE_TWENTY", nullable = false)
	private Integer puntInTwenty;
	@Column(name = "PUNT_INSIDE_TEN", nullable = false)
	private Integer puntInTen;
	@Column(name = "PUNT_INSIDE_FIVE", nullable = false)
	private Integer puntInFive;

	public StatPuntData() {
		// Base constructor
	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
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

	/**
	 * @return the puntInTwenty
	 */
	public Integer getPuntInTwenty() {
		return puntInTwenty;
	}

	/**
	 * @param puntInTwenty the puntInTwenty to set
	 */
	public void setPuntInTwenty(Integer puntInTwenty) {
		this.puntInTwenty = puntInTwenty;
	}

	/**
	 * @return the puntInTen
	 */
	public Integer getPuntInTen() {
		return puntInTen;
	}

	/**
	 * @param puntInTen the puntInTen to set
	 */
	public void setPuntInTen(Integer puntInTen) {
		this.puntInTen = puntInTen;
	}

	/**
	 * @return the puntInFive
	 */
	public Integer getPuntInFive() {
		return puntInFive;
	}

	/**
	 * @param puntInFive the puntInFive to set
	 */
	public void setPuntInFive(Integer puntInFive) {
		this.puntInFive = puntInFive;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(playStat, punt, puntBlocked, puntFairCatch, puntInFive, puntInTen,
				puntInTwenty, puntLandYard, puntReturnTouchdown, puntReturnYard, puntTouchback, puntYard);
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
		if (!(obj instanceof StatPuntData)) {
			return false;
		}
		StatPuntData other = (StatPuntData) obj;
		return Objects.equals(playStat, other.playStat) && Objects.equals(punt, other.punt)
				&& Objects.equals(puntBlocked, other.puntBlocked) && Objects.equals(puntFairCatch, other.puntFairCatch)
				&& Objects.equals(puntInFive, other.puntInFive) && Objects.equals(puntInTen, other.puntInTen)
				&& Objects.equals(puntInTwenty, other.puntInTwenty) && Objects.equals(puntLandYard, other.puntLandYard)
				&& Objects.equals(puntReturnTouchdown, other.puntReturnTouchdown)
				&& Objects.equals(puntReturnYard, other.puntReturnYard)
				&& Objects.equals(puntTouchback, other.puntTouchback) && Objects.equals(puntYard, other.puntYard);
	}

	@Override
	public String toString() {
		return "StatPuntData [playStat=" + playStat + ", punt=" + punt + ", puntYard=" + puntYard + ", puntBlocked="
				+ puntBlocked + ", puntReturnYard=" + puntReturnYard + ", puntReturnTouchdown=" + puntReturnTouchdown
				+ ", puntFairCatch=" + puntFairCatch + ", puntTouchback=" + puntTouchback + ", puntLandYard="
				+ puntLandYard + ", puntInTwenty=" + puntInTwenty + ", puntInTen=" + puntInTen + ", puntInFive="
				+ puntInFive + "]";
	}

}
