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
@Table(name = "STAT_KICKOFF")
public class StatKickoffData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 4131829792945944123L;
	@OneToOne(mappedBy = "kickoffStat")
	private TeamPlayStatData playStat;
	@Column(name = "KICKOFF_YARD", nullable = false)
	private Integer kickoffYard;
	@Column(name = "KICKOFF_TOUCHBACK", nullable = false)
	private Integer kickoffTouchback;
	@Column(name = "KICKOFF_ONSIDE_ATTEMPT", nullable = false)
	private Integer kickoffOnsideAttempt;
	@Column(name = "KICKOFF_ONSIDE_SUCCESS", nullable = false)
	private Integer kickoffOnsideSuccess;
	@Column(name = "KICKOFF_RETURN_YARD", nullable = false)
	private Integer kickoffReturnYard;
	@Column(name = "KICKOFF_OUT_OF_BOUNDS", nullable = false)
	private Integer kickoffOutOfBounds;
	@Column(name = "KICKOFF_RETURN_TOUCHDOWN", nullable = false)
	private Integer kickoffReturnTouchdown;
	@Column(name = "KICKOFF_RETURN_FAIR_CATCH", nullable = false)
	private Integer kickoffFairCatch;
	@Column(name = "KICKOFF_ATTEMPT", nullable = false)
	private Integer kickoff;
	@Column(name = "KICKOFF_LAND_YARD", nullable = false)
	private Integer kickoffLandYard;

	public StatKickoffData() {
		// Base constructor
	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
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

	/**
	 * @return the kickoffTouchback
	 */
	public Integer getKickoffTouchback() {
		return kickoffTouchback;
	}

	/**
	 * @param kickoffTouchback the kickoffTouchback to set
	 */
	public void setKickoffTouchback(Integer kickoffTouchback) {
		this.kickoffTouchback = kickoffTouchback;
	}

	/**
	 * @return the kickoffOnsideAttempt
	 */
	public Integer getKickoffOnsideAttempt() {
		return kickoffOnsideAttempt;
	}

	/**
	 * @param kickoffOnsideAttempt the kickoffOnsideAttempt to set
	 */
	public void setKickoffOnsideAttempt(Integer kickoffOnsideAttempt) {
		this.kickoffOnsideAttempt = kickoffOnsideAttempt;
	}

	/**
	 * @return the kickoffOnsideSuccess
	 */
	public Integer getKickoffOnsideSuccess() {
		return kickoffOnsideSuccess;
	}

	/**
	 * @param kickoffOnsideSuccess the kickoffOnsideSuccess to set
	 */
	public void setKickoffOnsideSuccess(Integer kickoffOnsideSuccess) {
		this.kickoffOnsideSuccess = kickoffOnsideSuccess;
	}

	/**
	 * @return the kickoffReturnYard
	 */
	public Integer getKickoffReturnYard() {
		return kickoffReturnYard;
	}

	/**
	 * @param kickoffReturnYard the kickoffReturnYard to set
	 */
	public void setKickoffReturnYard(Integer kickoffReturnYard) {
		this.kickoffReturnYard = kickoffReturnYard;
	}

	/**
	 * @return the kickoffOutOfBounds
	 */
	public Integer getKickoffOutOfBounds() {
		return kickoffOutOfBounds;
	}

	/**
	 * @param kickoffOutOfBounds the kickoffOutOfBounds to set
	 */
	public void setKickoffOutOfBounds(Integer kickoffOutOfBounds) {
		this.kickoffOutOfBounds = kickoffOutOfBounds;
	}

	/**
	 * @return the kickoffReturnTouchdown
	 */
	public Integer getKickoffReturnTouchdown() {
		return kickoffReturnTouchdown;
	}

	/**
	 * @param kickoffReturnTouchdown the kickoffReturnTouchdown to set
	 */
	public void setKickoffReturnTouchdown(Integer kickoffReturnTouchdown) {
		this.kickoffReturnTouchdown = kickoffReturnTouchdown;
	}

	/**
	 * @return the kickoffFairCatch
	 */
	public Integer getKickoffFairCatch() {
		return kickoffFairCatch;
	}

	/**
	 * @param kickoffFairCatch the kickoffFairCatch to set
	 */
	public void setKickoffFairCatch(Integer kickoffFairCatch) {
		this.kickoffFairCatch = kickoffFairCatch;
	}

	/**
	 * @return the kickoff
	 */
	public Integer getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(Integer kickoff) {
		this.kickoff = kickoff;
	}

	/**
	 * @return the kickoffLandYard
	 */
	public Integer getKickoffLandYard() {
		return kickoffLandYard;
	}

	/**
	 * @param kickoffLandYard the kickoffLandYard to set
	 */
	public void setKickoffLandYard(Integer kickoffLandYard) {
		this.kickoffLandYard = kickoffLandYard;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(kickoff, kickoffFairCatch, kickoffLandYard, kickoffOnsideAttempt,
				kickoffOnsideSuccess, kickoffOutOfBounds, kickoffReturnTouchdown, kickoffReturnYard, kickoffTouchback,
				kickoffYard, playStat);
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
		if (!(obj instanceof StatKickoffData)) {
			return false;
		}
		StatKickoffData other = (StatKickoffData) obj;
		return Objects.equals(kickoff, other.kickoff) && Objects.equals(kickoffFairCatch, other.kickoffFairCatch)
				&& Objects.equals(kickoffLandYard, other.kickoffLandYard)
				&& Objects.equals(kickoffOnsideAttempt, other.kickoffOnsideAttempt)
				&& Objects.equals(kickoffOnsideSuccess, other.kickoffOnsideSuccess)
				&& Objects.equals(kickoffOutOfBounds, other.kickoffOutOfBounds)
				&& Objects.equals(kickoffReturnTouchdown, other.kickoffReturnTouchdown)
				&& Objects.equals(kickoffReturnYard, other.kickoffReturnYard)
				&& Objects.equals(kickoffTouchback, other.kickoffTouchback)
				&& Objects.equals(kickoffYard, other.kickoffYard) && Objects.equals(playStat, other.playStat);
	}

	@Override
	public String toString() {
		return "StatKickoffData [playStat=" + playStat + ", kickoffYard=" + kickoffYard + ", kickoffTouchback="
				+ kickoffTouchback + ", kickoffOnsideAttempt=" + kickoffOnsideAttempt + ", kickoffOnsideSuccess="
				+ kickoffOnsideSuccess + ", kickoffReturnYard=" + kickoffReturnYard + ", kickoffOutOfBounds="
				+ kickoffOutOfBounds + ", kickoffReturnTouchdown=" + kickoffReturnTouchdown + ", kickoffFairCatch="
				+ kickoffFairCatch + ", kickoff=" + kickoff + ", kickoffLandYard=" + kickoffLandYard + "]";
	}

}
