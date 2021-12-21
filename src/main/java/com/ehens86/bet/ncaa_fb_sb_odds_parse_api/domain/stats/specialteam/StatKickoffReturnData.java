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
@Table(name = "STAT_KICKOFF_RETURN")
public class StatKickoffReturnData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -5129907994503579780L;
	@OneToOne(mappedBy = "kickoffReturnStat")
	private TeamPlayStatData playStat;
	@Column(name = "KICKOFF_RETURN_ATTEMPT", nullable = false)
	private Integer kickoffReturn;
	@Column(name = "KICKOFF_RETURN_YARD", nullable = false)
	private Integer kickoffReturnYard;
	@Column(name = "KICKOFF_RETURN_TOUCHDOWN", nullable = false)
	private Integer kickoffReturnTouchdown;
	@Column(name = "KICKOFF_RETURN_FAIR_CATCH", nullable = false)
	private Integer kickoffReturnFairCatch;
	@Column(name = "KICKOFF_RETURN_START_YARD", nullable = false)
	private Integer kickoffReturnStartYard;
	@Column(name = "KICKOFF_RETURN_FUMBLE", nullable = false)
	private Integer kickoffReturnFumble;
	@Column(name = "KICKOFF_RETURN_FUMBLE_LOST", nullable = false)
	private Integer kickoffReturnFumbleLost;
	@Column(name = "KICKOFF_RETURN_SAFETY", nullable = false)
	private Integer kickoffReturnSafety;

	public StatKickoffReturnData() {

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
	 * @return the kickoffReturn
	 */
	public Integer getKickoffReturn() {
		return kickoffReturn;
	}

	/**
	 * @param kickoffReturn the kickoffReturn to set
	 */
	public void setKickoffReturn(Integer kickoffReturn) {
		this.kickoffReturn = kickoffReturn;
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
	 * @return the kickoffReturnFairCatch
	 */
	public Integer getKickoffReturnFairCatch() {
		return kickoffReturnFairCatch;
	}

	/**
	 * @param kickoffReturnFairCatch the kickoffReturnFairCatch to set
	 */
	public void setKickoffReturnFairCatch(Integer kickoffReturnFairCatch) {
		this.kickoffReturnFairCatch = kickoffReturnFairCatch;
	}

	/**
	 * @return the kickoffReturnStartYard
	 */
	public Integer getKickoffReturnStartYard() {
		return kickoffReturnStartYard;
	}

	/**
	 * @param kickoffReturnStartYard the kickoffReturnStartYard to set
	 */
	public void setKickoffReturnStartYard(Integer kickoffReturnStartYard) {
		this.kickoffReturnStartYard = kickoffReturnStartYard;
	}

	/**
	 * @return the kickoffReturnFumble
	 */
	public Integer getKickoffReturnFumble() {
		return kickoffReturnFumble;
	}

	/**
	 * @param kickoffReturnFumble the kickoffReturnFumble to set
	 */
	public void setKickoffReturnFumble(Integer kickoffReturnFumble) {
		this.kickoffReturnFumble = kickoffReturnFumble;
	}

	/**
	 * @return the kickoffReturnFumbleLost
	 */
	public Integer getKickoffReturnFumbleLost() {
		return kickoffReturnFumbleLost;
	}

	/**
	 * @param kickoffReturnFumbleLost the kickoffReturnFumbleLost to set
	 */
	public void setKickoffReturnFumbleLost(Integer kickoffReturnFumbleLost) {
		this.kickoffReturnFumbleLost = kickoffReturnFumbleLost;
	}

	/**
	 * @return the kickoffReturnSafety
	 */
	public Integer getKickoffReturnSafety() {
		return kickoffReturnSafety;
	}

	/**
	 * @param kickoffReturnSafety the kickoffReturnSafety to set
	 */
	public void setKickoffReturnSafety(Integer kickoffReturnSafety) {
		this.kickoffReturnSafety = kickoffReturnSafety;
	}

	/**
	 * @param kickoffReturn the kickoffReturn to set
	 */
	public void setKickReturn(Integer kickoffReturn) {
		this.kickoffReturn = kickoffReturn;
	}

	/**
	 * @param kickoffReturnYard the kickoffReturnYard to set
	 */
	public void setKickReturnYard(Integer kickoffReturnYard) {
		this.kickoffReturnYard = kickoffReturnYard;
	}

	/**
	 * @param kickoffReturnTouchdown the kickoffReturnTouchdown to set
	 */
	public void setKickReturnTouchdown(Integer kickoffReturnTouchdown) {
		this.kickoffReturnTouchdown = kickoffReturnTouchdown;
	}

	/**
	 * @param kickoffReturnFairCatch the kickoffReturnFairCatch to set
	 */
	public void setKickReturnFairCatch(Integer kickoffReturnFairCatch) {
		this.kickoffReturnFairCatch = kickoffReturnFairCatch;
	}

	/**
	 * @param kickoffReturnStartYard the kickoffReturnStartYard to set
	 */
	public void setKickReturnStartYard(Integer kickoffReturnStartYard) {
		this.kickoffReturnStartYard = kickoffReturnStartYard;
	}

	/**
	 * @param kickoffReturnFumble the kickoffReturnFumble to set
	 */
	public void setKickReturnFumble(Integer kickoffReturnFumble) {
		this.kickoffReturnFumble = kickoffReturnFumble;
	}

	/**
	 * @param kickoffReturnFumbleLost the kickoffReturnFumbleLost to set
	 */
	public void setKickReturnFumbleLost(Integer kickoffReturnFumbleLost) {
		this.kickoffReturnFumbleLost = kickoffReturnFumbleLost;
	}

	/**
	 * @param kickoffReturnSafety the kickoffReturnSafety to set
	 */
	public void setKickReturnSafety(Integer kickoffReturnSafety) {
		this.kickoffReturnSafety = kickoffReturnSafety;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(kickoffReturn, kickoffReturnFairCatch, kickoffReturnFumble,
				kickoffReturnFumbleLost, kickoffReturnSafety, kickoffReturnStartYard, kickoffReturnTouchdown,
				kickoffReturnYard, playStat);
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
		if (!(obj instanceof StatKickoffReturnData)) {
			return false;
		}
		StatKickoffReturnData other = (StatKickoffReturnData) obj;
		return Objects.equals(kickoffReturn, other.kickoffReturn)
				&& Objects.equals(kickoffReturnFairCatch, other.kickoffReturnFairCatch)
				&& Objects.equals(kickoffReturnFumble, other.kickoffReturnFumble)
				&& Objects.equals(kickoffReturnFumbleLost, other.kickoffReturnFumbleLost)
				&& Objects.equals(kickoffReturnSafety, other.kickoffReturnSafety)
				&& Objects.equals(kickoffReturnStartYard, other.kickoffReturnStartYard)
				&& Objects.equals(kickoffReturnTouchdown, other.kickoffReturnTouchdown)
				&& Objects.equals(kickoffReturnYard, other.kickoffReturnYard)
				&& Objects.equals(playStat, other.playStat);
	}

	@Override
	public String toString() {
		return "StatKickoffReturnData [playStat=" + playStat + ", kickoffReturn=" + kickoffReturn
				+ ", kickoffReturnYard=" + kickoffReturnYard + ", kickoffReturnTouchdown=" + kickoffReturnTouchdown
				+ ", kickoffReturnFairCatch=" + kickoffReturnFairCatch + ", kickoffReturnStartYard="
				+ kickoffReturnStartYard + ", kickoffReturnFumble=" + kickoffReturnFumble + ", kickoffReturnFumbleLost="
				+ kickoffReturnFumbleLost + ", kickoffReturnSafety=" + kickoffReturnSafety + "]";
	}

}
