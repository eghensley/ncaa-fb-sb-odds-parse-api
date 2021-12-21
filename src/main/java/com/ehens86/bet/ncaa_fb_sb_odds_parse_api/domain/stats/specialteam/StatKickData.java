package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.PlayerStatEntity;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.KickMissReasonEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.KickTypeEnum;

@Entity
@Table(name = "STAT_KICK")
public class StatKickData extends PlayerStatEntity implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6475622031766676287L;
	@OneToOne(mappedBy = "kickStat")
	private TeamPlayStatData playStat;
	@Column(name = "KICK_SUCCESSFUL", nullable = false)
	private Integer fieldGoal;
	@Column(name = "KICK_ATTEMPT", nullable = false)
	private Integer fieldGoalAttempt;
	@Column(name = "KICK_TOTAL_POINT", nullable = false)
	private Integer totalPoint;
	@Column(name = "KICK_TYPE", nullable = false)
	private KickTypeEnum kickType;
	@Column(name = "KICK_BLOCK", nullable = false)
	private Integer fieldGoalBlock;
	@Column(name = "KICK_YARD", nullable = false)
	private Integer fieldGoalYard;
	@Column(name = "KICK_MISS", nullable = false)
	private Integer fieldGoalMiss;
	@Column(name = "KICK_MISS_REASON", nullable = true)
	private KickMissReasonEnum kickMissReason;

	public StatKickData() {

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
	 * @return the fieldGoal
	 */
	public Integer getFieldGoal() {
		return fieldGoal;
	}

	/**
	 * @param fieldGoal the fieldGoal to set
	 */
	public void setFieldGoal(Integer fieldGoal) {
		this.fieldGoal = fieldGoal;
	}

	/**
	 * @return the fieldGoalAttempt
	 */
	public Integer getFieldGoalAttempt() {
		return fieldGoalAttempt;
	}

	/**
	 * @param fieldGoalAttempt the fieldGoalAttempt to set
	 */
	public void setFieldGoalAttempt(Integer fieldGoalAttempt) {
		this.fieldGoalAttempt = fieldGoalAttempt;
	}

	/**
	 * @return the totalPoint
	 */
	public Integer getTotalPoint() {
		return totalPoint;
	}

	/**
	 * @param totalPoint the totalPoint to set
	 */
	public void setTotalPoint(Integer totalPoint) {
		this.totalPoint = totalPoint;
	}

	/**
	 * @return the kickType
	 */
	public KickTypeEnum getKickType() {
		return kickType;
	}

	/**
	 * @param kickType the kickType to set
	 */
	public void setKickType(KickTypeEnum kickType) {
		this.kickType = kickType;
	}

	/**
	 * @return the fieldGoalBlock
	 */
	public Integer getFieldGoalBlock() {
		return fieldGoalBlock;
	}

	/**
	 * @param fieldGoalBlock the fieldGoalBlock to set
	 */
	public void setFieldGoalBlock(Integer fieldGoalBlock) {
		this.fieldGoalBlock = fieldGoalBlock;
	}

	/**
	 * @return the fieldGoalYard
	 */
	public Integer getFieldGoalYard() {
		return fieldGoalYard;
	}

	/**
	 * @param fieldGoalYard the fieldGoalYard to set
	 */
	public void setFieldGoalYard(Integer fieldGoalYard) {
		this.fieldGoalYard = fieldGoalYard;
	}

	/**
	 * @return the fieldGoalMiss
	 */
	public Integer getFieldGoalMiss() {
		return fieldGoalMiss;
	}

	/**
	 * @param fieldGoalMiss the fieldGoalMiss to set
	 */
	public void setFieldGoalMiss(Integer fieldGoalMiss) {
		this.fieldGoalMiss = fieldGoalMiss;
	}

	/**
	 * @return the kickMissReason
	 */
	public KickMissReasonEnum getKickMissReason() {
		return kickMissReason;
	}

	/**
	 * @param kickMissReason the kickMissReason to set
	 */
	public void setKickMissReason(KickMissReasonEnum kickMissReason) {
		this.kickMissReason = kickMissReason;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(fieldGoal, fieldGoalAttempt, fieldGoalBlock, fieldGoalMiss,
				fieldGoalYard, kickMissReason, kickType, playStat, totalPoint);
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
		if (!(obj instanceof StatKickData)) {
			return false;
		}
		StatKickData other = (StatKickData) obj;
		return Objects.equals(fieldGoal, other.fieldGoal) && Objects.equals(fieldGoalAttempt, other.fieldGoalAttempt)
				&& Objects.equals(fieldGoalBlock, other.fieldGoalBlock)
				&& Objects.equals(fieldGoalMiss, other.fieldGoalMiss)
				&& Objects.equals(fieldGoalYard, other.fieldGoalYard) && kickMissReason == other.kickMissReason
				&& kickType == other.kickType && Objects.equals(playStat, other.playStat)
				&& Objects.equals(totalPoint, other.totalPoint);
	}

	@Override
	public String toString() {
		return "StatKickData [playStat=" + playStat + ", fieldGoal=" + fieldGoal + ", fieldGoalAttempt="
				+ fieldGoalAttempt + ", totalPoint=" + totalPoint + ", kickType=" + kickType + ", fieldGoalBlock="
				+ fieldGoalBlock + ", fieldGoalYard=" + fieldGoalYard + ", fieldGoalMiss=" + fieldGoalMiss
				+ ", kickMissReason=" + kickMissReason + "]";
	}

}
