package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.KickMissReasonEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.BaseStatKickingPojo;

public class PbpPlayerStatKickingPojo extends BaseStatKickingPojo {
	private Integer extraPointAttempt;
	private Integer fieldGoalBlock;
	private Integer fieldGoalYard;
	private Integer fieldGoalMiss;
	private Integer extraPointBlock;
	private Integer extraPointYard;
	private Integer extraPointMiss;
	private KickMissReasonEnum kickMissReason;

	public PbpPlayerStatKickingPojo(String playerName) {
		this.playerName = playerName;
	}

	public PbpPlayerStatKickingPojo() {
		super();
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

	/**
	 * @return the extraPointAttempt
	 */
	public Integer getExtraPointAttempt() {
		return extraPointAttempt;
	}

	/**
	 * @param extraPointAttempt the extraPointAttempt to set
	 */
	public void setExtraPointAttempt(Integer extraPointAttempt) {
		this.extraPointAttempt = extraPointAttempt;
	}

	/**
	 * @return the extraPointBlock
	 */
	public Integer getExtraPointBlock() {
		return extraPointBlock;
	}

	/**
	 * @param extraPointBlock the extraPointBlock to set
	 */
	public void setExtraPointBlock(Integer extraPointBlock) {
		this.extraPointBlock = extraPointBlock;
	}

	/**
	 * @return the extraPointYard
	 */
	public Integer getExtraPointYard() {
		return extraPointYard;
	}

	/**
	 * @param extraPointYard the extraPointYard to set
	 */
	public void setExtraPointYard(Integer extraPointYard) {
		this.extraPointYard = extraPointYard;
	}

	/**
	 * @return the extraPointMiss
	 */
	public Integer getExtraPointMiss() {
		return extraPointMiss;
	}

	/**
	 * @param extraPointMiss the extraPointMiss to set
	 */
	public void setExtraPointMiss(Integer extraPointMiss) {
		this.extraPointMiss = extraPointMiss;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(extraPointAttempt, extraPointBlock, extraPointMiss, extraPointYard,
				fieldGoalBlock, fieldGoalMiss, fieldGoalYard, kickMissReason);
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
		if (!(obj instanceof PbpPlayerStatKickingPojo)) {
			return false;
		}
		PbpPlayerStatKickingPojo other = (PbpPlayerStatKickingPojo) obj;
		return Objects.equals(extraPointAttempt, other.extraPointAttempt)
				&& Objects.equals(extraPointBlock, other.extraPointBlock)
				&& Objects.equals(extraPointMiss, other.extraPointMiss)
				&& Objects.equals(extraPointYard, other.extraPointYard)
				&& Objects.equals(fieldGoalBlock, other.fieldGoalBlock)
				&& Objects.equals(fieldGoalMiss, other.fieldGoalMiss)
				&& Objects.equals(fieldGoalYard, other.fieldGoalYard) && kickMissReason == other.kickMissReason;
	}

	@Override
	public String toString() {
		return "PbpPlayerStatKickingPojo [extraPointAttempt=" + extraPointAttempt + ", fieldGoalBlock=" + fieldGoalBlock
				+ ", fieldGoalYard=" + fieldGoalYard + ", fieldGoalMiss=" + fieldGoalMiss + ", extraPointBlock="
				+ extraPointBlock + ", extraPointYard=" + extraPointYard + ", extraPointMiss=" + extraPointMiss
				+ ", kickMissReason=" + kickMissReason + "]";
	}

}
