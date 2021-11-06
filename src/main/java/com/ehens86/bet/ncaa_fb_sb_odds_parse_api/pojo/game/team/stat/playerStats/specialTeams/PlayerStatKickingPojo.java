package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.specialTeams;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.KickMissReasonEnum;

public class PlayerStatKickingPojo {
	private String playerName;
	private Integer fieldGoal;
	private Integer fieldGoalAttempt;
	private Integer fieldGoalLong;
	private Integer extraPoint;
	private Integer extraPointAttempt;
	private Integer totalPoint;
	private Integer fieldGoalBlock;
	private Integer fieldGoalYard;
	private Integer fieldGoalMiss;
	private Integer extraPointBlock;
	private Integer extraPointYard;
	private Integer extraPointMiss;	
	private KickMissReasonEnum kickMissReason;
	
	public PlayerStatKickingPojo() {

	}

	public PlayerStatKickingPojo(String playerName) {
		this.playerName = playerName;
//		this.fieldGoal = 0;
//		this.fieldGoalAttempt = 0;
//		this.fieldGoalLong = 0;
//		this.extraPoint = 0;
//		this.totalPoint = 0;
//		this.kickoffYard = 0;
//		this.kickoffTouchback = 0;
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
	 * @return the fieldGoalLong
	 */
	public Integer getFieldGoalLong() {
		return fieldGoalLong;
	}

	/**
	 * @param fieldGoalLong the fieldGoalLong to set
	 */
	public void setFieldGoalLong(Integer fieldGoalLong) {
		this.fieldGoalLong = fieldGoalLong;
	}

	/**
	 * @return the extraPoint
	 */
	public Integer getExtraPoint() {
		return extraPoint;
	}

	/**
	 * @param extraPoint the extraPoint to set
	 */
	public void setExtraPoint(Integer extraPoint) {
		this.extraPoint = extraPoint;
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

}
