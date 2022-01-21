package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.PlayerStatEntity;

@Entity
@Table(name = "STAT_RECEIVER")
public class StatReceivingData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -7344010171216003270L;
	@OneToOne(mappedBy = "receiving")
	private StatPassingData passing;
	@Column(name = "RECEIVE_RECEPTION", nullable = false)
	private Integer receivingReception;
	@Column(name = "RECEIVE_YARD", nullable = false)
	private Integer receivingYard;
	@Column(name = "RECEIVE_TOUCHDOWN", nullable = false)
	private Integer receivingTouchdown;
	@Column(name = "RECEIVE_TARGET", nullable = false)
	private Integer receivingTarget;
	@Column(name = "RECEIVE_DROP", nullable = false)
	private Integer receivingDrop;
	@Column(name = "RECEIVE_YARD_AFTER_CATCH", nullable = true)
	private Integer receivingYardAfterCatch;
	@Column(name = "RECEIVE_FIRSTDOWN", nullable = false)
	private Integer recievingFirstDown;
	@Column(name = "RECEIVE_FUMBLE", nullable = false)
	private Integer receivingFumble;
	@Column(name = "RECEIVE_FUMBLE_LOST", nullable = false)
	private Integer receivingFumbleLost;
	@Column(name = "RECEIVE_SAFETY", nullable = false)
	private Integer receivingSafety;
	@Column(name = "RECEIVE_TWO_POINT_CONV", nullable = false)
	private Integer receivingTwoPointConversion;

	public StatReceivingData() {
		// Base constructor
	}

	/**
	 * @return the passing
	 */
	public StatPassingData getPassing() {
		return passing;
	}

	/**
	 * @param passing the passing to set
	 */
	public void setPassing(StatPassingData passing) {
		this.passing = passing;
	}

	/**
	 * @return the receivingReception
	 */
	public Integer getReceivingReception() {
		return receivingReception;
	}

	/**
	 * @param receivingReception the receivingReception to set
	 */
	public void setReceivingReception(Integer receivingReception) {
		this.receivingReception = receivingReception;
	}

	/**
	 * @return the receivingYard
	 */
	public Integer getReceivingYard() {
		return receivingYard;
	}

	/**
	 * @param receivingYard the receivingYard to set
	 */
	public void setReceivingYard(Integer receivingYard) {
		this.receivingYard = receivingYard;
	}

	/**
	 * @return the receivingTouchdown
	 */
	public Integer getReceivingTouchdown() {
		return receivingTouchdown;
	}

	/**
	 * @param receivingTouchdown the receivingTouchdown to set
	 */
	public void setReceivingTouchdown(Integer receivingTouchdown) {
		this.receivingTouchdown = receivingTouchdown;
	}

	/**
	 * @return the receivingTarget
	 */
	public Integer getReceivingTarget() {
		return receivingTarget;
	}

	/**
	 * @param receivingTarget the receivingTarget to set
	 */
	public void setReceivingTarget(Integer receivingTarget) {
		this.receivingTarget = receivingTarget;
	}

	/**
	 * @return the receivingDrop
	 */
	public Integer getReceivingDrop() {
		return receivingDrop;
	}

	/**
	 * @param receivingDrop the receivingDrop to set
	 */
	public void setReceivingDrop(Integer receivingDrop) {
		this.receivingDrop = receivingDrop;
	}

	/**
	 * @return the receivingYardAfterCatch
	 */
	public Integer getReceivingYardAfterCatch() {
		return receivingYardAfterCatch;
	}

	/**
	 * @param receivingYardAfterCatch the receivingYardAfterCatch to set
	 */
	public void setReceivingYardAfterCatch(Integer receivingYardAfterCatch) {
		this.receivingYardAfterCatch = receivingYardAfterCatch;
	}

	/**
	 * @return the recievingFirstDown
	 */
	public Integer getRecievingFirstDown() {
		return recievingFirstDown;
	}

	/**
	 * @param recievingFirstDown the recievingFirstDown to set
	 */
	public void setRecievingFirstDown(Integer recievingFirstDown) {
		this.recievingFirstDown = recievingFirstDown;
	}

	/**
	 * @return the receivingFumble
	 */
	public Integer getReceivingFumble() {
		return receivingFumble;
	}

	/**
	 * @param receivingFumble the receivingFumble to set
	 */
	public void setReceivingFumble(Integer receivingFumble) {
		this.receivingFumble = receivingFumble;
	}

	/**
	 * @return the receivingFumbleLost
	 */
	public Integer getReceivingFumbleLost() {
		return receivingFumbleLost;
	}

	/**
	 * @param receivingFumbleLost the receivingFumbleLost to set
	 */
	public void setReceivingFumbleLost(Integer receivingFumbleLost) {
		this.receivingFumbleLost = receivingFumbleLost;
	}

	/**
	 * @return the receivingSafety
	 */
	public Integer getReceivingSafety() {
		return receivingSafety;
	}

	/**
	 * @param receivingSafety the receivingSafety to set
	 */
	public void setReceivingSafety(Integer receivingSafety) {
		this.receivingSafety = receivingSafety;
	}

	/**
	 * @return the receivingTwoPointConversion
	 */
	public Integer getReceivingTwoPointConversion() {
		return receivingTwoPointConversion;
	}

	/**
	 * @param receivingTwoPointConversion the receivingTwoPointConversion to set
	 */
	public void setReceivingTwoPointConversion(Integer receivingTwoPointConversion) {
		this.receivingTwoPointConversion = receivingTwoPointConversion;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(passing, receivingDrop, receivingFumble, receivingFumbleLost,
				receivingReception, receivingSafety, receivingTarget, receivingTouchdown, receivingTwoPointConversion,
				receivingYard, receivingYardAfterCatch, recievingFirstDown);
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
		if (!(obj instanceof StatReceivingData)) {
			return false;
		}
		StatReceivingData other = (StatReceivingData) obj;
		return Objects.equals(passing, other.passing) && Objects.equals(receivingDrop, other.receivingDrop)
				&& Objects.equals(receivingFumble, other.receivingFumble)
				&& Objects.equals(receivingFumbleLost, other.receivingFumbleLost)
				&& Objects.equals(receivingReception, other.receivingReception)
				&& Objects.equals(receivingSafety, other.receivingSafety)
				&& Objects.equals(receivingTarget, other.receivingTarget)
				&& Objects.equals(receivingTouchdown, other.receivingTouchdown)
				&& Objects.equals(receivingTwoPointConversion, other.receivingTwoPointConversion)
				&& Objects.equals(receivingYard, other.receivingYard)
				&& Objects.equals(receivingYardAfterCatch, other.receivingYardAfterCatch)
				&& Objects.equals(recievingFirstDown, other.recievingFirstDown);
	}

	@Override
	public String toString() {
		return "StatReceivingData [passing=" + passing + ", receivingReception=" + receivingReception
				+ ", receivingYard=" + receivingYard + ", receivingTouchdown=" + receivingTouchdown
				+ ", receivingTarget=" + receivingTarget + ", receivingDrop=" + receivingDrop
				+ ", receivingYardAfterCatch=" + receivingYardAfterCatch + ", recievingFirstDown=" + recievingFirstDown
				+ ", receivingFumble=" + receivingFumble + ", receivingFumbleLost=" + receivingFumbleLost
				+ ", receivingSafety=" + receivingSafety + ", receivingTwoPointConversion="
				+ receivingTwoPointConversion + "]";
	}

}
