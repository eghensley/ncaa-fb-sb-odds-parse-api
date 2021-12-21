package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.BaseStatReceivingPojo;

public class PbpPlayerStatReceivingPojo extends BaseStatReceivingPojo {
	private Integer receivingTarget;
	private Integer receivingDrop;
	private Integer receivingYardAfterCatch;
	private Integer recievingFirstDown;
	private Integer receivingFumble;
	private Integer receivingFumbleLost;
	private Integer receivingSafety;
	private Integer receivingTwoPointConversion;

	public PbpPlayerStatReceivingPojo(String name, Integer reception, Integer yards) {
		this.playerName = name;
		this.receivingTarget = 1;
		this.receivingReception = reception;
		this.receivingYard = yards;
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
	 * @param receivingFumble the receivingFumble to set
	 */
	public void applyReceivingFumble(boolean turnover) {
		this.receivingFumble = 1;
		if (turnover) {
			this.receivingFumbleLost = 1;
		} else {
			this.receivingFumbleLost = 0;
		}
	}

	public void applyNoReceivingFumble() {
		this.receivingFumble = 0;
		this.receivingFumbleLost = 0;
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
		result = prime * result + Objects.hash(receivingDrop, receivingFumble, receivingFumbleLost, receivingSafety,
				receivingTarget, receivingTwoPointConversion, receivingYardAfterCatch, recievingFirstDown);
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
		if (!(obj instanceof PbpPlayerStatReceivingPojo)) {
			return false;
		}
		PbpPlayerStatReceivingPojo other = (PbpPlayerStatReceivingPojo) obj;
		return Objects.equals(receivingDrop, other.receivingDrop)
				&& Objects.equals(receivingFumble, other.receivingFumble)
				&& Objects.equals(receivingFumbleLost, other.receivingFumbleLost)
				&& Objects.equals(receivingSafety, other.receivingSafety)
				&& Objects.equals(receivingTarget, other.receivingTarget)
				&& Objects.equals(receivingTwoPointConversion, other.receivingTwoPointConversion)
				&& Objects.equals(receivingYardAfterCatch, other.receivingYardAfterCatch)
				&& Objects.equals(recievingFirstDown, other.recievingFirstDown);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatReceivingPojo [receivingTarget=" + receivingTarget + ", receivingDrop=" + receivingDrop
				+ ", receivingYardAfterCatch=" + receivingYardAfterCatch + ", recievingFirstDown=" + recievingFirstDown
				+ ", receivingFumble=" + receivingFumble + ", receivingFumbleLost=" + receivingFumbleLost
				+ ", receivingSafety=" + receivingSafety + ", receivingTwoPointConversion="
				+ receivingTwoPointConversion + "]";
	}

}
