package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.OidAuditEntity;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.StatPosNegEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.StatTypeEnum;

public class BaseStatEntity extends OidAuditEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 4333147831248062467L;
	@Column(name = "STAT_POS_NEG", nullable = true)
	protected StatPosNegEnum posNeg;
	@Column(name = "STAT_TYPE", nullable = true)
	protected StatTypeEnum statType;
	
	public BaseStatEntity() {
		// Base constructor
	}
	
	/**
	 * @return the posNeg
	 */
	public StatPosNegEnum getPosNeg() {
		return posNeg;
	}

	/**
	 * @param posNeg the posNeg to set
	 */
	public void setPosNeg(StatPosNegEnum posNeg) {
		this.posNeg = posNeg;
	}

	/**
	 * @return the statType
	 */
	public StatTypeEnum getStatType() {
		return statType;
	}

	/**
	 * @param statType the statType to set
	 */
	public void setStatType(StatTypeEnum statType) {
		this.statType = statType;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(posNeg, statType);
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
		if (!(obj instanceof BaseStatEntity)) {
			return false;
		}
		BaseStatEntity other = (BaseStatEntity) obj;
		return posNeg == other.posNeg && statType == other.statType;
	}

	@Override
	public String toString() {
		return "BaseStatEntity [posNeg=" + posNeg + ", statType=" + statType + "]";
	}
}
