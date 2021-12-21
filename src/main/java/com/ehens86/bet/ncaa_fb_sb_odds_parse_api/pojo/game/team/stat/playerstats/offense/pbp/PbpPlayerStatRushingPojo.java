package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.BaseStatRushingPojo;

public class PbpPlayerStatRushingPojo extends BaseStatRushingPojo {
	private Integer rushingFirstDown;
	private Integer rushingFumble;
	private Integer rushingFumbleLost;
	private PlayDirectionEnum rushingDirection;
	private Integer rushingSafety;
	private Integer rushingKneel;
	private Integer rushingTwoPointConversion;
	private Double rushingLineYard;
	private Boolean rushingOpenField;
	private Integer rushingOpenFieldYard;
	private Boolean rushingStuff;
	private Boolean rushingSecondLevel;
	private Integer rushingSecondLevelYard;
	private Boolean rushingPower;
	private Boolean rushingPowerSuccess;
	private Boolean rushingSuccess;

	public PbpPlayerStatRushingPojo() {

	}

	public PbpPlayerStatRushingPojo(String name) {
		this.playerName = name;
		this.rushingAttempt = 1;
	}

	public PbpPlayerStatRushingPojo(Integer rushingFirstDown, Integer rushingFumble, Integer rushingFumbleLost,
			PlayDirectionEnum rushingDirection, Integer rushingSafety, Integer rushingKneel,
			Integer rushingTwoPointConversion) {
		super();
		this.rushingFirstDown = rushingFirstDown;
		this.rushingFumble = rushingFumble;
		this.rushingFumbleLost = rushingFumbleLost;
		this.rushingDirection = rushingDirection;
		this.rushingSafety = rushingSafety;
		this.rushingKneel = rushingKneel;
		this.rushingTwoPointConversion = rushingTwoPointConversion;
	}

	/**
	 * @return the rushingFirstDown
	 */
	public Integer getRushingFirstDown() {
		return rushingFirstDown;
	}

	/**
	 * @param rushingFirstDown the rushingFirstDown to set
	 */
	public void setRushingFirstDown(Integer rushingFirstDown) {
		this.rushingFirstDown = rushingFirstDown;
	}

	/**
	 * @return the rushingFumble
	 */
	public Integer getRushingFumble() {
		return rushingFumble;
	}

	/**
	 * @param rushingFumble the rushingFumble to set
	 */
	public void setRushingFumble(Integer rushingFumble) {
		this.rushingFumble = rushingFumble;
	}

	/**
	 * @param rushingFumble the rushingFumble to set
	 */
	public void applyRushingFumble(boolean turnover) {
		this.rushingFumble = 1;
		if (turnover) {
			this.rushingFumbleLost = 1;
		} else {
			this.rushingFumbleLost = 0;
		}
	}

	public void applyNoRushingFumble() {
		this.rushingFumble = 0;
		this.rushingFumbleLost = 0;
	}

	/**
	 * @return the rushingFumbleLost
	 */
	public Integer getRushingFumbleLost() {
		return rushingFumbleLost;
	}

	/**
	 * @param rushingFumbleLost the rushingFumbleLost to set
	 */
	public void setRushingFumbleLost(Integer rushingFumbleLost) {
		this.rushingFumbleLost = rushingFumbleLost;
	}

	/**
	 * @return the rushingDirection
	 */
	public PlayDirectionEnum getRushingDirection() {
		return rushingDirection;
	}

	/**
	 * @param rushingDirection the rushingDirection to set
	 */
	public void setRushingDirection(PlayDirectionEnum rushingDirection) {
		this.rushingDirection = rushingDirection;
	}

	/**
	 * @return the rushingSafety
	 */
	public Integer getRushingSafety() {
		return rushingSafety;
	}

	/**
	 * @param rushingSafety the rushingSafety to set
	 */
	public void setRushingSafety(Integer rushingSafety) {
		this.rushingSafety = rushingSafety;
	}

	/**
	 * @return the rushingKneel
	 */
	public Integer getRushingKneel() {
		return rushingKneel;
	}

	/**
	 * @param rushingKneel the rushingKneel to set
	 */
	public void setRushingKneel(Integer rushingKneel) {
		this.rushingKneel = rushingKneel;
	}

	/**
	 * @return the rushingTwoPointConversion
	 */
	public Integer getRushingTwoPointConversion() {
		return rushingTwoPointConversion;
	}

	/**
	 * @param rushingTwoPointConversion the rushingTwoPointConversion to set
	 */
	public void setRushingTwoPointConversion(Integer rushingTwoPointConversion) {
		this.rushingTwoPointConversion = rushingTwoPointConversion;
	}

	/**
	 * @return the rushingLineYard
	 */
	public Double getRushingLineYard() {
		return rushingLineYard;
	}

	/**
	 * @param rushingLineYard the rushingLineYard to set
	 */
	public void setRushingLineYard(Double rushingLineYard) {
		this.rushingLineYard = rushingLineYard;
	}

	/**
	 * @return the rushingOpenFieldYard
	 */
	public Integer getRushingOpenFieldYard() {
		return rushingOpenFieldYard;
	}

	/**
	 * @param rushingOpenFieldYard the rushingOpenFieldYard to set
	 */
	public void setRushingOpenFieldYard(Integer rushingOpenFieldYard) {
		this.rushingOpenFieldYard = rushingOpenFieldYard;
	}

	/**
	 * @return the rushingStuff
	 */
	public Boolean getRushingStuff() {
		return rushingStuff;
	}

	/**
	 * @param rushingStuff the rushingStuff to set
	 */
	public void setRushingStuff(Boolean rushingStuff) {
		this.rushingStuff = rushingStuff;
	}

	/**
	 * @return the rushingSecondLevelYard
	 */
	public Integer getRushingSecondLevelYard() {
		return rushingSecondLevelYard;
	}

	/**
	 * @param rushingSecondLevelYard the rushingSecondLevelYard to set
	 */
	public void setRushingSecondLevelYard(Integer rushingSecondLevelYard) {
		this.rushingSecondLevelYard = rushingSecondLevelYard;
	}

	/**
	 * @return the rushingPower
	 */
	public Boolean getRushingPower() {
		return rushingPower;
	}

	/**
	 * @param rushingPower the rushingPower to set
	 */
	public void setRushingPower(Boolean rushingPower) {
		this.rushingPower = rushingPower;
	}

	/**
	 * @return the rushingPowerSuccess
	 */
	public Boolean getRushingPowerSuccess() {
		return rushingPowerSuccess;
	}

	/**
	 * @param rushingPowerSuccess the rushingPowerSuccess to set
	 */
	public void setRushingPowerSuccess(Boolean rushingPowerSuccess) {
		this.rushingPowerSuccess = rushingPowerSuccess;
	}

	/**
	 * @return the rushingSuccess
	 */
	public Boolean getRushingSuccess() {
		return rushingSuccess;
	}

	/**
	 * @param rushingSuccess the rushingSuccess to set
	 */
	public void setRushingSuccess(Boolean rushingSuccess) {
		this.rushingSuccess = rushingSuccess;
	}

	/**
	 * @return the rushingOpenField
	 */
	public Boolean getRushingOpenField() {
		return rushingOpenField;
	}

	/**
	 * @param rushingOpenField the rushingOpenField to set
	 */
	public void setRushingOpenField(Boolean rushingOpenField) {
		this.rushingOpenField = rushingOpenField;
	}

	/**
	 * @return the rushingSecondLevel
	 */
	public Boolean getRushingSecondLevel() {
		return rushingSecondLevel;
	}

	/**
	 * @param rushingSecondLevel the rushingSecondLevel to set
	 */
	public void setRushingSecondLevel(Boolean rushingSecondLevel) {
		this.rushingSecondLevel = rushingSecondLevel;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(rushingDirection, rushingFirstDown, rushingFumble, rushingFumbleLost,
				rushingKneel, rushingLineYard, rushingOpenField, rushingOpenFieldYard, rushingPower,
				rushingPowerSuccess, rushingSafety, rushingSecondLevel, rushingSecondLevelYard, rushingStuff,
				rushingSuccess, rushingTwoPointConversion);
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
		if (!(obj instanceof PbpPlayerStatRushingPojo)) {
			return false;
		}
		PbpPlayerStatRushingPojo other = (PbpPlayerStatRushingPojo) obj;
		return rushingDirection == other.rushingDirection && Objects.equals(rushingFirstDown, other.rushingFirstDown)
				&& Objects.equals(rushingFumble, other.rushingFumble)
				&& Objects.equals(rushingFumbleLost, other.rushingFumbleLost)
				&& Objects.equals(rushingKneel, other.rushingKneel)
				&& Objects.equals(rushingLineYard, other.rushingLineYard)
				&& Objects.equals(rushingOpenField, other.rushingOpenField)
				&& Objects.equals(rushingOpenFieldYard, other.rushingOpenFieldYard)
				&& Objects.equals(rushingPower, other.rushingPower)
				&& Objects.equals(rushingPowerSuccess, other.rushingPowerSuccess)
				&& Objects.equals(rushingSafety, other.rushingSafety)
				&& Objects.equals(rushingSecondLevel, other.rushingSecondLevel)
				&& Objects.equals(rushingSecondLevelYard, other.rushingSecondLevelYard)
				&& Objects.equals(rushingStuff, other.rushingStuff)
				&& Objects.equals(rushingSuccess, other.rushingSuccess)
				&& Objects.equals(rushingTwoPointConversion, other.rushingTwoPointConversion);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatRushingPojo [rushingFirstDown=" + rushingFirstDown + ", rushingFumble=" + rushingFumble
				+ ", rushingFumbleLost=" + rushingFumbleLost + ", rushingDirection=" + rushingDirection
				+ ", rushingSafety=" + rushingSafety + ", rushingKneel=" + rushingKneel + ", rushingTwoPointConversion="
				+ rushingTwoPointConversion + ", rushingLineYard=" + rushingLineYard + ", rushingOpenField="
				+ rushingOpenField + ", rushingOpenFieldYard=" + rushingOpenFieldYard + ", rushingStuff=" + rushingStuff
				+ ", rushingSecondLevel=" + rushingSecondLevel + ", rushingSecondLevelYard=" + rushingSecondLevelYard
				+ ", rushingPower=" + rushingPower + ", rushingPowerSuccess=" + rushingPowerSuccess
				+ ", rushingSuccess=" + rushingSuccess + "]";
	}

}
