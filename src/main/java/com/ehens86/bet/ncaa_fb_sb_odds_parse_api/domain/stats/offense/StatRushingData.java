package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.PlayerStatEntity;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;

@Entity
@Table(name = "STAT_RUSHING")
public class StatRushingData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6871070017121348441L;

	@OneToOne(mappedBy = "rushStat")
	private TeamPlayStatData playStat;
	@Column(name = "RUSH_ATTEMPT", nullable = false)
	private Integer rushingAttempt;
	@Column(name = "RUSH_YARD", nullable = false)
	private Integer rushingYard;
	@Column(name = "RUSH_TOUCHDOWN", nullable = false)
	private Integer rushingTouchdown;
	@Column(name = "RUSH_FIRSTDOWN", nullable = false)
	private Integer rushingFirstDown;
	@Column(name = "RUSH_FUMBLE", nullable = false)
	private Integer rushingFumble;
	@Column(name = "RUSH_FUMBLE_LOST", nullable = false)
	private Integer rushingFumbleLost;
	@Column(name = "RUSH_DIRECTION", nullable = false)
	private PlayDirectionEnum rushingDirection;
	@Column(name = "RUSH_SAFETY", nullable = false)
	private Integer rushingSafety;
	@Column(name = "RUSH_KNEEL", nullable = false)
	private Integer rushingKneel;
	@Column(name = "RUSH_TWO_POINT_CONV", nullable = false)
	private Integer rushingTwoPointConversion;
	@Column(name = "RUSH_LINE_YARD", nullable = false)
	private Double rushingLineYard;
	@Column(name = "F_RUSH_OPEN_FIELD", nullable = false)
	private boolean rushingOpenField;
	@Column(name = "RUSH_OPEN_FIELD_YARD", nullable = true)
	private Integer rushingOpenFieldYard;
	@Column(name = "F_RUSH_SECOND_LEVEL", nullable = false)
	private boolean rushingSecondLevel;
	@Column(name = "RUSH_SECOND_LEVEL_YARD", nullable = true)
	private Integer rushingSecondLevelYard;
	@Column(name = "F_RUSH_STUFF", nullable = false)
	private boolean rushingStuff;
	@Column(name = "F_RUSH_POWER", nullable = false)
	private boolean rushingPower;
	@Column(name = "F_RUSH_POWER_SUCCESS", nullable = true)
	private boolean rushingPowerSuccess;
	@Column(name = "F_RUSH_SUCCESS", nullable = true)
	private boolean rushingSuccess;
	
	public StatRushingData() {
		// Base constructor
	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
	}

	/**
	 * @return the rushingAttempt
	 */
	public Integer getRushingAttempt() {
		return rushingAttempt;
	}

	/**
	 * @param rushingAttempt the rushingAttempt to set
	 */
	public void setRushingAttempt(Integer rushingAttempt) {
		this.rushingAttempt = rushingAttempt;
	}

	/**
	 * @return the rushingYard
	 */
	public Integer getRushingYard() {
		return rushingYard;
	}

	/**
	 * @param rushingYard the rushingYard to set
	 */
	public void setRushingYard(Integer rushingYard) {
		this.rushingYard = rushingYard;
	}

	/**
	 * @return the rushingTouchdown
	 */
	public Integer getRushingTouchdown() {
		return rushingTouchdown;
	}

	/**
	 * @param rushingTouchdown the rushingTouchdown to set
	 */
	public void setRushingTouchdown(Integer rushingTouchdown) {
		this.rushingTouchdown = rushingTouchdown;
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
	public boolean getRushingStuff() {
		return rushingStuff;
	}

	/**
	 * @param rushingStuff the rushingStuff to set
	 */
	public void setRushingStuff(boolean rushingStuff) {
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
	public boolean getRushingPower() {
		return rushingPower;
	}

	/**
	 * @param rushingPower the rushingPower to set
	 */
	public void setRushingPower(boolean rushingPower) {
		this.rushingPower = rushingPower;
	}

	/**
	 * @return the rushingPowerSuccess
	 */
	public boolean getRushingPowerSuccess() {
		return rushingPowerSuccess;
	}

	/**
	 * @param rushingPowerSuccess the rushingPowerSuccess to set
	 */
	public void setRushingPowerSuccess(boolean rushingPowerSuccess) {
		this.rushingPowerSuccess = rushingPowerSuccess;
	}

	/**
	 * @return the rushingSuccess
	 */
	public boolean getRushingSuccess() {
		return rushingSuccess;
	}

	/**
	 * @param rushingSuccess the rushingSuccess to set
	 */
	public void setRushingSuccess(boolean rushingSuccess) {
		this.rushingSuccess = rushingSuccess;
	}

	/**
	 * @return the rushingOpenField
	 */
	public boolean getRushingOpenField() {
		return rushingOpenField;
	}

	/**
	 * @param rushingOpenField the rushingOpenField to set
	 */
	public void setRushingOpenField(boolean rushingOpenField) {
		this.rushingOpenField = rushingOpenField;
	}

	/**
	 * @return the rushingSecondLevel
	 */
	public boolean getRushingSecondLevel() {
		return rushingSecondLevel;
	}

	/**
	 * @param rushingSecondLevel the rushingSecondLevel to set
	 */
	public void setRushingSecondLevel(boolean rushingSecondLevel) {
		this.rushingSecondLevel = rushingSecondLevel;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(playStat, rushingAttempt, rushingDirection, rushingFirstDown,
				rushingFumble, rushingFumbleLost, rushingKneel, rushingLineYard, rushingOpenField, rushingOpenFieldYard,
				rushingPower, rushingPowerSuccess, rushingSafety, rushingSecondLevel, rushingSecondLevelYard,
				rushingStuff, rushingSuccess, rushingTouchdown, rushingTwoPointConversion, rushingYard);
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
		if (!(obj instanceof StatRushingData)) {
			return false;
		}
		StatRushingData other = (StatRushingData) obj;
		return Objects.equals(playStat, other.playStat) && Objects.equals(rushingAttempt, other.rushingAttempt)
				&& rushingDirection == other.rushingDirection
				&& Objects.equals(rushingFirstDown, other.rushingFirstDown)
				&& Objects.equals(rushingFumble, other.rushingFumble)
				&& Objects.equals(rushingFumbleLost, other.rushingFumbleLost)
				&& Objects.equals(rushingKneel, other.rushingKneel)
				&& Objects.equals(rushingLineYard, other.rushingLineYard) && rushingOpenField == other.rushingOpenField
				&& Objects.equals(rushingOpenFieldYard, other.rushingOpenFieldYard)
				&& rushingPower == other.rushingPower && rushingPowerSuccess == other.rushingPowerSuccess
				&& Objects.equals(rushingSafety, other.rushingSafety) && rushingSecondLevel == other.rushingSecondLevel
				&& Objects.equals(rushingSecondLevelYard, other.rushingSecondLevelYard)
				&& rushingStuff == other.rushingStuff && rushingSuccess == other.rushingSuccess
				&& Objects.equals(rushingTouchdown, other.rushingTouchdown)
				&& Objects.equals(rushingTwoPointConversion, other.rushingTwoPointConversion)
				&& Objects.equals(rushingYard, other.rushingYard);
	}

	@Override
	public String toString() {
		return "StatRushingData [playStat=" + playStat + ", rushingAttempt=" + rushingAttempt + ", rushingYard="
				+ rushingYard + ", rushingTouchdown=" + rushingTouchdown + ", rushingFirstDown=" + rushingFirstDown
				+ ", rushingFumble=" + rushingFumble + ", rushingFumbleLost=" + rushingFumbleLost
				+ ", rushingDirection=" + rushingDirection + ", rushingSafety=" + rushingSafety + ", rushingKneel="
				+ rushingKneel + ", rushingTwoPointConversion=" + rushingTwoPointConversion + ", rushingLineYard="
				+ rushingLineYard + ", rushingOpenField=" + rushingOpenField + ", rushingOpenFieldYard="
				+ rushingOpenFieldYard + ", rushingSecondLevel=" + rushingSecondLevel + ", rushingSecondLevelYard="
				+ rushingSecondLevelYard + ", rushingStuff=" + rushingStuff + ", rushingPower=" + rushingPower
				+ ", rushingPowerSuccess=" + rushingPowerSuccess + ", rushingSuccess=" + rushingSuccess + "]";
	}

}
