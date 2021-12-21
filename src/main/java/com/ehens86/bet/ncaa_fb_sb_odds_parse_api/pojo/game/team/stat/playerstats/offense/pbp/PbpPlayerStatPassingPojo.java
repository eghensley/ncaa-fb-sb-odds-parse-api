package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.BaseStatPassingPojo;

public class PbpPlayerStatPassingPojo extends BaseStatPassingPojo {
	private Integer passingInterceptionYard;
	private Integer passingInterceptionTouchdown;
	private Integer passingBreakup;
	private Integer passingFirstDown;
	private PlayDirectionEnum passingDirection;
	private Integer passingDrop;
	private Integer passingYardAfterCatch;
	private Integer passingYardThrownTo;
	private Integer passingHurry;
	private Integer passingSack;
	private Integer passingFumble;
	private Integer passingFumbleLost;
	private Integer passingSackYard;
	private Integer passingSafety;
	private Integer passingSpike;
	private Integer passingTwoPointConversion;
	private Integer passingAirLessNeeded;

	public PbpPlayerStatPassingPojo(String name) {
		this.playerName = name;
		this.passingAttempt = 1;
	}

	/**
	 * @return the passingBreakup
	 */
	public Integer getPassingBreakup() {
		return passingBreakup;
	}

	/**
	 * @param passingBreakup the passingBreakup to set
	 */
	public void setPassingBreakup(Integer passingBreakup) {
		this.passingBreakup = passingBreakup;
	}

	/**
	 * @return the passingInterceptionYard
	 */
	public Integer getPassingInterceptionYard() {
		return passingInterceptionYard;
	}

	/**
	 * @param passingInterceptionYard the passingInterceptionYard to set
	 */
	public void setPassingInterceptionYard(Integer passingInterceptionYard) {
		this.passingInterceptionYard = passingInterceptionYard;
	}

	/**
	 * @return the passingInterceptionTouchdown
	 */
	public Integer getPassingInterceptionTouchdown() {
		return passingInterceptionTouchdown;
	}

	/**
	 * @param passingInterceptionTouchdown the passingInterceptionTouchdown to set
	 */
	public void setPassingInterceptionTouchdown(Integer passingInterceptionTouchdown) {
		this.passingInterceptionTouchdown = passingInterceptionTouchdown;
	}

	/**
	 * @return the passingFirstDown
	 */
	public Integer getPassingFirstDown() {
		return passingFirstDown;
	}

	/**
	 * @param passingFirstDown the passingFirstDown to set
	 */
	public void setPassingFirstDown(Integer passingFirstDown) {
		this.passingFirstDown = passingFirstDown;
	}

	/**
	 * @return the passingDirection
	 */
	public PlayDirectionEnum getPassingDirection() {
		return passingDirection;
	}

	/**
	 * @param passingDirection the passingDirection to set
	 */
	public void setPassingDirection(PlayDirectionEnum passingDirection) {
		this.passingDirection = passingDirection;
	}

	/**
	 * @return the passingDrop
	 */
	public Integer getPassingDrop() {
		return passingDrop;
	}

	/**
	 * @param passingDrop the passingDrop to set
	 */
	public void setPassingDrop(Integer passingDrop) {
		this.passingDrop = passingDrop;
	}

	/**
	 * @return the passingYardAfterCatch
	 */
	public Integer getPassingYardAfterCatch() {
		return passingYardAfterCatch;
	}

	/**
	 * @param passingYardAfterCatch the passingYardAfterCatch to set
	 */
	public void setPassingYardAfterCatch(Integer passingYardAfterCatch) {
		this.passingYardAfterCatch = passingYardAfterCatch;
	}

	/**
	 * @return the passingYardThrownTo
	 */
	public Integer getPassingYardThrownTo() {
		return passingYardThrownTo;
	}

	/**
	 * @param passingYardThrownTo the passingYardThrownTo to set
	 */
	public void setPassingYardThrownTo(Integer passingYardThrownTo) {
		this.passingYardThrownTo = passingYardThrownTo;
	}

	/**
	 * @return the passingHurry
	 */
	public Integer getPassingHurry() {
		return passingHurry;
	}

	/**
	 * @param passingHurry the passingHurry to set
	 */
	public void setPassingHurry(Integer passingHurry) {
		this.passingHurry = passingHurry;
	}

	/**
	 * @return the passingSack
	 */
	public Integer getPassingSack() {
		return passingSack;
	}

	/**
	 * @param passingSack the passingSack to set
	 */
	public void setPassingSack(Integer passingSack) {
		this.passingSack = passingSack;
	}

	/**
	 * @return the passingFumble
	 */
	public Integer getPassingFumble() {
		return passingFumble;
	}

	/**
	 * @param passingFumble the passingFumble to set
	 */
	public void setPassingFumble(Integer passingFumble) {
		this.passingFumble = passingFumble;
	}

	/**
	 * @return the passingFumbleLost
	 */
	public Integer getPassingFumbleLost() {
		return passingFumbleLost;
	}

	/**
	 * @param passingFumbleLost the passingFumbleLost to set
	 */
	public void setPassingFumbleLost(Integer passingFumbleLost) {
		this.passingFumbleLost = passingFumbleLost;
	}

	/**
	 * @param passingFumble the rushingFumble to set
	 */
	public void applyPassingFumble(boolean turnover) {
		this.passingFumble = 1;
		if (turnover) {
			this.passingFumbleLost = 1;
		} else {
			this.passingFumbleLost = 0;
		}
	}

	public void applyNoPassingFumble() {
		this.passingFumble = 0;
		this.passingFumbleLost = 0;
	}

	/**
	 * @return the passingSackYard
	 */
	public Integer getPassingSackYard() {
		return passingSackYard;
	}

	/**
	 * @param passingSackYard the passingSackYard to set
	 */
	public void setPassingSackYard(Integer passingSackYard) {
		this.passingSackYard = passingSackYard;
	}

	/**
	 * @return the passingSafety
	 */
	public Integer getPassingSafety() {
		return passingSafety;
	}

	/**
	 * @param passingSafety the passingSafety to set
	 */
	public void setPassingSafety(Integer passingSafety) {
		this.passingSafety = passingSafety;
	}

	/**
	 * @return the passingSpike
	 */
	public Integer getPassingSpike() {
		return passingSpike;
	}

	/**
	 * @param passingSpike the passingSpike to set
	 */
	public void setPassingSpike(Integer passingSpike) {
		this.passingSpike = passingSpike;
	}

	/**
	 * @return the passingTwoPointConversion
	 */
	public Integer getPassingTwoPointConversion() {
		return passingTwoPointConversion;
	}

	/**
	 * @param passingTwoPointConversion the passingTwoPointConversion to set
	 */
	public void setPassingTwoPointConversion(Integer passingTwoPointConversion) {
		this.passingTwoPointConversion = passingTwoPointConversion;
	}

	/**
	 * @return the passingAirLessNeeded
	 */
	public Integer getPassingAirLessNeeded() {
		return passingAirLessNeeded;
	}

	/**
	 * @param passingAirLessNeeded the passingAirLessNeeded to set
	 */
	public void setPassingAirLessNeeded(Integer passingAirLessNeeded) {
		this.passingAirLessNeeded = passingAirLessNeeded;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(passingAirLessNeeded, passingBreakup, passingDirection, passingDrop,
				passingFirstDown, passingFumble, passingFumbleLost, passingHurry, passingInterceptionTouchdown,
				passingInterceptionYard, passingSack, passingSackYard, passingSafety, passingSpike,
				passingTwoPointConversion, passingYardAfterCatch, passingYardThrownTo);
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
		if (!(obj instanceof PbpPlayerStatPassingPojo)) {
			return false;
		}
		PbpPlayerStatPassingPojo other = (PbpPlayerStatPassingPojo) obj;
		return Objects.equals(passingAirLessNeeded, other.passingAirLessNeeded)
				&& Objects.equals(passingBreakup, other.passingBreakup) && passingDirection == other.passingDirection
				&& Objects.equals(passingDrop, other.passingDrop)
				&& Objects.equals(passingFirstDown, other.passingFirstDown)
				&& Objects.equals(passingFumble, other.passingFumble)
				&& Objects.equals(passingFumbleLost, other.passingFumbleLost)
				&& Objects.equals(passingHurry, other.passingHurry)
				&& Objects.equals(passingInterceptionTouchdown, other.passingInterceptionTouchdown)
				&& Objects.equals(passingInterceptionYard, other.passingInterceptionYard)
				&& Objects.equals(passingSack, other.passingSack)
				&& Objects.equals(passingSackYard, other.passingSackYard)
				&& Objects.equals(passingSafety, other.passingSafety)
				&& Objects.equals(passingSpike, other.passingSpike)
				&& Objects.equals(passingTwoPointConversion, other.passingTwoPointConversion)
				&& Objects.equals(passingYardAfterCatch, other.passingYardAfterCatch)
				&& Objects.equals(passingYardThrownTo, other.passingYardThrownTo);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatPassingPojo [passingInterceptionYard=" + passingInterceptionYard
				+ ", passingInterceptionTouchdown=" + passingInterceptionTouchdown + ", passingBreakup="
				+ passingBreakup + ", passingFirstDown=" + passingFirstDown + ", passingDirection=" + passingDirection
				+ ", passingDrop=" + passingDrop + ", passingYardAfterCatch=" + passingYardAfterCatch
				+ ", passingYardThrownTo=" + passingYardThrownTo + ", passingHurry=" + passingHurry + ", passingSack="
				+ passingSack + ", passingFumble=" + passingFumble + ", passingFumbleLost=" + passingFumbleLost
				+ ", passingSackYard=" + passingSackYard + ", passingSafety=" + passingSafety + ", passingSpike="
				+ passingSpike + ", passingTwoPointConversion=" + passingTwoPointConversion + ", passingAirLessNeeded="
				+ passingAirLessNeeded + "]";
	}

}
