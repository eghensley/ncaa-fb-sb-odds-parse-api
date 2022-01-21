package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.PlayerStatEntity;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDirectionEnum;

@Entity
@Table(name = "STAT_PASSING")
public class StatPassingData extends PlayerStatEntity implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4122868029529159462L;
	@OneToOne(mappedBy = "passStat")
	private TeamPlayStatData playStat;
	@Column(name = "PASS_ATTEMPT", nullable = false)
	private Integer passingAttempt;
	@Column(name = "PASS_INTERCEPTION", nullable = false)
	private Integer passingInterception;
	@Column(name = "PASS_INTERCEPTION_YARD", nullable = false)
	private Integer passingInterceptionYard;
	@Column(name = "PASS_INTERCEPTION_TD", nullable = false)
	private Integer passingInterceptionTouchdown;
	@Column(name = "PASS_BREAKUP", nullable = false)
	private Integer passingBreakup;
	@Column(name = "PASS_DIRECTION", nullable = false)
	private PlayDirectionEnum passingDirection;
	@Column(name = "PASS_YARD_THROWN", nullable = true)
	private Integer passingYardThrownTo;
	@Column(name = "PASS_HURRY", nullable = false)
	private Integer passingHurry;
	@Column(name = "PASS_SACK", nullable = false)
	private Integer passingSack;
	@Column(name = "PASS_FUMBLE", nullable = false)
	private Integer passingFumble;
	@Column(name = "PASS_FUMBLE_LOST", nullable = false)
	private Integer passingFumbleLost;
	@Column(name = "PASS_SACK_YARD", nullable = false)
	private Integer passingSackYard;
	@Column(name = "PASS_SPIKE", nullable = false)
	private Integer passingSpike;
	@Column(name = "PASS_AIR_LESS_NEEDED", nullable = true)
	private Integer passingAirLessNeeded;
	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "RECEIVING_OID", referencedColumnName = "OID", nullable = true)
	private StatReceivingData receiving;

	public StatPassingData() {
		// Base constructor
	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
	}

	/**
	 * @return the passingAttempt
	 */
	public Integer getPassingAttempt() {
		return passingAttempt;
	}

	/**
	 * @param passingAttempt the passingAttempt to set
	 */
	public void setPassingAttempt(Integer passingAttempt) {
		this.passingAttempt = passingAttempt;
	}

	/**
	 * @return the passingInterception
	 */
	public Integer getPassingInterception() {
		return passingInterception;
	}

	/**
	 * @param passingInterception the passingInterception to set
	 */
	public void setPassingInterception(Integer passingInterception) {
		this.passingInterception = passingInterception;
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
	 * @return the receiving
	 */
	public StatReceivingData getReceiving() {
		return receiving;
	}

	/**
	 * @param receiving the receiving to set
	 */
	public void setReceiving(StatReceivingData receiving) {
		this.receiving = receiving;
		receiving.setPassing(this);
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
		result = prime * result + Objects.hash(passingAirLessNeeded, passingAttempt, passingBreakup, passingDirection,
				passingFumble, passingFumbleLost, passingHurry, passingInterception, passingInterceptionTouchdown,
				passingInterceptionYard, passingSack, passingSackYard, passingSpike, passingYardThrownTo, playStat,
				receiving);
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
		if (!(obj instanceof StatPassingData)) {
			return false;
		}
		StatPassingData other = (StatPassingData) obj;
		return Objects.equals(passingAirLessNeeded, other.passingAirLessNeeded)
				&& Objects.equals(passingAttempt, other.passingAttempt)
				&& Objects.equals(passingBreakup, other.passingBreakup) && passingDirection == other.passingDirection
				&& Objects.equals(passingFumble, other.passingFumble)
				&& Objects.equals(passingFumbleLost, other.passingFumbleLost)
				&& Objects.equals(passingHurry, other.passingHurry)
				&& Objects.equals(passingInterception, other.passingInterception)
				&& Objects.equals(passingInterceptionTouchdown, other.passingInterceptionTouchdown)
				&& Objects.equals(passingInterceptionYard, other.passingInterceptionYard)
				&& Objects.equals(passingSack, other.passingSack)
				&& Objects.equals(passingSackYard, other.passingSackYard)
				&& Objects.equals(passingSpike, other.passingSpike)
				&& Objects.equals(passingYardThrownTo, other.passingYardThrownTo)
				&& Objects.equals(playStat, other.playStat) && Objects.equals(receiving, other.receiving);
	}

	@Override
	public String toString() {
		return "StatPassingData [playStat=" + playStat + ", passingAttempt=" + passingAttempt + ", passingInterception="
				+ passingInterception + ", passingInterceptionYard=" + passingInterceptionYard
				+ ", passingInterceptionTouchdown=" + passingInterceptionTouchdown + ", passingBreakup="
				+ passingBreakup + ", passingDirection=" + passingDirection + ", passingYardThrownTo="
				+ passingYardThrownTo + ", passingHurry=" + passingHurry + ", passingSack=" + passingSack
				+ ", passingFumble=" + passingFumble + ", passingFumbleLost=" + passingFumbleLost + ", passingSackYard="
				+ passingSackYard + ", passingSpike=" + passingSpike + ", passingAirLessNeeded=" + passingAirLessNeeded
				+ ", receiving=" + receiving + "]";
	}

}
