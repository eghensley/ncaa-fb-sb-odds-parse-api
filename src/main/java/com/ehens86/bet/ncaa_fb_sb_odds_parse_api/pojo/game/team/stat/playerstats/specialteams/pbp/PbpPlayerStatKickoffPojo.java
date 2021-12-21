package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.PlayerStatKickoffPojo;

public class PbpPlayerStatKickoffPojo extends PlayerStatKickoffPojo {
	private Integer kickoffTouchback;
	private Integer kickoffOnsideAttempt;
	private Integer kickoffOnsideSuccess;
	private Integer kickoffReturnYard;
	private Integer kickoffOutOfBounds;
	private Integer kickoffReturnTouchdown;
	private Integer kickoffFairCatch;
	private Integer kickoff;
	private Integer kickoffLandYard;
	
	
	public PbpPlayerStatKickoffPojo() {
		super();
	}

	public PbpPlayerStatKickoffPojo(Integer kickoffTouchback, Integer kickoffOnsideAttempt,
			Integer kickoffOnsideSuccess, Integer kickoffReturnYard, Integer kickoffOutOfBounds,
			Integer kickoffReturnTouchdown, Integer kickoffFairCatch, Integer kickoff, Integer kickoffLandYard) {
		super();
		this.kickoffTouchback = kickoffTouchback;
		this.kickoffOnsideAttempt = kickoffOnsideAttempt;
		this.kickoffOnsideSuccess = kickoffOnsideSuccess;
		this.kickoffReturnYard = kickoffReturnYard;
		this.kickoffOutOfBounds = kickoffOutOfBounds;
		this.kickoffReturnTouchdown = kickoffReturnTouchdown;
		this.kickoffFairCatch = kickoffFairCatch;
		this.kickoff = kickoff;
		this.kickoffLandYard = kickoffLandYard;
	}

	public PbpPlayerStatKickoffPojo(String playerName) {
		this.playerName = playerName;
		this.kickoff = 1;
	}
	
	/**
	 * @return the kickoffTouchback
	 */
	public Integer getKickoffTouchback() {
		return kickoffTouchback;
	}

	/**
	 * @param kickoffTouchback the kickoffTouchback to set
	 */
	public void setKickoffTouchback(Integer kickoffTouchback) {
		this.kickoffTouchback = kickoffTouchback;
	}

	/**
	 * @return the kickoffOnsideAttempt
	 */
	public Integer getKickoffOnsideAttempt() {
		return kickoffOnsideAttempt;
	}

	/**
	 * @param kickoffOnsideAttempt the kickoffOnsideAttempt to set
	 */
	public void setKickoffOnsideAttempt(Integer kickoffOnsideAttempt) {
		this.kickoffOnsideAttempt = kickoffOnsideAttempt;
	}

	/**
	 * @return the kickoffOnsideSuccess
	 */
	public Integer getKickoffOnsideSuccess() {
		return kickoffOnsideSuccess;
	}

	/**
	 * @param kickoffOnsideSuccess the kickoffOnsideSuccess to set
	 */
	public void setKickoffOnsideSuccess(Integer kickoffOnsideSuccess) {
		this.kickoffOnsideSuccess = kickoffOnsideSuccess;
	}

	/**
	 * @return the kickoff
	 */
	public Integer getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(Integer kickoff) {
		this.kickoff = kickoff;
	}

	/**
	 * @return the kickoffReturnYard
	 */
	public Integer getKickoffReturnYard() {
		return kickoffReturnYard;
	}

	/**
	 * @param kickoffReturnYard the kickoffReturnYard to set
	 */
	public void setKickoffReturnYard(Integer kickoffReturnYard) {
		this.kickoffReturnYard = kickoffReturnYard;
	}

	/**
	 * @return the kickoffFairCatch
	 */
	public Integer getKickoffFairCatch() {
		return kickoffFairCatch;
	}

	/**
	 * @param kickoffFairCatch the kickoffFairCatch to set
	 */
	public void setKickoffFairCatch(Integer kickoffFairCatch) {
		this.kickoffFairCatch = kickoffFairCatch;
	}

	/**
	 * @return the kickoffOutOfBounds
	 */
	public Integer getKickoffOutOfBounds() {
		return kickoffOutOfBounds;
	}

	/**
	 * @param kickoffOutOfBounds the kickoffOutOfBounds to set
	 */
	public void setKickoffOutOfBounds(Integer kickoffOutOfBounds) {
		this.kickoffOutOfBounds = kickoffOutOfBounds;
	}

	/**
	 * @return the kickoffReturnTouchdown
	 */
	public Integer getKickoffReturnTouchdown() {
		return kickoffReturnTouchdown;
	}

	/**
	 * @param kickoffReturnTouchdown the kickoffReturnTouchdown to set
	 */
	public void setKickoffReturnTouchdown(Integer kickoffReturnTouchdown) {
		this.kickoffReturnTouchdown = kickoffReturnTouchdown;
	}
	
	
	/**
	 * @return the kickoffLandYard
	 */
	public Integer getKickoffLandYard() {
		return kickoffLandYard;
	}

	/**
	 * @param kickoffLandYard the kickoffLandYard to set
	 */
	public void setKickoffLandYard(Integer kickoffLandYard) {
		this.kickoffLandYard = kickoffLandYard;
	}

	public void applyTouchback() {
		this.kickoffTouchback = 1;
		this.kickoffFairCatch = 0;
		this.kickoffReturnTouchdown = 0;
		this.kickoffReturnYard = 25;
	}
	
	public void applyOutOfBounds() {
		this.kickoffTouchback = 0;
		this.kickoffFairCatch = 0;
		this.kickoffReturnTouchdown = 0;
		this.kickoffReturnYard = 35;
		this.kickoffOutOfBounds = 1;
	}
	
	public void applyFairCatch() {
		this.kickoffTouchback = 0;
		this.kickoffFairCatch = 1;
		this.kickoffOutOfBounds = 0;
		this.kickoffReturnYard = 25 - (100 - this.kickoffLandYard);
		this.kickoffReturnTouchdown = 0;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(kickoff, kickoffFairCatch, kickoffLandYard, kickoffOnsideAttempt,
				kickoffOnsideSuccess, kickoffOutOfBounds, kickoffReturnTouchdown, kickoffReturnYard, kickoffTouchback);
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
		if (!(obj instanceof PbpPlayerStatKickoffPojo)) {
			return false;
		}
		PbpPlayerStatKickoffPojo other = (PbpPlayerStatKickoffPojo) obj;
		return Objects.equals(kickoff, other.kickoff) && Objects.equals(kickoffFairCatch, other.kickoffFairCatch)
				&& Objects.equals(kickoffLandYard, other.kickoffLandYard)
				&& Objects.equals(kickoffOnsideAttempt, other.kickoffOnsideAttempt)
				&& Objects.equals(kickoffOnsideSuccess, other.kickoffOnsideSuccess)
				&& Objects.equals(kickoffOutOfBounds, other.kickoffOutOfBounds)
				&& Objects.equals(kickoffReturnTouchdown, other.kickoffReturnTouchdown)
				&& Objects.equals(kickoffReturnYard, other.kickoffReturnYard)
				&& Objects.equals(kickoffTouchback, other.kickoffTouchback);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatKickoffPojo [kickoffTouchback=" + kickoffTouchback + ", kickoffOnsideAttempt="
				+ kickoffOnsideAttempt + ", kickoffOnsideSuccess=" + kickoffOnsideSuccess + ", kickoffReturnYard="
				+ kickoffReturnYard + ", kickoffOutOfBounds=" + kickoffOutOfBounds + ", kickoffReturnTouchdown="
				+ kickoffReturnTouchdown + ", kickoffFairCatch=" + kickoffFairCatch + ", kickoff=" + kickoff
				+ ", kickoffLandYard=" + kickoffLandYard + "]";
	}
	
	
}
