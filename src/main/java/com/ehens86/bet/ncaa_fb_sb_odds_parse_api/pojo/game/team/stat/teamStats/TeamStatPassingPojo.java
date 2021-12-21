package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

import java.util.Objects;

public class TeamStatPassingPojo {
	private Integer passingFirstDown;
	private Integer passingYard;
	private Integer passingAttempt;
	private Integer passingCompletion;
	private Integer passingTouchdown;
	private Integer passingInterception;
	private Integer passingInterceptionYard;
	private Integer passingInterceptionTouchdown;

	public TeamStatPassingPojo() {
		this.passingInterceptionTouchdown = 0;
		this.passingInterceptionYard = 0;
	}

	public TeamStatPassingPojo(Integer passingFirstDown, Integer passingYard, Integer passingAttempt,
			Integer passingCompletion, Integer passingTouchdown, Integer passingInterception,
			Integer passingInterceptionYard, Integer passingInterceptionTouchdown) {
		super();
		this.passingFirstDown = passingFirstDown;
		this.passingYard = passingYard;
		this.passingAttempt = passingAttempt;
		this.passingCompletion = passingCompletion;
		this.passingTouchdown = passingTouchdown;
		this.passingInterception = passingInterception;
		this.passingInterceptionYard = passingInterceptionYard;
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
	 * @return the passingYard
	 */
	public Integer getPassingYard() {
		return passingYard;
	}

	/**
	 * @param passingYard the passingYard to set
	 */
	public void setPassingYard(Integer passingYard) {
		this.passingYard = passingYard;
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
	 * @return the passingCompletion
	 */
	public Integer getPassingCompletion() {
		return passingCompletion;
	}

	/**
	 * @param passingCompletion the passingCompletion to set
	 */
	public void setPassingCompletion(Integer passingCompletion) {
		this.passingCompletion = passingCompletion;
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
	 * @return the passingTouchdown
	 */
	public Integer getPassingTouchdown() {
		return passingTouchdown;
	}

	/**
	 * @param passingTouchdown the passingTouchdown to set
	 */
	public void setPassingTouchdown(Integer passingTouchdown) {
		this.passingTouchdown = passingTouchdown;
	}

	/**
	 * @param passingTouchdown the passingTouchdown to set
	 */
	public void addPassingInterceptionTouchdownAndYard(Integer passingInterceptionTouchdown,
			Integer passingInterceptionYard) {
		this.passingInterceptionTouchdown += passingInterceptionTouchdown;
		this.passingInterceptionYard += passingInterceptionYard;
	}

	@Override
	public int hashCode() {
		return Objects.hash(passingAttempt, passingCompletion, passingFirstDown, passingInterception,
				passingInterceptionTouchdown, passingInterceptionYard, passingTouchdown, passingYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatPassingPojo)) {
			return false;
		}
		TeamStatPassingPojo other = (TeamStatPassingPojo) obj;
		return Objects.equals(passingAttempt, other.passingAttempt)
				&& Objects.equals(passingCompletion, other.passingCompletion)
				&& Objects.equals(passingFirstDown, other.passingFirstDown)
				&& Objects.equals(passingInterception, other.passingInterception)
				&& Objects.equals(passingInterceptionTouchdown, other.passingInterceptionTouchdown)
				&& Objects.equals(passingInterceptionYard, other.passingInterceptionYard)
				&& Objects.equals(passingTouchdown, other.passingTouchdown)
				&& Objects.equals(passingYard, other.passingYard);
	}

	@Override
	public String toString() {
		return "TeamStatPassingPojo [passingFirstDown=" + passingFirstDown + ", passingYard=" + passingYard
				+ ", passingAttempt=" + passingAttempt + ", passingCompletion=" + passingCompletion
				+ ", passingTouchdown=" + passingTouchdown + ", passingInterception=" + passingInterception
				+ ", passingInterceptionYard=" + passingInterceptionYard + ", passingInterceptionTouchdown="
				+ passingInterceptionTouchdown + "]";
	}



}
