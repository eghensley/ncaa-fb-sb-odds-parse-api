package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.ParseTargetEnum;

public class ParseRequest {

	private ParseTargetEnum target;
	private String fightId;
	private String boutId;
	private String fighterId;
	private String fighterBoutXrefOid;
	
	public ParseRequest() {
		
	}
	
	public ParseRequest(ParseTargetEnum target, String fightId, String boutId, String fighterId) {
		this.target = target;
		this.fightId = fightId;
		this.boutId = boutId;
		this.fighterId = fighterId;
	}

	public ParseRequest(ParseTargetEnum target, String fightId, String boutId, String fighterId, String fighterBoutXrefOid) {
		this.target = target;
		this.fightId = fightId;
		this.boutId = boutId;
		this.fighterId = fighterId;
		this.fighterBoutXrefOid = fighterBoutXrefOid;
	}
	
	/**
	 * @return the target
	 */
	public ParseTargetEnum getTarget() {
		return target;
	}

	/**
	 * @param target the target to set
	 */
	public void setTarget(ParseTargetEnum target) {
		this.target = target;
	}

	/**
	 * @return the fightId
	 */
	public String getFightId() {
		return fightId;
	}

	/**
	 * @param fightId the fightId to set
	 */
	public void setFightId(String fightId) {
		this.fightId = fightId;
	}

	/**
	 * @return the boutId
	 */
	public String getBoutId() {
		return boutId;
	}

	/**
	 * @param boutId the boutId to set
	 */
	public void setBoutId(String boutId) {
		this.boutId = boutId;
	}

	/**
	 * @return the fighterId
	 */
	public String getFighterId() {
		return fighterId;
	}

	/**
	 * @param fighterId the fighterId to set
	 */
	public void setFighterId(String fighterId) {
		this.fighterId = fighterId;
	}

	/**
	 * @return the fighterBoutXrefOid
	 */
	public String getFighterBoutXrefOid() {
		return fighterBoutXrefOid;
	}

	/**
	 * @param fighterBoutXrefOid the fighterBoutXrefOid to set
	 */
	public void setFighterBoutXrefOid(String fighterBoutXrefOid) {
		this.fighterBoutXrefOid = fighterBoutXrefOid;
	}

}
