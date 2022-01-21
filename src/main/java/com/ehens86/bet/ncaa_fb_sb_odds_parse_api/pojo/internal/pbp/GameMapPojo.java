package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp;

import java.util.Map;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayTeamPojo;

public class GameMapPojo {
	private Map<String, HomeAwayEnum> teamDict;
	private Map<String, PlayByPlayTeamPojo> teamAbbrevDict;

	public GameMapPojo() {
		// base constructor
	}

	public GameMapPojo(Map<String, HomeAwayEnum> teamDict, Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		super();
		this.teamDict = teamDict;
		this.teamAbbrevDict = teamAbbrevDict;
	}

	/**
	 * @return the teamDict
	 */
	public Map<String, HomeAwayEnum> getTeamDict() {
		return teamDict;
	}

	/**
	 * @param teamDict the teamDict to set
	 */
	public void setTeamDict(Map<String, HomeAwayEnum> teamDict) {
		this.teamDict = teamDict;
	}

	/**
	 * @return the teamAbbrevDict
	 */
	public Map<String, PlayByPlayTeamPojo> getTeamAbbrevDict() {
		return teamAbbrevDict;
	}

	/**
	 * @param teamAbbrevDict the teamAbbrevDict to set
	 */
	public void setTeamAbbrevDict(Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		this.teamAbbrevDict = teamAbbrevDict;
	}

	@Override
	public int hashCode() {
		return Objects.hash(teamAbbrevDict, teamDict);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameMapPojo)) {
			return false;
		}
		GameMapPojo other = (GameMapPojo) obj;
		return Objects.equals(teamAbbrevDict, other.teamAbbrevDict) && Objects.equals(teamDict, other.teamDict);
	}

	@Override
	public String toString() {
		return "GameMapPojo [teamDict=" + teamDict + ", teamAbbrevDict=" + teamAbbrevDict + "]";
	}

}
