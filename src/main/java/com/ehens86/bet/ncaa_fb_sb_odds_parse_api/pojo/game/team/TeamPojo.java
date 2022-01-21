package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team;

import java.util.Objects;

import javax.persistence.Id;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PlayerStatPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.TeamStatPojo;

public class TeamPojo {

	private String teamName;
	private String teamNameShort;
	private String teamNameSixChar;
	private String teamNameSeo;
	private String teamNickname;
	private String teamColor;
	private String logoLink;
	private TeamStatPojo teamStat;
	@Id
	private String ncaaTeamId;
	private PlayerStatPojo playerStat;
	private Integer score;

	public TeamPojo() {
		this.teamStat = new TeamStatPojo();
		this.playerStat = new PlayerStatPojo();
	}

	/**
	 * @return the teamName
	 */
	public String getTeamName() {
		return teamName;
	}

	/**
	 * @param teamName the teamName to set
	 */
	public void setTeamName(String teamName) {
		this.teamName = teamName;
	}

	/**
	 * @return the teamNameShort
	 */
	public String getTeamNameShort() {
		return teamNameShort;
	}

	/**
	 * @param teamNameShort the teamNameShort to set
	 */
	public void setTeamNameShort(String teamNameShort) {
		this.teamNameShort = teamNameShort;
	}

	/**
	 * @return the teamNameSixChar
	 */
	public String getTeamNameSixChar() {
		return teamNameSixChar;
	}

	/**
	 * @param teamNameSixChar the teamNameSixChar to set
	 */
	public void setTeamNameSixChar(String teamNameSixChar) {
		this.teamNameSixChar = teamNameSixChar;
	}

	/**
	 * @return the teamNameSeo
	 */
	public String getTeamNameSeo() {
		return teamNameSeo;
	}

	/**
	 * @param teamNameSeo the teamNameSeo to set
	 */
	public void setTeamNameSeo(String teamNameSeo) {
		this.teamNameSeo = teamNameSeo;
		this.logoLink = String.format("https://i.turner.ncaa.com/sites/default/files/images/logos/schools/bgl/%s.svg",
				teamNameSeo);
	}

	/**
	 * @return the teamColor
	 */
	public String getTeamColor() {
		return teamColor;
	}

	/**
	 * @param teamColor the teamColor to set
	 */
	public void setTeamColor(String teamColor) {
		this.teamColor = teamColor;
	}

	/**
	 * @return the logoLink
	 */
	public String getLogoLink() {
		return logoLink;
	}

	/**
	 * @param logoLink the logoLink to set
	 */
	public void setLogoLink(String logoLink) {
		this.logoLink = logoLink;
	}

	/**
	 * @return the teamStat
	 */
	public TeamStatPojo getTeamStat() {
		return teamStat;
	}

	/**
	 * @param teamStat the teamStat to set
	 */
	public void setTeamStat(TeamStatPojo teamStat) {
		this.teamStat = teamStat;
	}

	/**
	 * @return the ncaaTeamId
	 */
	public String getNcaaTeamId() {
		return ncaaTeamId;
	}

	/**
	 * @param ncaaTeamId the ncaaTeamId to set
	 */
	public void setNcaaTeamId(String ncaaTeamId) {
		this.ncaaTeamId = ncaaTeamId;
	}

	/**
	 * @return the playerStat
	 */
	public PlayerStatPojo getPlayerStat() {
		return playerStat;
	}

	/**
	 * @param playerStat the playerStat to set
	 */
	public void setPlayerStat(PlayerStatPojo playerStat) {
		this.playerStat = playerStat;
	}

	/**
	 * @return the teamNickname
	 */
	public String getTeamNickname() {
		return teamNickname;
	}

	/**
	 * @param teamNickname the teamNickname to set
	 */
	public void setTeamNickname(String teamNickname) {
		this.teamNickname = teamNickname;
	}

	
	/**
	 * @return the score
	 */
	public Integer getScore() {
		return score;
	}

	/**
	 * @param score the score to set
	 */
	public void setScore(Integer score) {
		this.score = score;
	}

	@Override
	public int hashCode() {
		return Objects.hash(logoLink, ncaaTeamId, playerStat, score, teamColor, teamName, teamNameSeo,
				teamNameShort, teamNameSixChar, teamNickname, teamStat);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamPojo)) {
			return false;
		}
		TeamPojo other = (TeamPojo) obj;
		return Objects.equals(logoLink, other.logoLink)
				&& Objects.equals(ncaaTeamId, other.ncaaTeamId) && Objects.equals(playerStat, other.playerStat)
				&& Objects.equals(score, other.score) && Objects.equals(teamColor, other.teamColor)
				&& Objects.equals(teamName, other.teamName) && Objects.equals(teamNameSeo, other.teamNameSeo)
				&& Objects.equals(teamNameShort, other.teamNameShort)
				&& Objects.equals(teamNameSixChar, other.teamNameSixChar)
				&& Objects.equals(teamNickname, other.teamNickname) && Objects.equals(teamStat, other.teamStat);
	}

	@Override
	public String toString() {
		return "TeamPojo [teamName=" + teamName + ", teamNameShort=" + teamNameShort + ", teamNameSixChar="
				+ teamNameSixChar + ", teamNameSeo=" + teamNameSeo + ", teamNickname=" + teamNickname + ", teamColor="
				+ teamColor + ", logoLink=" + logoLink + ", teamStat=" + teamStat + ", ncaaTeamId="
				+ ncaaTeamId + ", playerStat=" + playerStat + ", score=" + score + "]";
	}



}
