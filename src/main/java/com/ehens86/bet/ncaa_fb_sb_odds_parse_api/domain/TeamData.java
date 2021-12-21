package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "TEAM")
public class TeamData  extends BaseAuditEntity implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = -3669398420536456473L;
	
	@Id
	@Column(name = "NCAA_TEAM_ID", updatable = false, nullable = false)
	private String ncaaTeamId;
	@Column(name = "NAME", nullable = false)
	private String teamName;
	@Column(name = "NAME_SHORT", nullable = false)
	private String teamNameShort;
	@Column(name = "NAME_SIX_CHAR", nullable = false)
	private String teamNameSixChar;
	@Column(name = "NAME_SEO", nullable = false)
	private String teamNameSeo;
	@Column(name = "MASCOT", nullable = false)
	private String teamNickname;
	@Column(name = "COLOR", nullable = false)
	private String teamColor;
	@Column(name = "LOGO", nullable = false)
	private String logoLink;

	public TeamData() {

	}

	public TeamData(String teamName, String teamNameShort, String teamNameSixChar, String teamNameSeo,
			String teamNickname, String teamColor, String logoLink, String ncaaTeamId) {
		super();
		this.teamName = teamName;
		this.teamNameShort = teamNameShort;
		this.teamNameSixChar = teamNameSixChar;
		this.teamNameSeo = teamNameSeo;
		this.teamNickname = teamNickname;
		this.teamColor = teamColor;
		this.logoLink = logoLink;
		this.ncaaTeamId = ncaaTeamId;
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

	@Override
	public int hashCode() {
		return Objects.hash(logoLink, ncaaTeamId, teamColor, teamName, teamNameSeo, teamNameShort, teamNameSixChar,
				teamNickname);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamData)) {
			return false;
		}
		TeamData other = (TeamData) obj;
		return Objects.equals(logoLink, other.logoLink) && Objects.equals(ncaaTeamId, other.ncaaTeamId)
				&& Objects.equals(teamColor, other.teamColor) && Objects.equals(teamName, other.teamName)
				&& Objects.equals(teamNameSeo, other.teamNameSeo) && Objects.equals(teamNameShort, other.teamNameShort)
				&& Objects.equals(teamNameSixChar, other.teamNameSixChar)
				&& Objects.equals(teamNickname, other.teamNickname);
	}

	@Override
	public String toString() {
		return "TeamData [teamName=" + teamName + ", teamNameShort=" + teamNameShort + ", teamNameSixChar="
				+ teamNameSixChar + ", teamNameSeo=" + teamNameSeo + ", teamNickname=" + teamNickname + ", teamColor="
				+ teamColor + ", logoLink=" + logoLink + ", ncaaTeamId=" + ncaaTeamId + "]";
	}

}
