package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PlayerStatPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.TeamStatPojo;

public class TeamPojo {

	private String teamName;
	private String teamNameShort;
	private String teamNameSixChar;
	private String teamNameSeo;
	private String teamColor;
	private String logoLink;
	private TeamStatPojo teamStat;
	private boolean home;
	private String ncaaTeamId;
	private PlayerStatPojo playerStat;

	public TeamPojo() {
		this.teamStat = new TeamStatPojo();
		this.playerStat = new PlayerStatPojo();
	}

	public TeamPojo(String teamName, String teamNameShort, String teamNameSixChar, String teamNameSeo, String teamColor,
			String logoLink, TeamStatPojo teamStat, boolean home, String ncaaTeamId, PlayerStatPojo playerStat) {
		super();
		this.teamName = teamName;
		this.teamNameShort = teamNameShort;
		this.teamNameSixChar = teamNameSixChar;
		this.teamNameSeo = teamNameSeo;
		this.teamColor = teamColor;
		this.logoLink = logoLink;
		this.teamStat = teamStat;
		this.home = home;
		this.ncaaTeamId = ncaaTeamId;
		this.playerStat = playerStat;
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
	 * @return the home
	 */
	public boolean isHome() {
		return home;
	}

	/**
	 * @param home the home to set
	 */
	public void setHome(boolean home) {
		this.home = home;
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (home ? 1231 : 1237);
		result = prime * result + ((logoLink == null) ? 0 : logoLink.hashCode());
		result = prime * result + ((ncaaTeamId == null) ? 0 : ncaaTeamId.hashCode());
		result = prime * result + ((playerStat == null) ? 0 : playerStat.hashCode());
		result = prime * result + ((teamColor == null) ? 0 : teamColor.hashCode());
		result = prime * result + ((teamName == null) ? 0 : teamName.hashCode());
		result = prime * result + ((teamNameSeo == null) ? 0 : teamNameSeo.hashCode());
		result = prime * result + ((teamNameShort == null) ? 0 : teamNameShort.hashCode());
		result = prime * result + ((teamNameSixChar == null) ? 0 : teamNameSixChar.hashCode());
		result = prime * result + ((teamStat == null) ? 0 : teamStat.hashCode());
		return result;
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
		if (home != other.home) {
			return false;
		}
		if (logoLink == null) {
			if (other.logoLink != null) {
				return false;
			}
		} else if (!logoLink.equals(other.logoLink)) {
			return false;
		}
		if (ncaaTeamId == null) {
			if (other.ncaaTeamId != null) {
				return false;
			}
		} else if (!ncaaTeamId.equals(other.ncaaTeamId)) {
			return false;
		}
		if (playerStat == null) {
			if (other.playerStat != null) {
				return false;
			}
		} else if (!playerStat.equals(other.playerStat)) {
			return false;
		}
		if (teamColor == null) {
			if (other.teamColor != null) {
				return false;
			}
		} else if (!teamColor.equals(other.teamColor)) {
			return false;
		}
		if (teamName == null) {
			if (other.teamName != null) {
				return false;
			}
		} else if (!teamName.equals(other.teamName)) {
			return false;
		}
		if (teamNameSeo == null) {
			if (other.teamNameSeo != null) {
				return false;
			}
		} else if (!teamNameSeo.equals(other.teamNameSeo)) {
			return false;
		}
		if (teamNameShort == null) {
			if (other.teamNameShort != null) {
				return false;
			}
		} else if (!teamNameShort.equals(other.teamNameShort)) {
			return false;
		}
		if (teamNameSixChar == null) {
			if (other.teamNameSixChar != null) {
				return false;
			}
		} else if (!teamNameSixChar.equals(other.teamNameSixChar)) {
			return false;
		}
		if (teamStat == null) {
			if (other.teamStat != null) {
				return false;
			}
		} else if (!teamStat.equals(other.teamStat)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamPojo [teamName=" + teamName + ", teamNameShort=" + teamNameShort + ", teamNameSixChar="
				+ teamNameSixChar + ", teamNameSeo=" + teamNameSeo + ", teamColor=" + teamColor + ", logoLink="
				+ logoLink + ", teamStat=" + teamStat + ", home=" + home + ", ncaaTeamId=" + ncaaTeamId
				+ ", playerStat=" + playerStat + "]";
	}

}
