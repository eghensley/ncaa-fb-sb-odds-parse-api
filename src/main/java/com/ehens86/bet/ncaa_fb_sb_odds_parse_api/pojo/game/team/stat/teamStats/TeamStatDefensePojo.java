package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

public class TeamStatDefensePojo {
	private TeamStatRushingPojo defenseRushing;
	private TeamStatPassingPojo defensePassing;
	private TeamStatFumblePojo defenseFumble;
	private TeamStatConversionPojo defenseConversion;
	private Integer defenseFirstDownPenalty;
	
	public TeamStatDefensePojo() {
		this.defenseRushing = new TeamStatRushingPojo();
		this.defensePassing = new TeamStatPassingPojo();
		this.defenseFumble = new TeamStatFumblePojo();
		this.defenseConversion = new TeamStatConversionPojo();
	}

	public TeamStatDefensePojo(TeamStatRushingPojo defenseRushing, TeamStatPassingPojo defensePassing,
			TeamStatFumblePojo defenseFumble, TeamStatConversionPojo defenseConversion, Integer defenseFirstDownPenalty) {
		super();
		this.defenseRushing = defenseRushing;
		this.defensePassing = defensePassing;
		this.defenseFumble = defenseFumble;
		this.defenseConversion = defenseConversion;
		this.defenseFirstDownPenalty = defenseFirstDownPenalty;
	}

	/**
	 * @return the defenseRushing
	 */
	public TeamStatRushingPojo getDefenseRushing() {
		return defenseRushing;
	}

	/**
	 * @param defenseRushing the defenseRushing to set
	 */
	public void setDefenseRushing(TeamStatRushingPojo defenseRushing) {
		this.defenseRushing = defenseRushing;
	}

	/**
	 * @return the defensePassing
	 */
	public TeamStatPassingPojo getDefensePassing() {
		return defensePassing;
	}

	/**
	 * @param defensePassing the defensePassing to set
	 */
	public void setDefensePassing(TeamStatPassingPojo defensePassing) {
		this.defensePassing = defensePassing;
	}

	/**
	 * @return the defenseFumble
	 */
	public TeamStatFumblePojo getDefenseFumble() {
		return defenseFumble;
	}

	/**
	 * @param defenseFumble the defenseFumble to set
	 */
	public void setDefenseFumble(TeamStatFumblePojo defenseFumble) {
		this.defenseFumble = defenseFumble;
	}

	/**
	 * @return the defenseConversion
	 */
	public TeamStatConversionPojo getDefenseConversion() {
		return defenseConversion;
	}

	/**
	 * @param defenseConversion the defenseConversion to set
	 */
	public void setDefenseConversion(TeamStatConversionPojo defenseConversion) {
		this.defenseConversion = defenseConversion;
	}

	/**
	 * @return the defenseFirstDownPenalty
	 */
	public Integer getDefenseFirstDownPenalty() {
		return defenseFirstDownPenalty;
	}

	/**
	 * @param defenseFirstDownPenalty the defenseFirstDownPenalty to set
	 */
	public void setDefenseFirstDownPenalty(Integer defenseFirstDownPenalty) {
		this.defenseFirstDownPenalty = defenseFirstDownPenalty;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((defenseConversion == null) ? 0 : defenseConversion.hashCode());
		result = prime * result + ((defenseFirstDownPenalty == null) ? 0 : defenseFirstDownPenalty.hashCode());
		result = prime * result + ((defenseFumble == null) ? 0 : defenseFumble.hashCode());
		result = prime * result + ((defensePassing == null) ? 0 : defensePassing.hashCode());
		result = prime * result + ((defenseRushing == null) ? 0 : defenseRushing.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatDefensePojo)) {
			return false;
		}
		TeamStatDefensePojo other = (TeamStatDefensePojo) obj;
		if (defenseConversion == null) {
			if (other.defenseConversion != null) {
				return false;
			}
		} else if (!defenseConversion.equals(other.defenseConversion)) {
			return false;
		}
		if (defenseFirstDownPenalty == null) {
			if (other.defenseFirstDownPenalty != null) {
				return false;
			}
		} else if (!defenseFirstDownPenalty.equals(other.defenseFirstDownPenalty)) {
			return false;
		}
		if (defenseFumble == null) {
			if (other.defenseFumble != null) {
				return false;
			}
		} else if (!defenseFumble.equals(other.defenseFumble)) {
			return false;
		}
		if (defensePassing == null) {
			if (other.defensePassing != null) {
				return false;
			}
		} else if (!defensePassing.equals(other.defensePassing)) {
			return false;
		}
		if (defenseRushing == null) {
			if (other.defenseRushing != null) {
				return false;
			}
		} else if (!defenseRushing.equals(other.defenseRushing)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatDefensePojo [defenseRushing=" + defenseRushing + ", defensePassing=" + defensePassing
				+ ", defenseFumble=" + defenseFumble + ", defenseConversion=" + defenseConversion
				+ ", defenseFirstDownPenalty=" + defenseFirstDownPenalty + "]";
	}
	
	
}
