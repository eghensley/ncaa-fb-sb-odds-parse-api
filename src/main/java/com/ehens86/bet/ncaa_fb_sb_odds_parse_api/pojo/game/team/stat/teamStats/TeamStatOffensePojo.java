package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

public class TeamStatOffensePojo {
	private TeamStatRushingPojo offenseRushing;
	private TeamStatPassingPojo offensePassing;
	private TeamStatFumblePojo offenseFumble;
	private TeamStatConversionPojo offenseConversion;
	private Integer offenseFirstDownPenalty;
	
	public TeamStatOffensePojo() {
		this.offenseRushing = new TeamStatRushingPojo();
		this.offensePassing = new TeamStatPassingPojo();
		this.offenseFumble = new TeamStatFumblePojo();
		this.offenseConversion = new TeamStatConversionPojo();
	}

	public TeamStatOffensePojo(TeamStatRushingPojo offenseRushing, TeamStatPassingPojo offensePassing,
			TeamStatFumblePojo offenseFumble, TeamStatConversionPojo offenseConversion,
			Integer offenseFirstDownPenalty) {
		super();
		this.offenseRushing = offenseRushing;
		this.offensePassing = offensePassing;
		this.offenseFumble = offenseFumble;
		this.offenseConversion = offenseConversion;
		this.offenseFirstDownPenalty = offenseFirstDownPenalty;
	}

	/**
	 * @return the offenseRushing
	 */
	public TeamStatRushingPojo getOffenseRushing() {
		return offenseRushing;
	}

	/**
	 * @param offenseRushing the offenseRushing to set
	 */
	public void setOffenseRushing(TeamStatRushingPojo offenseRushing) {
		this.offenseRushing = offenseRushing;
	}

	/**
	 * @return the offensePassing
	 */
	public TeamStatPassingPojo getOffensePassing() {
		return offensePassing;
	}

	/**
	 * @param offensePassing the offensePassing to set
	 */
	public void setOffensePassing(TeamStatPassingPojo offensePassing) {
		this.offensePassing = offensePassing;
	}

	/**
	 * @return the offenseFumble
	 */
	public TeamStatFumblePojo getOffenseFumble() {
		return offenseFumble;
	}

	/**
	 * @param offenseFumble the offenseFumble to set
	 */
	public void setOffenseFumble(TeamStatFumblePojo offenseFumble) {
		this.offenseFumble = offenseFumble;
	}

	/**
	 * @return the offenseConversion
	 */
	public TeamStatConversionPojo getOffenseConversion() {
		return offenseConversion;
	}

	/**
	 * @param offenseConversion the offenseConversion to set
	 */
	public void setOffenseConversion(TeamStatConversionPojo offenseConversion) {
		this.offenseConversion = offenseConversion;
	}

	/**
	 * @return the offenseFirstDownPenalty
	 */
	public Integer getOffenseFirstDownPenalty() {
		return offenseFirstDownPenalty;
	}

	/**
	 * @param offenseFirstDownPenalty the offenseFirstDownPenalty to set
	 */
	public void setOffenseFirstDownPenalty(Integer offenseFirstDownPenalty) {
		this.offenseFirstDownPenalty = offenseFirstDownPenalty;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((offenseConversion == null) ? 0 : offenseConversion.hashCode());
		result = prime * result + ((offenseFirstDownPenalty == null) ? 0 : offenseFirstDownPenalty.hashCode());
		result = prime * result + ((offenseFumble == null) ? 0 : offenseFumble.hashCode());
		result = prime * result + ((offensePassing == null) ? 0 : offensePassing.hashCode());
		result = prime * result + ((offenseRushing == null) ? 0 : offenseRushing.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatOffensePojo)) {
			return false;
		}
		TeamStatOffensePojo other = (TeamStatOffensePojo) obj;
		if (offenseConversion == null) {
			if (other.offenseConversion != null) {
				return false;
			}
		} else if (!offenseConversion.equals(other.offenseConversion)) {
			return false;
		}
		if (offenseFirstDownPenalty == null) {
			if (other.offenseFirstDownPenalty != null) {
				return false;
			}
		} else if (!offenseFirstDownPenalty.equals(other.offenseFirstDownPenalty)) {
			return false;
		}
		if (offenseFumble == null) {
			if (other.offenseFumble != null) {
				return false;
			}
		} else if (!offenseFumble.equals(other.offenseFumble)) {
			return false;
		}
		if (offensePassing == null) {
			if (other.offensePassing != null) {
				return false;
			}
		} else if (!offensePassing.equals(other.offensePassing)) {
			return false;
		}
		if (offenseRushing == null) {
			if (other.offenseRushing != null) {
				return false;
			}
		} else if (!offenseRushing.equals(other.offenseRushing)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatOffensePojo [offenseRushing=" + offenseRushing + ", offensePassing=" + offensePassing
				+ ", offenseFumble=" + offenseFumble + ", offenseConversion=" + offenseConversion
				+ ", offenseFirstDownPenalty=" + offenseFirstDownPenalty + "]";
	}
	
	
	
}
