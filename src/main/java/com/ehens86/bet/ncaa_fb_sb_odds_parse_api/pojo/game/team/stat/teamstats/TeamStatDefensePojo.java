package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats;

import java.util.Objects;

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
		return Objects.hash(defenseConversion, defenseFirstDownPenalty, defenseFumble, defensePassing, defenseRushing);
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
		return Objects.equals(defenseConversion, other.defenseConversion)
				&& Objects.equals(defenseFirstDownPenalty, other.defenseFirstDownPenalty)
				&& Objects.equals(defenseFumble, other.defenseFumble)
				&& Objects.equals(defensePassing, other.defensePassing)
				&& Objects.equals(defenseRushing, other.defenseRushing);
	}

	@Override
	public String toString() {
		return "TeamStatDefensePojo [defenseRushing=" + defenseRushing + ", defensePassing=" + defensePassing
				+ ", defenseFumble=" + defenseFumble + ", defenseConversion=" + defenseConversion
				+ ", defenseFirstDownPenalty=" + defenseFirstDownPenalty + "]";
	}
	
	
}
