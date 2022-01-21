package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats;

import java.util.Objects;

public class TeamStatFumblePojo {
	private Integer fumble;
	private Integer fumbleLost;
	private Integer fumbleYard;
	private Integer fumbleTouchdown;
	
	public TeamStatFumblePojo() {
		this.fumbleYard = 0;
		this.fumbleTouchdown = 0;
	}

	public TeamStatFumblePojo(Integer fumble, Integer fumbleLost, Integer fumbleYard, Integer fumbleTouchdown) {
		super();
		this.fumble = fumble;
		this.fumbleLost = fumbleLost;
		this.fumbleYard = fumbleYard;
		this.fumbleTouchdown = fumbleTouchdown;
	}

	/**
	 * @return the fumble
	 */
	public Integer getFumble() {
		return fumble;
	}

	/**
	 * @param fumble the fumble to set
	 */
	public void setFumble(Integer fumble) {
		this.fumble = fumble;
	}

	/**
	 * @return the fumbleLost
	 */
	public Integer getFumbleLost() {
		return fumbleLost;
	}

	/**
	 * @param fumbleLost the fumbleLost to set
	 */
	public void setFumbleLost(Integer fumbleLost) {
		this.fumbleLost = fumbleLost;
	}

	/**
	 * @return the fumbleYard
	 */
	public Integer getFumbleYard() {
		return fumbleYard;
	}

	/**
	 * @param fumbleYards the fumbleYard to set
	 */
	public void setFumbleYard(Integer fumbleYard) {
		this.fumbleYard = fumbleYard;
	}
	
	/**
	 * @param fumbleYards the fumbleYard to set
	 */
	public void addFumbleYard(Integer fumbleYard) {
		this.fumbleYard += fumbleYard;
	}

	/**
	 * @return the fumbleTouchdown
	 */
	public Integer getFumbleTouchdown() {
		return fumbleTouchdown;
	}

	/**
	 * @param fumbleTouchdown the fumbleTouchdown to set
	 */
	public void setFumbleTouchdown(Integer fumbleTouchdown) {
		this.fumbleTouchdown = fumbleTouchdown;
	}

	/**
	 * @param fumbleTouchdown the fumbleTouchdown to set
	 */
	public void addFumbleTouchdown(Integer fumbleTouchdown) {
		this.fumbleTouchdown += fumbleTouchdown;
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(fumble, fumbleLost, fumbleTouchdown, fumbleYard);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatFumblePojo)) {
			return false;
		}
		TeamStatFumblePojo other = (TeamStatFumblePojo) obj;
		return Objects.equals(fumble, other.fumble) && Objects.equals(fumbleLost, other.fumbleLost)
				&& Objects.equals(fumbleTouchdown, other.fumbleTouchdown)
				&& Objects.equals(fumbleYard, other.fumbleYard);
	}

	@Override
	public String toString() {
		return "TeamStatFumblePojo [fumble=" + fumble + ", fumbleLost=" + fumbleLost + ", fumbleYard=" + fumbleYard
				+ ", fumbleTouchdown=" + fumbleTouchdown + "]";
	}


}
