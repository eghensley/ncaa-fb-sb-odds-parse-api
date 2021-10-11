package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((fumble == null) ? 0 : fumble.hashCode());
		result = prime * result + ((fumbleLost == null) ? 0 : fumbleLost.hashCode());
		result = prime * result + ((fumbleTouchdown == null) ? 0 : fumbleTouchdown.hashCode());
		result = prime * result + ((fumbleYard == null) ? 0 : fumbleYard.hashCode());
		return result;
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
		if (fumble == null) {
			if (other.fumble != null) {
				return false;
			}
		} else if (!fumble.equals(other.fumble)) {
			return false;
		}
		if (fumbleLost == null) {
			if (other.fumbleLost != null) {
				return false;
			}
		} else if (!fumbleLost.equals(other.fumbleLost)) {
			return false;
		}
		if (fumbleTouchdown == null) {
			if (other.fumbleTouchdown != null) {
				return false;
			}
		} else if (!fumbleTouchdown.equals(other.fumbleTouchdown)) {
			return false;
		}
		if (fumbleYard == null) {
			if (other.fumbleYard != null) {
				return false;
			}
		} else if (!fumbleYard.equals(other.fumbleYard)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatFumblePojo [fumble=" + fumble + ", fumbleLost=" + fumbleLost + ", fumbleYard=" + fumbleYard
				+ ", fumbleTouchdown=" + fumbleTouchdown + "]";
	}


}
