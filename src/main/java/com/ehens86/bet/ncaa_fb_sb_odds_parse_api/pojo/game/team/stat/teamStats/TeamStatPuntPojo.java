package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

public class TeamStatPuntPojo {
	private Integer punt;
	private Integer puntYards;
	private Integer puntReturnYards;
	private Integer puntReturnTouchdown;
	
	public TeamStatPuntPojo() {
		this.puntReturnTouchdown = 0;
	}

	public TeamStatPuntPojo(Integer punt, Integer puntYards, Integer puntReturnYards, Integer puntReturnTouchdown) {
		super();
		this.punt = punt;
		this.puntYards = puntYards;
		this.puntReturnYards = puntReturnYards;
		this.puntReturnTouchdown = puntReturnTouchdown;
	}

	/**
	 * @return the punt
	 */
	public Integer getPunt() {
		return punt;
	}

	/**
	 * @param punt the punt to set
	 */
	public void setPunt(Integer punt) {
		this.punt = punt;
	}

	/**
	 * @return the puntYards
	 */
	public Integer getPuntYards() {
		return puntYards;
	}

	/**
	 * @param puntYards the puntYards to set
	 */
	public void setPuntYards(Integer puntYards) {
		this.puntYards = puntYards;
	}

	/**
	 * @return the puntReturnYards
	 */
	public Integer getPuntReturnYards() {
		return puntReturnYards;
	}

	/**
	 * @param puntReturnYards the puntReturnYards to set
	 */
	public void setPuntReturnYards(Integer puntReturnYards) {
		this.puntReturnYards = puntReturnYards;
	}

	/**
	 * @return the puntReturnTouchdown
	 */
	public Integer getPuntReturnTouchdown() {
		return puntReturnTouchdown;
	}

	/**
	 * @param puntReturnTouchdown the puntReturnTouchdown to set
	 */
	public void setPuntReturnTouchdown(Integer puntReturnTouchdown) {
		this.puntReturnTouchdown = puntReturnTouchdown;
	}
	
	/**
	 * @param puntReturnTouchdown the puntReturnTouchdown to set
	 */
	public void addPuntReturnTouchdown(Integer puntReturnTouchdown) {
		this.puntReturnTouchdown += puntReturnTouchdown;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((punt == null) ? 0 : punt.hashCode());
		result = prime * result + ((puntReturnTouchdown == null) ? 0 : puntReturnTouchdown.hashCode());
		result = prime * result + ((puntReturnYards == null) ? 0 : puntReturnYards.hashCode());
		result = prime * result + ((puntYards == null) ? 0 : puntYards.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatPuntPojo)) {
			return false;
		}
		TeamStatPuntPojo other = (TeamStatPuntPojo) obj;
		if (punt == null) {
			if (other.punt != null) {
				return false;
			}
		} else if (!punt.equals(other.punt)) {
			return false;
		}
		if (puntReturnTouchdown == null) {
			if (other.puntReturnTouchdown != null) {
				return false;
			}
		} else if (!puntReturnTouchdown.equals(other.puntReturnTouchdown)) {
			return false;
		}
		if (puntReturnYards == null) {
			if (other.puntReturnYards != null) {
				return false;
			}
		} else if (!puntReturnYards.equals(other.puntReturnYards)) {
			return false;
		}
		if (puntYards == null) {
			if (other.puntYards != null) {
				return false;
			}
		} else if (!puntYards.equals(other.puntYards)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatPunt [punt=" + punt + ", puntYards=" + puntYards + ", puntReturnYards=" + puntReturnYards
				+ ", puntReturnTouchdown=" + puntReturnTouchdown + "]";
	}

	
}
