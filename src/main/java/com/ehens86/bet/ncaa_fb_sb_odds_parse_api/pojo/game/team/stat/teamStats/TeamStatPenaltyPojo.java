package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats;

public class TeamStatPenaltyPojo {
	private Integer penalty;
	private Integer penaltyYards;
	
	public TeamStatPenaltyPojo() {
		
	}

	public TeamStatPenaltyPojo(Integer penalty, Integer penaltyYards) {
		super();
		this.penalty = penalty;
		this.penaltyYards = penaltyYards;
	}

	/**
	 * @return the penalty
	 */
	public Integer getPenalty() {
		return penalty;
	}

	/**
	 * @param penalty the penalty to set
	 */
	public void setPenalty(Integer penalty) {
		this.penalty = penalty;
	}

	/**
	 * @return the penaltyYards
	 */
	public Integer getPenaltyYards() {
		return penaltyYards;
	}

	/**
	 * @param penaltyYards the penaltyYards to set
	 */
	public void setPenaltyYards(Integer penaltyYards) {
		this.penaltyYards = penaltyYards;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((penalty == null) ? 0 : penalty.hashCode());
		result = prime * result + ((penaltyYards == null) ? 0 : penaltyYards.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatPenaltyPojo)) {
			return false;
		}
		TeamStatPenaltyPojo other = (TeamStatPenaltyPojo) obj;
		if (penalty == null) {
			if (other.penalty != null) {
				return false;
			}
		} else if (!penalty.equals(other.penalty)) {
			return false;
		}
		if (penaltyYards == null) {
			if (other.penaltyYards != null) {
				return false;
			}
		} else if (!penaltyYards.equals(other.penaltyYards)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatPenaltyPojo [penalty=" + penalty + ", penaltyYards=" + penaltyYards + "]";
	}
	
	
}
