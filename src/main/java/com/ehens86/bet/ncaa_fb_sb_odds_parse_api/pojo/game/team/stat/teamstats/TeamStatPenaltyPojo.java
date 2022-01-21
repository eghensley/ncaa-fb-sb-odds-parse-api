package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamstats;

import java.util.Objects;

public class TeamStatPenaltyPojo {
	protected Integer penalty;
	protected Integer penaltyYards;

	
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
		return Objects.hash(penalty, penaltyYards);
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
		return Objects.equals(penalty, other.penalty) && Objects.equals(penaltyYards, other.penaltyYards);
	}

	@Override
	public String toString() {
		return "TeamStatPenaltyPojo [penalty=" + penalty + ", penaltyYards=" + penaltyYards + "]";
	}



	
}
