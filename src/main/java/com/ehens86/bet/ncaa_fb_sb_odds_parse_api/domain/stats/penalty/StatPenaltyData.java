package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.penalty;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.PlayerStatEntity;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PenaltyEnum;

@Entity
@Table(name = "STAT_PENALTY")
public class StatPenaltyData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -1471897722743852834L;
	@ManyToOne
	@JoinColumn(name = "PLAY_STAT_OID", referencedColumnName = "OID", nullable = false)
	private TeamPlayStatData playStat;
	@Column(name = "PENALTY", nullable = false)
	private Integer penalty;
	@Column(name = "PENALTY_YARD", nullable = false)
	private Integer penaltyYards;
	@Column(name = "PENALTY_FIRST_DOWN", nullable = false)
	private Integer penaltyFirstDown;
	@Column(name = "PENALTY_NAME", nullable = false)
	private PenaltyEnum penaltyName;
	
	public StatPenaltyData() {
		// Base constructor
	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
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

	/**
	 * @return the penaltyFirstDown
	 */
	public Integer getPenaltyFirstDown() {
		return penaltyFirstDown;
	}

	/**
	 * @param penaltyFirstDown the penaltyFirstDown to set
	 */
	public void setPenaltyFirstDown(Integer penaltyFirstDown) {
		this.penaltyFirstDown = penaltyFirstDown;
	}

	/**
	 * @return the penaltyName
	 */
	public PenaltyEnum getPenaltyName() {
		return penaltyName;
	}

	/**
	 * @param penaltyName the penaltyName to set
	 */
	public void setPenaltyName(PenaltyEnum penaltyName) {
		this.penaltyName = penaltyName;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(penalty, penaltyFirstDown, penaltyName, penaltyYards, playStat);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (!(obj instanceof StatPenaltyData)) {
			return false;
		}
		StatPenaltyData other = (StatPenaltyData) obj;
		return Objects.equals(penalty, other.penalty) && Objects.equals(penaltyFirstDown, other.penaltyFirstDown)
				&& penaltyName == other.penaltyName && Objects.equals(penaltyYards, other.penaltyYards)
				&& Objects.equals(playStat, other.playStat);
	}

	@Override
	public String toString() {
		return "StatPenaltyData [playStat=" + playStat + ", penalty=" + penalty + ", penaltyYards=" + penaltyYards
				+ ", penaltyFirstDown=" + penaltyFirstDown + ", penaltyName=" + penaltyName + "]";
	}
	
	
}
