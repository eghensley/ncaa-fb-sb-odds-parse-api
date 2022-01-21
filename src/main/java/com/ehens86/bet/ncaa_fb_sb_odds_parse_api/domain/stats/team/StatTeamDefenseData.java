package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.team;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.BaseStatEntity;

@Entity
@Table(name = "STAT_TEAM_DEFENSE")
public class StatTeamDefenseData extends BaseStatEntity implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6369525655783996224L;
	@OneToOne(mappedBy = "rushStat")
	private TeamPlayStatData playStat;
	@Column(name = "F_HAVOC_DB", nullable = false)
	private boolean havocDb;
	@Column(name = "F_HAVOC_FRONT", nullable = false)
	private boolean havocFront;
	@Column(name = "F_HAVOC", nullable = false)
	private boolean havoc;
	@Column(name = "F_DEFEAT", nullable = false)
	private boolean defeat;

	public StatTeamDefenseData() {
		// Base constructor
	}

	/**
	 * @return the havocDb
	 */
	public boolean getHavocDb() {
		return havocDb;
	}

	/**
	 * @param havocDb the havocDb to set
	 */
	public void setHavocDb(boolean havocDb) {
		this.havocDb = havocDb;
	}

	/**
	 * @return the havocFront
	 */
	public boolean getHavocFront() {
		return havocFront;
	}

	/**
	 * @param havocFront the havocFront to set
	 */
	public void setHavocFront(boolean havocFront) {
		this.havocFront = havocFront;
	}

	/**
	 * @return the havoc
	 */
	public boolean getHavoc() {
		return havoc;
	}

	/**
	 * @param havoc the havoc to set
	 */
	public void setHavoc(boolean havoc) {
		this.havoc = havoc;
	}

	/**
	 * @return the defeat
	 */
	public boolean getDefeat() {
		return defeat;
	}

	/**
	 * @param playDefeat the defeat to set
	 */
	public void setDefeat(boolean defeat) {
		this.defeat = defeat;
	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (havoc ? 1231 : 1237);
		result = prime * result + (havocDb ? 1231 : 1237);
		result = prime * result + (havocFront ? 1231 : 1237);
		result = prime * result + (defeat ? 1231 : 1237);
		result = prime * result + ((playStat == null) ? 0 : playStat.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		StatTeamDefenseData other = (StatTeamDefenseData) obj;
		if (havoc != other.havoc)
			return false;
		if (havocDb != other.havocDb)
			return false;
		if (havocFront != other.havocFront)
			return false;
		if (defeat != other.defeat)
			return false;
		if (playStat == null) {
			if (other.playStat != null)
				return false;
		} else if (!playStat.equals(other.playStat))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "StatTeamDefenseData [playStat=" + playStat + ", havocDb=" + havocDb + ", havocFront=" + havocFront
				+ ", havoc=" + havoc + ", defeat=" + defeat + "]";
	}

}
