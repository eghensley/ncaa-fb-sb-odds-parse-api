package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.team;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.BaseStatEntity;

@Entity
@Table(name = "STAT_TEAM_DEFENSE")
public class StatTeamDefenseData extends BaseStatEntity implements Serializable{

	@Column(name = "DEF_HAVOC_DB", nullable = false)
	private Integer havocDb;
	@Column(name = "DEF_HAVOC_FRONT", nullable = false)
	private Integer havocFront;
	@Column(name = "DEF_HAVOC", nullable = false)
	private Integer havoc;
	
	/**
	 * @return the havocDb
	 */
	public Integer getHavocDb() {
		return havocDb;
	}

	/**
	 * @param havocDb the havocDb to set
	 */
	public void setHavocDb(Integer havocDb) {
		this.havocDb = havocDb;
	}

	/**
	 * @return the havocFront
	 */
	public Integer getHavocFront() {
		return havocFront;
	}

	/**
	 * @param havocFront the havocFront to set
	 */
	public void setHavocFront(Integer havocFront) {
		this.havocFront = havocFront;
	}

	/**
	 * @return the havoc
	 */
	public Integer getHavoc() {
		return havoc;
	}

	/**
	 * @param havoc the havoc to set
	 */
	public void setHavoc(Integer havoc) {
		this.havoc = havoc;
	}
}
