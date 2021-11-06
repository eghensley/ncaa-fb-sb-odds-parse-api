package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat;

import java.util.ArrayList;
import java.util.List;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.PlayerStatDefensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.PlayerStatOffensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.PlayerStatSpecialTeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.teamStats.TeamStatPenaltyPojo;

public class PlayerStatPojo {
	private PlayerStatOffensePojo offense;
	private PlayerStatDefensePojo defense;
	private PlayerStatSpecialTeamPojo specialTeam;
	private List<TeamStatPenaltyPojo> penalty;
	
	public PlayerStatPojo() {
		this.offense = new PlayerStatOffensePojo();
		this.defense = new PlayerStatDefensePojo();
		this.specialTeam = new PlayerStatSpecialTeamPojo();
		this.penalty = new ArrayList<>();
	}

	public PlayerStatPojo(PlayerStatOffensePojo offense, PlayerStatDefensePojo defense,
			PlayerStatSpecialTeamPojo specialTeam, List<TeamStatPenaltyPojo> penalty) {
		super();
		this.offense = offense;
		this.defense = defense;
		this.specialTeam = specialTeam;
		this.penalty = penalty;
	}

	/**
	 * @return the offense
	 */
	public PlayerStatOffensePojo getOffense() {
		return offense;
	}

	/**
	 * @param offense the offense to set
	 */
	public void setOffense(PlayerStatOffensePojo offense) {
		this.offense = offense;
	}

	/**
	 * @return the defense
	 */
	public PlayerStatDefensePojo getDefense() {
		return defense;
	}

	/**
	 * @param defense the defense to set
	 */
	public void setDefense(PlayerStatDefensePojo defense) {
		this.defense = defense;
	}

	/**
	 * @return the specialTeam
	 */
	public PlayerStatSpecialTeamPojo getSpecialTeam() {
		return specialTeam;
	}

	/**
	 * @param specialTeam the specialTeam to set
	 */
	public void setSpecialTeam(PlayerStatSpecialTeamPojo specialTeam) {
		this.specialTeam = specialTeam;
	}

	/**
	 * @return the penalty
	 */
	public List<TeamStatPenaltyPojo> getPenalty() {
		return penalty;
	}

	/**
	 * @param penalty the penalty to set
	 */
	public void setPenalty(List<TeamStatPenaltyPojo> penalty) {
		this.penalty = penalty;
	}
	
}
