package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatDefensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatOffensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatSpecialTeamPojo;

public class PlayerStatPojo {
	private PlayerStatOffensePojo offense;
	private PlayerStatDefensePojo defense;
	private PlayerStatSpecialTeamPojo specialTeam;
	private List<PlayerStatPenaltyPojo> penalty;
	
	public PlayerStatPojo() {
		this.offense = new PlayerStatOffensePojo();
		this.defense = new PlayerStatDefensePojo();
		this.specialTeam = new PlayerStatSpecialTeamPojo();
		this.penalty = new ArrayList<>();
	}

	public PlayerStatPojo(PlayerStatOffensePojo offense, PlayerStatDefensePojo defense,
			PlayerStatSpecialTeamPojo specialTeam, List<PlayerStatPenaltyPojo> penalty) {
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
	public List<PlayerStatPenaltyPojo> getPenalty() {
		return penalty;
	}

	/**
	 * @param penalty the penalty to set
	 */
	public void setPenalty(List<PlayerStatPenaltyPojo> penalty) {
		this.penalty = penalty;
	}

	@Override
	public int hashCode() {
		return Objects.hash(defense, offense, penalty, specialTeam);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatPojo)) {
			return false;
		}
		PlayerStatPojo other = (PlayerStatPojo) obj;
		return Objects.equals(defense, other.defense) && Objects.equals(offense, other.offense)
				&& Objects.equals(penalty, other.penalty) && Objects.equals(specialTeam, other.specialTeam);
	}

	@Override
	public String toString() {
		return "PlayerStatPojo [offense=" + offense + ", defense=" + defense + ", specialTeam=" + specialTeam
				+ ", penalty=" + penalty + "]";
	}
	
}
