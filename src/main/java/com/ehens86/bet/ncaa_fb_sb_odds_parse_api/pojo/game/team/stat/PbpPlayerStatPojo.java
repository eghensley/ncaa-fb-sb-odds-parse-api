package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PbpPlayerStatDefensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PbpPlayerStatOffensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PbpPlayerStatSpecialTeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;

public class PbpPlayerStatPojo {
	private PbpPlayerStatOffensePojo offense;
	private PbpPlayerStatDefensePojo defense;
	private PbpPlayerStatSpecialTeamPojo specialTeam;
	private List<PlayerStatPenaltyPojo> penalty;

	public PbpPlayerStatPojo() {
		this.offense = new PbpPlayerStatOffensePojo();
		this.defense = new PbpPlayerStatDefensePojo();
		this.specialTeam = new PbpPlayerStatSpecialTeamPojo();
		this.penalty = new ArrayList<>();
	}

	public PbpPlayerStatPojo(PbpPlayerStatOffensePojo offense, PbpPlayerStatDefensePojo defense,
			PbpPlayerStatSpecialTeamPojo specialTeam, List<PlayerStatPenaltyPojo> penalty) {
		super();
		this.offense = offense;
		this.defense = defense;
		this.specialTeam = specialTeam;
		this.penalty = penalty;
	}

	/**
	 * @return the offense
	 */
	public PbpPlayerStatOffensePojo getOffense() {
		return offense;
	}

	/**
	 * @param offense the offense to set
	 */
	public void setOffense(PbpPlayerStatOffensePojo offense) {
		this.offense = offense;
	}

	/**
	 * @return the defense
	 */
	public PbpPlayerStatDefensePojo getDefense() {
		return defense;
	}

	/**
	 * @param defense the defense to set
	 */
	public void setDefense(PbpPlayerStatDefensePojo defense) {
		this.defense = defense;
	}

	/**
	 * @return the specialTeam
	 */
	public PbpPlayerStatSpecialTeamPojo getSpecialTeam() {
		return specialTeam;
	}

	/**
	 * @param specialTeam the specialTeam to set
	 */
	public void setSpecialTeam(PbpPlayerStatSpecialTeamPojo specialTeam) {
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
		if (!(obj instanceof PbpPlayerStatPojo)) {
			return false;
		}
		PbpPlayerStatPojo other = (PbpPlayerStatPojo) obj;
		return Objects.equals(defense, other.defense) && Objects.equals(offense, other.offense)
				&& Objects.equals(penalty, other.penalty) && Objects.equals(specialTeam, other.specialTeam);
	}

	@Override
	public String toString() {
		return "PbpPlayerStat [offense=" + offense + ", defense=" + defense + ", specialTeam=" + specialTeam
				+ ", penalty=" + penalty + "]";
	}

}
