package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.PlayerStatDefensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.PlayerStatOffensePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.PlayerStatSpecialTeamPojo;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class PlayerStatPojo {
	@JsonIgnore
	private PlayerStatOffensePojo offense;
	@JsonIgnore
	private PlayerStatDefensePojo defense;
	private PlayerStatSpecialTeamPojo specialTeam;

	public PlayerStatPojo() {
		this.offense = new PlayerStatOffensePojo();
		this.defense = new PlayerStatDefensePojo();
		this.specialTeam = new PlayerStatSpecialTeamPojo();

	}

	public PlayerStatPojo(PlayerStatOffensePojo offense, PlayerStatDefensePojo defense,
			PlayerStatSpecialTeamPojo specialTeam) {
		super();
		this.offense = offense;
		this.defense = defense;
		this.specialTeam = specialTeam;
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((defense == null) ? 0 : defense.hashCode());
		result = prime * result + ((offense == null) ? 0 : offense.hashCode());
		result = prime * result + ((specialTeam == null) ? 0 : specialTeam.hashCode());
		return result;
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
		if (defense == null) {
			if (other.defense != null) {
				return false;
			}
		} else if (!defense.equals(other.defense)) {
			return false;
		}
		if (offense == null) {
			if (other.offense != null) {
				return false;
			}
		} else if (!offense.equals(other.offense)) {
			return false;
		}
		if (specialTeam == null) {
			if (other.specialTeam != null) {
				return false;
			}
		} else if (!specialTeam.equals(other.specialTeam)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatPojo [offense=" + offense + ", defense=" + defense + ", specialTeam=" + specialTeam + "]";
	}

}
