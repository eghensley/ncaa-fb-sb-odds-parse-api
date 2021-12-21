package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats;

import java.util.ArrayList;
import java.util.List;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player.PlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player.PlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.player.PlayerStatRushingPojo;


public class PlayerStatOffensePojo {
	private List<PlayerStatRushingPojo> rushingStat;
	private List<PlayerStatPassingPojo> passingStat;
	private List<PlayerStatReceivingPojo> receivingStat;

	public PlayerStatOffensePojo() {
		this.rushingStat = new ArrayList<>();
		this.passingStat = new ArrayList<>();
		this.receivingStat = new ArrayList<>();
	}

	public PlayerStatOffensePojo(List<PlayerStatRushingPojo> rushingStat, List<PlayerStatPassingPojo> passingStat,
			List<PlayerStatReceivingPojo> receivingStat) {
		super();
		this.rushingStat = rushingStat;
		this.passingStat = passingStat;
		this.receivingStat = receivingStat;
	}

	/**
	 * @return the rushingStat
	 */
	public List<PlayerStatRushingPojo> getRushingStat() {
		return rushingStat;
	}

	/**
	 * @param rushingStat the rushingStat to set
	 */
	public void setRushingStat(List<PlayerStatRushingPojo> rushingStat) {
		this.rushingStat = rushingStat;
	}

	/**
	 * @return the passingStat
	 */
	public List<PlayerStatPassingPojo> getPassingStat() {
		return passingStat;
	}

	/**
	 * @param passingStat the passingStat to set
	 */
	public void setPassingStat(List<PlayerStatPassingPojo> passingStat) {
		this.passingStat = passingStat;
	}

	/**
	 * @return the receivingStat
	 */
	public List<PlayerStatReceivingPojo> getReceivingStat() {
		return receivingStat;
	}

	/**
	 * @param receivingStat the receivingStat to set
	 */
	public void setReceivingStat(List<PlayerStatReceivingPojo> receivingStat) {
		this.receivingStat = receivingStat;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((passingStat == null) ? 0 : passingStat.hashCode());
		result = prime * result + ((receivingStat == null) ? 0 : receivingStat.hashCode());
		result = prime * result + ((rushingStat == null) ? 0 : rushingStat.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatOffensePojo)) {
			return false;
		}
		PlayerStatOffensePojo other = (PlayerStatOffensePojo) obj;
		if (passingStat == null) {
			if (other.passingStat != null) {
				return false;
			}
		} else if (!passingStat.equals(other.passingStat)) {
			return false;
		}
		if (receivingStat == null) {
			if (other.receivingStat != null) {
				return false;
			}
		} else if (!receivingStat.equals(other.receivingStat)) {
			return false;
		}
		if (rushingStat == null) {
			if (other.rushingStat != null) {
				return false;
			}
		} else if (!rushingStat.equals(other.rushingStat)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatOffensePojo [rushingStat=" + rushingStat + ", passingStat=" + passingStat + ", receivingStat="
				+ receivingStat + "]";
	}

}
