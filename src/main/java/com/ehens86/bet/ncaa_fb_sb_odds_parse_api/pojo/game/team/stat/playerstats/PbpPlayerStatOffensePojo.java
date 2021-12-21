package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;

public class PbpPlayerStatOffensePojo {
	private List<PbpPlayerStatRushingPojo> rushingStat;
	private List<PbpPlayerStatPassingPojo> passingStat;
	private List<PbpPlayerStatReceivingPojo> receivingStat;

	public PbpPlayerStatOffensePojo() {
		this.rushingStat = new ArrayList<>();
		this.passingStat = new ArrayList<>();
		this.receivingStat = new ArrayList<>();
	}
	
	public PbpPlayerStatOffensePojo(List<PbpPlayerStatRushingPojo> rushingStat,
			List<PbpPlayerStatPassingPojo> passingStat, List<PbpPlayerStatReceivingPojo> receivingStat) {
		super();
		this.rushingStat = rushingStat;
		this.passingStat = passingStat;
		this.receivingStat = receivingStat;
	}

	/**
	 * @return the rushingStat
	 */
	public List<PbpPlayerStatRushingPojo> getRushingStat() {
		return rushingStat;
	}

	/**
	 * @param rushingStat the rushingStat to set
	 */
	public void setRushingStat(List<PbpPlayerStatRushingPojo> rushingStat) {
		this.rushingStat = rushingStat;
	}

	/**
	 * @return the passingStat
	 */
	public List<PbpPlayerStatPassingPojo> getPassingStat() {
		return passingStat;
	}

	/**
	 * @param passingStat the passingStat to set
	 */
	public void setPassingStat(List<PbpPlayerStatPassingPojo> passingStat) {
		this.passingStat = passingStat;
	}

	/**
	 * @return the receivingStat
	 */
	public List<PbpPlayerStatReceivingPojo> getReceivingStat() {
		return receivingStat;
	}

	/**
	 * @param receivingStat the receivingStat to set
	 */
	public void setReceivingStat(List<PbpPlayerStatReceivingPojo> receivingStat) {
		this.receivingStat = receivingStat;
	}

	@Override
	public int hashCode() {
		return Objects.hash(passingStat, receivingStat, rushingStat);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PbpPlayerStatOffensePojo)) {
			return false;
		}
		PbpPlayerStatOffensePojo other = (PbpPlayerStatOffensePojo) obj;
		return Objects.equals(passingStat, other.passingStat) && Objects.equals(receivingStat, other.receivingStat)
				&& Objects.equals(rushingStat, other.rushingStat);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatOffensePojo [rushingStat=" + rushingStat + ", passingStat=" + passingStat
				+ ", receivingStat=" + receivingStat + "]";
	}

}
