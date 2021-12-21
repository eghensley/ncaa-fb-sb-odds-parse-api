package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;

public class PbpPlayerStatDefensePojo {
	private List<PbpPlayerStatDefenseProductionPojo> defenseProduction;

	public PbpPlayerStatDefensePojo() {
		this.defenseProduction = new ArrayList<>();
	}
	
	public PbpPlayerStatDefensePojo(List<PbpPlayerStatDefenseProductionPojo> defenseProduction) {
		super();
		this.defenseProduction = defenseProduction;
	}

	public PbpPlayerStatDefenseProductionPojo findDefenseProductionByName(String playerName) {
		if (this.defenseProduction.stream().filter(name -> playerName.equals(name.getPlayerName()))
				.collect(Collectors.toList()).isEmpty()) {
			PbpPlayerStatDefenseProductionPojo newDef = new PbpPlayerStatDefenseProductionPojo();
			newDef.applyBase(playerName);
			this.defenseProduction.add(newDef);
		}
		return this.defenseProduction.stream().filter(name -> playerName.equals(name.getPlayerName()))
				.collect(Collectors.toList()).get(0);
	}

	public PbpPlayerStatDefenseProductionPojo findDefenseWithFumbleRecovery() {
		return this.defenseProduction.stream().filter(name -> name.getFumbleRecovered() == 1)
				.collect(Collectors.toList()).get(0);
	}

	public PbpPlayerStatDefenseProductionPojo findDefenseWithTurnover() {
		List<PbpPlayerStatDefenseProductionPojo> turnoverDef = this.defenseProduction.stream()
				.filter(PbpPlayerStatDefenseProductionPojo::resolveTurnover).collect(Collectors.toList());
		if (turnoverDef.isEmpty()) {
			return null;
		} else {
			return turnoverDef.get(0);
		}
	}

	/**
	 * @return the defenseProduction
	 */
	public List<PbpPlayerStatDefenseProductionPojo> getDefenseProduction() {
		return defenseProduction;
	}

	/**
	 * @param defenseProduction the defenseProduction to set
	 */
	public void setDefenseProduction(List<PbpPlayerStatDefenseProductionPojo> defenseProduction) {
		this.defenseProduction = defenseProduction;
	}

	@Override
	public int hashCode() {
		return Objects.hash(defenseProduction);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PbpPlayerStatDefensePojo)) {
			return false;
		}
		PbpPlayerStatDefensePojo other = (PbpPlayerStatDefensePojo) obj;
		return Objects.equals(defenseProduction, other.defenseProduction);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatDefensePojo [defenseProduction=" + defenseProduction + "]";
	}
	
	

}
