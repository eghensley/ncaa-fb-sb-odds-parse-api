package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.BaseStatDefenseProductionPojo;

public class PbpPlayerStatDefenseProductionPojo extends BaseStatDefenseProductionPojo {
	private Double tackleAssist;
	private Integer passBreakUp;
	private Integer quarterbackHurry;
	private Integer tackleYard;
	private Integer safety;
	private Integer kickBlock;

	public PbpPlayerStatDefenseProductionPojo() {
		// Base constructor
	}

	public PbpPlayerStatDefenseProductionPojo(String playerName) {
		this.playerName = playerName;
	}

	public void applyBase(String name) {
		this.playerName = name;
		this.tackleTotal = 0;
		this.tackleSolo = 0.0;
		this.tackleAssist = 0.0;
		this.sack = 0.0;
		this.passBreakUp = 0;
		this.tackleForLoss = 0;
		this.interception = 0;
		this.fumbleForced = 0;
		this.fumbleRecovered = 0;
		this.fumbleYard = 0;
		this.fumbleTouchdown = 0;
		this.interceptionTouchdown = 0;
		this.interceptionYard = 0;
		this.quarterbackHurry = 0;
		this.tackleYard = 0;
		this.kickBlock = 0;
	}

	/**
	 * @return the tackleAssist
	 */
	public Double getTackleAssist() {
		return tackleAssist;
	}

	/**
	 * @param tackleAssist the tackleAssist to set
	 */
	public void setTackleAssist(Double tackleAssist) {
		this.tackleAssist = tackleAssist;
	}

	/**
	 * @return the passBreakUp
	 */
	public Integer getPassBreakUp() {
		return passBreakUp;
	}

	/**
	 * @param passBreakUp the passBreakUp to set
	 */
	public void setPassBreakUp(Integer passBreakUp) {
		this.passBreakUp = passBreakUp;
	}

	public void applyFumbleRecovery(String playerName, Integer returnYards) {
		this.playerName = playerName;
		this.fumbleRecovered = 1;
		this.fumbleYard = returnYards;
	}

	public void applyFumbleRecovery(Integer returnYards) {
		this.fumbleRecovered = 1;
		this.fumbleYard = returnYards;
	}

	public void applyReturnYards(Integer returnYards) {
		if (this.interception == 1) {
			this.interceptionYard = returnYards;
		} else if (this.fumbleRecovered == 1) {
			this.fumbleYard = returnYards;
		} else {
			throw new IllegalArgumentException("No turnover!");
		}
	}

	public void applyReturnTouchdown() {
		if (this.interception == 1) {
			this.interceptionTouchdown = 1;
		} else if (this.fumbleRecovered == 1) {
			this.fumbleTouchdown = 1;
		} else {
			throw new IllegalArgumentException("No turnover!");
		}
	}

	public void applyTackleSolo(String playerName, Integer yards) {
		this.playerName = playerName;
		this.tackleSolo = 1.0;
		this.tackleAssist = 0.0;
		this.tackleTotal = 1;
		this.tackleYard = yards;
		if (Objects.nonNull(yards) && yards < 0) {
			this.tackleForLoss = 1;
		} else {
			this.tackleForLoss = 0;
		}
	}

	public void applyTackle(String playerName, Integer yards) {
		this.playerName = playerName;
		this.tackleSolo = 0.0;
		this.tackleAssist = 1.0;
		this.tackleTotal = 1;
		this.tackleYard = yards;
		if (Objects.nonNull(yards) && yards < 0) {
			this.tackleForLoss = 1;
		} else {
			this.tackleForLoss = 0;
		}
	}

	public void clearTackles() {
		this.tackleAssist = 0.0;
		this.tackleForLoss = 0;
		this.tackleSolo = 0.0;
		this.tackleTotal = 0;
		this.tackleYard = 0;
	}

	/**
	 * @return the quarterbackHurry
	 */
	public Integer getQuarterbackHurry() {
		return quarterbackHurry;
	}

	/**
	 * @param quarterbackHurry the quarterbackHurry to set
	 */
	public void setQuarterbackHurry(Integer quarterbackHurry) {
		this.quarterbackHurry = quarterbackHurry;
	}

	public boolean resolveDefenseScore() {
		return (this.fumbleTouchdown == 1 || this.interceptionTouchdown == 1 || this.safety == 1);
	}

	/**
	 * @return the tackleYard
	 */
	public Integer getTackleYard() {
		return tackleYard;
	}

	/**
	 * @param tackleYard the tackleYard to set
	 */
	public void setTackleYard(Integer tackleYard) {
		this.tackleYard = tackleYard;
	}

	/**
	 * @return the safety
	 */
	public Integer getSafety() {
		return safety;
	}

	/**
	 * @param safety the safety to set
	 */
	public void setSafety(Integer safety) {
		this.safety = safety;
	}

	/**
	 * @return the kickBlock
	 */
	public Integer getKickBlock() {
		return kickBlock;
	}

	/**
	 * @param kickBlock the kickBlock to set
	 */
	public void setKickBlock(Integer kickBlock) {
		this.kickBlock = kickBlock;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result
				+ Objects.hash(kickBlock, passBreakUp, quarterbackHurry, safety, tackleAssist, tackleYard);
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
		if (!(obj instanceof PbpPlayerStatDefenseProductionPojo)) {
			return false;
		}
		PbpPlayerStatDefenseProductionPojo other = (PbpPlayerStatDefenseProductionPojo) obj;
		return Objects.equals(kickBlock, other.kickBlock) && Objects.equals(passBreakUp, other.passBreakUp)
				&& Objects.equals(quarterbackHurry, other.quarterbackHurry) && Objects.equals(safety, other.safety)
				&& Objects.equals(tackleAssist, other.tackleAssist) && Objects.equals(tackleYard, other.tackleYard);
	}

	@Override
	public String toString() {
		return "PbpPlayerStatDefenseProductionPojo [tackleAssist=" + tackleAssist + ", passBreakUp=" + passBreakUp
				+ ", quarterbackHurry=" + quarterbackHurry + ", tackleYard=" + tackleYard + ", safety=" + safety
				+ ", kickBlock=" + kickBlock + "]";
	}

}
