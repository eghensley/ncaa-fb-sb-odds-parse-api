package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense;

import java.util.Objects;

public class PlayerStatDefenseProductionPojo {
	private String playerName;
	private Integer tackleTotal;
	private Double tackleSolo;
	private Double tackleAssist;
	private Double sack;
	private Integer passBreakUp;
	private Integer tackleForLoss;
	private Integer interception;
	private Integer fumbleForced;
	private Integer fumbleRecovered;
	private Integer fumbleYard;
	private Integer fumbleTouchdown;
	private Integer interceptionTouchdown;
	private Integer interceptionYard;
	private Integer quarterbackHurry;
	private Integer tackleYard;
	private Integer safety;
	private Integer kickBlock;

	public PlayerStatDefenseProductionPojo() {
//		this.fumbleTouchdown = 0;
//		this.fumbleYard = 0;
//		this.interceptionTouchdown = 0;
//		this.interceptionYard = 0;
	}

	public PlayerStatDefenseProductionPojo(String playerName) {
		this.playerName = playerName;
//		this.fumbleTouchdown = 0;
//		this.fumbleYard = 0;
//		this.interceptionTouchdown = 0;
//		this.interceptionYard = 0;
	}

//	public void applyRushSpecialTeamsBase(String name) {
//		this.playerName = name;
//		this.sack = 0.0;
//		this.passBreakUp = 0;
//		this.interception = 0;
//		this.interceptionTouchdown = 0;
//		this.interceptionYard = 0;
//		this.quarterbackHurry = 0;
//	}

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

//	public void applyPassBase(String name) {
//		this.playerName = name;
//		this.quarterbackHurry = 0;
//	}

	public PlayerStatDefenseProductionPojo(String playerName, Integer tackleTotal, Double tackleSolo,
			Double tackleAssist, Double sack, Integer passBreakUp, Integer tackleForLoss, Integer interception,
			Integer fumbleForced, Integer fumbleRecovered, Integer fumbleYard, Integer fumbleTouchdown,
			Integer interceptionTouchdown, Integer interceptionYard) {
		super();
		this.playerName = playerName;
		this.tackleTotal = tackleTotal;
		this.tackleSolo = tackleSolo;
		this.tackleAssist = tackleAssist;
		this.sack = sack;
		this.passBreakUp = passBreakUp;
		this.tackleForLoss = tackleForLoss;
		this.interception = interception;
		this.fumbleForced = fumbleForced;
		this.fumbleRecovered = fumbleRecovered;
		this.fumbleYard = fumbleYard;
		this.fumbleTouchdown = fumbleTouchdown;
		this.interceptionTouchdown = interceptionTouchdown;
		this.interceptionYard = interceptionYard;
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
	 * @return the playerName
	 */
	public String getPlayerName() {
		return playerName;
	}

	/**
	 * @param playerName the playerName to set
	 */
	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}

	/**
	 * @return the tackleTotal
	 */
	public Integer getTackleTotal() {
		return tackleTotal;
	}

	/**
	 * @param tackleTotal the tackleTotal to set
	 */
	public void setTackleTotal(Integer tackleTotal) {
		this.tackleTotal = tackleTotal;
	}

	/**
	 * @return the tackleSolo
	 */
	public Double getTackleSolo() {
		return tackleSolo;
	}

	/**
	 * @param tackleSolo the tackleSolo to set
	 */
	public void setTackleSolo(Double tackleSolo) {
		this.tackleSolo = tackleSolo;
	}

	/**
	 * @return the sack
	 */
	public Double getSack() {
		return sack;
	}

	/**
	 * @param sack the sack to set
	 */
	public void setSack(Double sack) {
		this.sack = sack;
	}

	/**
	 * @return the tackleForLoss
	 */
	public Integer getTackleForLoss() {
		return tackleForLoss;
	}

	/**
	 * @param tackleForLoss the tackleForLoss to set
	 */
	public void setTackleForLoss(Integer tackleForLoss) {
		this.tackleForLoss = tackleForLoss;
	}

	/**
	 * @return the interception
	 */
	public Integer getInterception() {
		return interception;
	}

	/**
	 * @param interception the interception to set
	 */
	public void setInterception(Integer interception) {
		this.interception = interception;
	}

	/**
	 * @return the fumbleForced
	 */
	public Integer getFumbleForced() {
		return fumbleForced;
	}

	/**
	 * @param fumbleForced the fumbleForced to set
	 */
	public void setFumbleForced(Integer fumbleForced) {
		this.fumbleForced = fumbleForced;
	}

	/**
	 * @return the fumbleRecovered
	 */
	public Integer getFumbleRecovered() {
		return fumbleRecovered;
	}

	/**
	 * @param fumbleRecovered the fumbleRecovered to set
	 */
	public void setFumbleRecovered(Integer fumbleRecovered) {
		this.fumbleRecovered = fumbleRecovered;
	}

	/**
	 * @return the fumbleTouchdown
	 */
	public Integer getFumbleTouchdown() {
		return fumbleTouchdown;
	}

	/**
	 * @param fumbleTouchdown the fumbleTouchdown to set
	 */
	public void setFumbleTouchdown(Integer fumbleTouchdown) {
		this.fumbleTouchdown = fumbleTouchdown;
	}

	/**
	 * @param fumbleTouchdown the fumbleTouchdown to set
	 */
	public void addFumbleTouchdownAndYards(Integer fumbleTouchdown, Integer fumbleYards) {
		this.fumbleTouchdown += fumbleTouchdown;
		this.fumbleYard += fumbleYards;
	}

	/**
	 * @return the interceptionTouchdown
	 */
	public Integer getInterceptionTouchdown() {
		return interceptionTouchdown;
	}

	/**
	 * @param interceptionTouchdown the interceptionTouchdown to set
	 */
	public void setInterceptionTouchdown(Integer interceptionTouchdown) {
		this.interceptionTouchdown = interceptionTouchdown;
	}

	/**
	 * @return the fumbleYard
	 */
	public Integer getFumbleYard() {
		return fumbleYard;
	}

	/**
	 * @param fumbleYard the fumbleYard to set
	 */
	public void setFumbleYard(Integer fumbleYard) {
		this.fumbleYard = fumbleYard;
	}

//	/**
//	 * @param fumbleYard the fumbleYard to set
//	 */
//	public void addFumbleYard(Integer fumbleYard) {
//		this.fumbleYard += fumbleYard;
//	}

	/**
	 * @return the interceptionYard
	 */
	public Integer getInterceptionYard() {
		return interceptionYard;
	}

	/**
	 * @param interceptionYard the interceptionYard to set
	 */
	public void setInterceptionYard(Integer interceptionYard) {
		this.interceptionYard = interceptionYard;
	}

	/**
	 * @param fumbleTouchdown the fumbleTouchdown to set
	 */
	public void addInterceptionTouchdownAndYards(Integer interceptionTouchdown, Integer interceptionYards) {
		this.interceptionTouchdown += interceptionTouchdown;
		this.interceptionYard += interceptionYards;
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

	public void clearTackles() {
		this.tackleAssist = 0.0;
		this.tackleForLoss = 0;
		this.tackleSolo = 0.0;
		this.tackleTotal = 0;
		this.tackleYard = 0;
	}
	
	public void applyTackleSolo(String playerName, boolean forLoss) {
		this.playerName = playerName;
		this.tackleSolo = 1.0;
		this.tackleTotal = 1;
		if (forLoss) {
			this.tackleForLoss = 1;
		} else {
			this.tackleForLoss = 0;
		}
	}
	
	public void applyTackleSolo(String playerName, Integer yards) {
		this.playerName = playerName;
		this.tackleSolo = 1.0;
		this.tackleAssist = 0.0;
		this.tackleTotal = 1;
		this.tackleYard = yards;
		if (Objects.nonNull(yards) && yards <= 0) {
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
		if (Objects.nonNull(yards) && yards <= 0) {
			this.tackleForLoss = 1;
		} else {
			this.tackleForLoss = 0;
		}
	}
	
	public void applyTackleSolo(boolean forLoss) {
		this.tackleSolo = 1.0;
		this.tackleTotal = 1;
		if (forLoss) {
			this.tackleForLoss = 1;
		} else {
			this.tackleForLoss = 0;
		}
	}

	public void applyTackle(String playerName, boolean forLoss) {
		this.playerName = playerName;
		this.tackleSolo = 0.0;
		this.tackleTotal = 1;
		if (forLoss) {
			this.tackleForLoss = 1;
		} else {
			this.tackleForLoss = 0;
		}
	}

	public void applyTackle(boolean forLoss) {
		this.tackleSolo = 0.0;
		this.tackleTotal = 1;
		if (forLoss) {
			this.tackleForLoss = 1;
		} else {
			this.tackleForLoss = 0;
		}
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
		if (this.fumbleTouchdown == 1 || this.interceptionTouchdown == 1 || this.safety == 1) {
			return true;
		} else {
			return false;
		}
	}

	public boolean resolveTurnover() {
		if (this.fumbleRecovered == 1 || this.interception == 1) {
			return true;
		} else {
			return false;
		}
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
	
	public void addForcedFumbleTackle() {
		this.fumbleForced = 1;
		this.tackleTotal += 1;
		this.tackleSolo += 1;
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

	
}
