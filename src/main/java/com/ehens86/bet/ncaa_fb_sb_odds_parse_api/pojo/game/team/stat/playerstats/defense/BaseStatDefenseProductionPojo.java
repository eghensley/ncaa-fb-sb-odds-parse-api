package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense;

import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.player.PlayerStatDefenseProductionPojo;

public class BaseStatDefenseProductionPojo {
	protected String playerName;
	protected Integer tackleTotal;
	protected Double tackleSolo;
	protected Double sack;
	protected Integer tackleForLoss;
	protected Integer interception;
	protected Integer fumbleForced;
	protected Integer fumbleRecovered;
	protected Integer fumbleYard;
	protected Integer fumbleTouchdown;
	protected Integer interceptionTouchdown;
	protected Integer interceptionYard;
	
	
	public BaseStatDefenseProductionPojo() {
		// Constructor
	}



	public BaseStatDefenseProductionPojo(String playerName, Integer tackleTotal, Double tackleSolo, Double sack,
			Integer tackleForLoss, Integer interception, Integer fumbleForced, Integer fumbleRecovered,
			Integer fumbleYard, Integer fumbleTouchdown) {
		super();
		this.playerName = playerName;
		this.tackleTotal = tackleTotal;
		this.tackleSolo = tackleSolo;
		this.sack = sack;
		this.tackleForLoss = tackleForLoss;
		this.interception = interception;
		this.fumbleForced = fumbleForced;
		this.fumbleRecovered = fumbleRecovered;
		this.fumbleYard = fumbleYard;
		this.fumbleTouchdown = fumbleTouchdown;
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
	public void addFumbleTouchdownAndYards(Integer fumbleTouchdown, Integer fumbleYards) {
		this.fumbleTouchdown += fumbleTouchdown;
		this.fumbleYard += fumbleYards;
	}
	
	/**
	 * @param fumbleTouchdown the fumbleTouchdown to set
	 */
	public void addInterceptionTouchdownAndYards(Integer interceptionTouchdown, Integer interceptionYards) {
		this.interceptionTouchdown += interceptionTouchdown;
		this.interceptionYard += interceptionYards;
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

	public boolean resolveTurnover() {
		return (this.fumbleRecovered == 1 || this.interception == 1);
	}

	public void addForcedFumbleTackle() {
		this.fumbleForced = 1;
		this.tackleTotal += 1;
		this.tackleSolo += 1;
	}



	@Override
	public int hashCode() {
		return Objects.hash(fumbleForced, fumbleRecovered, fumbleTouchdown, fumbleYard, interception,
				interceptionTouchdown, interceptionYard, playerName, sack, tackleForLoss, tackleSolo, tackleTotal);
	}



	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayerStatDefenseProductionPojo)) {
			return false;
		}
		PlayerStatDefenseProductionPojo other = (PlayerStatDefenseProductionPojo) obj;
		return Objects.equals(fumbleForced, other.fumbleForced)
				&& Objects.equals(fumbleRecovered, other.fumbleRecovered)
				&& Objects.equals(fumbleTouchdown, other.fumbleTouchdown)
				&& Objects.equals(fumbleYard, other.fumbleYard) && Objects.equals(interception, other.interception)
				&& Objects.equals(interceptionTouchdown, other.interceptionTouchdown)
				&& Objects.equals(interceptionYard, other.interceptionYard)
				&& Objects.equals(playerName, other.playerName) && Objects.equals(sack, other.sack)
				&& Objects.equals(tackleForLoss, other.tackleForLoss) && Objects.equals(tackleSolo, other.tackleSolo)
				&& Objects.equals(tackleTotal, other.tackleTotal);
	}



	@Override
	public String toString() {
		return "PlayerStatDefenseProductionPojo [playerName=" + playerName + ", tackleTotal=" + tackleTotal
				+ ", tackleSolo=" + tackleSolo + ", sack=" + sack + ", tackleForLoss=" + tackleForLoss
				+ ", interception=" + interception + ", fumbleForced=" + fumbleForced + ", fumbleRecovered="
				+ fumbleRecovered + ", fumbleYard=" + fumbleYard + ", fumbleTouchdown=" + fumbleTouchdown
				+ ", interceptionTouchdown=" + interceptionTouchdown + ", interceptionYard=" + interceptionYard + "]";
	}

}
