package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerStats.defense;

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

	public PlayerStatDefenseProductionPojo() {
		this.fumbleTouchdown = 0;
		this.fumbleYard = 0;
		this.interceptionTouchdown = 0;
		this.interceptionYard = 0;
	}

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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((fumbleForced == null) ? 0 : fumbleForced.hashCode());
		result = prime * result + ((fumbleRecovered == null) ? 0 : fumbleRecovered.hashCode());
		result = prime * result + ((fumbleTouchdown == null) ? 0 : fumbleTouchdown.hashCode());
		result = prime * result + ((fumbleYard == null) ? 0 : fumbleYard.hashCode());
		result = prime * result + ((interception == null) ? 0 : interception.hashCode());
		result = prime * result + ((interceptionTouchdown == null) ? 0 : interceptionTouchdown.hashCode());
		result = prime * result + ((interceptionYard == null) ? 0 : interceptionYard.hashCode());
		result = prime * result + ((passBreakUp == null) ? 0 : passBreakUp.hashCode());
		result = prime * result + ((playerName == null) ? 0 : playerName.hashCode());
		result = prime * result + ((sack == null) ? 0 : sack.hashCode());
		result = prime * result + ((tackleAssist == null) ? 0 : tackleAssist.hashCode());
		result = prime * result + ((tackleForLoss == null) ? 0 : tackleForLoss.hashCode());
		result = prime * result + ((tackleSolo == null) ? 0 : tackleSolo.hashCode());
		result = prime * result + ((tackleTotal == null) ? 0 : tackleTotal.hashCode());
		return result;
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
		if (fumbleForced == null) {
			if (other.fumbleForced != null) {
				return false;
			}
		} else if (!fumbleForced.equals(other.fumbleForced)) {
			return false;
		}
		if (fumbleRecovered == null) {
			if (other.fumbleRecovered != null) {
				return false;
			}
		} else if (!fumbleRecovered.equals(other.fumbleRecovered)) {
			return false;
		}
		if (fumbleTouchdown == null) {
			if (other.fumbleTouchdown != null) {
				return false;
			}
		} else if (!fumbleTouchdown.equals(other.fumbleTouchdown)) {
			return false;
		}
		if (fumbleYard == null) {
			if (other.fumbleYard != null) {
				return false;
			}
		} else if (!fumbleYard.equals(other.fumbleYard)) {
			return false;
		}
		if (interception == null) {
			if (other.interception != null) {
				return false;
			}
		} else if (!interception.equals(other.interception)) {
			return false;
		}
		if (interceptionTouchdown == null) {
			if (other.interceptionTouchdown != null) {
				return false;
			}
		} else if (!interceptionTouchdown.equals(other.interceptionTouchdown)) {
			return false;
		}
		if (interceptionYard == null) {
			if (other.interceptionYard != null) {
				return false;
			}
		} else if (!interceptionYard.equals(other.interceptionYard)) {
			return false;
		}
		if (passBreakUp == null) {
			if (other.passBreakUp != null) {
				return false;
			}
		} else if (!passBreakUp.equals(other.passBreakUp)) {
			return false;
		}
		if (playerName == null) {
			if (other.playerName != null) {
				return false;
			}
		} else if (!playerName.equals(other.playerName)) {
			return false;
		}
		if (sack == null) {
			if (other.sack != null) {
				return false;
			}
		} else if (!sack.equals(other.sack)) {
			return false;
		}
		if (tackleAssist == null) {
			if (other.tackleAssist != null) {
				return false;
			}
		} else if (!tackleAssist.equals(other.tackleAssist)) {
			return false;
		}
		if (tackleForLoss == null) {
			if (other.tackleForLoss != null) {
				return false;
			}
		} else if (!tackleForLoss.equals(other.tackleForLoss)) {
			return false;
		}
		if (tackleSolo == null) {
			if (other.tackleSolo != null) {
				return false;
			}
		} else if (!tackleSolo.equals(other.tackleSolo)) {
			return false;
		}
		if (tackleTotal == null) {
			if (other.tackleTotal != null) {
				return false;
			}
		} else if (!tackleTotal.equals(other.tackleTotal)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatDefenseProductionPojo [playerName=" + playerName + ", tackleTotal=" + tackleTotal
				+ ", tackleSolo=" + tackleSolo + ", tackleAssist=" + tackleAssist + ", sack=" + sack + ", passBreakUp="
				+ passBreakUp + ", tackleForLoss=" + tackleForLoss + ", interception=" + interception
				+ ", fumbleForced=" + fumbleForced + ", fumbleRecovered=" + fumbleRecovered + ", fumbleYard="
				+ fumbleYard + ", fumbleTouchdown=" + fumbleTouchdown + ", interceptionTouchdown="
				+ interceptionTouchdown + ", interceptionYard=" + interceptionYard + "]";
	}

}
