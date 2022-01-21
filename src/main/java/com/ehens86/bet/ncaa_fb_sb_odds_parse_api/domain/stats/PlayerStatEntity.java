package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayerData;

@MappedSuperclass
public class PlayerStatEntity extends BaseStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -7544030946080685513L;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "PLAYER_OID", referencedColumnName = "OID", nullable = false)
	protected PlayerData player;

	public PlayerStatEntity() {
		// Base constructor
	}

	/**
	 * @return the player
	 */
	public PlayerData getPlayer() {
		return player;
	}

	/**
	 * @param player the player to set
	 */
	public void setPlayer(PlayerData player) {
		this.player = player;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(player, posNeg, statType);
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
		if (!(obj instanceof PlayerStatEntity)) {
			return false;
		}
		PlayerStatEntity other = (PlayerStatEntity) obj;
		return Objects.equals(player, other.player) && posNeg == other.posNeg && statType == other.statType;
	}

	@Override
	public String toString() {
		return "BaseStatEntity [posNeg=" + posNeg + ", statType=" + statType + ", player=" + player + "]";
	}
	
	
}
