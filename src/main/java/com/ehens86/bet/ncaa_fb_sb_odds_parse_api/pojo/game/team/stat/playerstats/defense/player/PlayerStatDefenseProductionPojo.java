package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.player;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.BaseStatDefenseProductionPojo;

public class PlayerStatDefenseProductionPojo extends BaseStatDefenseProductionPojo {

	public PlayerStatDefenseProductionPojo() {
		this.fumbleYard = 0;
		this.fumbleTouchdown = 0;
		this.interceptionTouchdown = 0;
		this.interceptionYard = 0;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (!(obj instanceof PlayerStatDefenseProductionPojo)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "PlayerStatDefenseProductionPojo [playerName=" + playerName + ", tackleTotal=" + tackleTotal
				+ ", tackleSolo=" + tackleSolo + ", sack=" + sack + ", tackleForLoss=" + tackleForLoss
				+ ", interception=" + interception + ", fumbleForced=" + fumbleForced + ", fumbleRecovered="
				+ fumbleRecovered + ", fumbleYard=" + fumbleYard + ", fumbleTouchdown=" + fumbleTouchdown
				+ ", interceptionTouchdown=" + interceptionTouchdown + ", interceptionYard=" + interceptionYard
				+ ", hashCode()=" + hashCode() + ", getFumbleTouchdown()=" + getFumbleTouchdown()
				+ ", getInterceptionTouchdown()=" + getInterceptionTouchdown() + ", getFumbleYard()=" + getFumbleYard()
				+ ", getInterceptionYard()=" + getInterceptionYard() + ", getPlayerName()=" + getPlayerName()
				+ ", getTackleTotal()=" + getTackleTotal() + ", getTackleSolo()=" + getTackleSolo() + ", getSack()="
				+ getSack() + ", getTackleForLoss()=" + getTackleForLoss() + ", getInterception()=" + getInterception()
				+ ", getFumbleForced()=" + getFumbleForced() + ", getFumbleRecovered()=" + getFumbleRecovered()
				+ ", resolveTurnover()=" + resolveTurnover() + ", toString()=" + super.toString() + ", getClass()="
				+ getClass() + "]";
	}

}
