package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.defense;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.PlayerStatEntity;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.StatDefSpecTeam;

@Entity
@Table(name = "STAT_DEFENSE")
public class StatDefenseData extends PlayerStatEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 9089772651652870618L;
	@ManyToOne
	@JoinColumn(name = "PLAY_STAT_OID", referencedColumnName = "OID", nullable = false)
	private TeamPlayStatData playStat;
	@Column(name = "DEF_TACKLE_TOTAL", nullable = false)
	private Integer tackleTotal;
	@Column(name = "DEF_TACKLE_SOLO", nullable = false)
	private Integer tackleSolo;
	@Column(name = "DEF_SACK", nullable = false)
	private Integer sack;
	@Column(name = "DEF_TACKLE_LOSS", nullable = false)
	private Integer tackleForLoss;
	@Column(name = "DEF_INTERCEPTION", nullable = false)
	private Integer interception;
	@Column(name = "DEF_FUMBLE_FORCED", nullable = false)
	private Integer fumbleForced;
	@Column(name = "DEF_FUMBLE_RECOVERED", nullable = false)
	private Integer fumbleRecovered;
	@Column(name = "DEF_FUMBLE_YARD", nullable = false)
	private Integer fumbleYard;
	@Column(name = "DEF_FUMBLE_TOUCHDOWN", nullable = false)
	private Integer fumbleTouchdown;
	@Column(name = "DEF_INTERCEPTION_TOUCHDOWN", nullable = false)
	private Integer interceptionTouchdown;
	@Column(name = "DEF_INTERCEPTION_YARD", nullable = false)
	private Integer interceptionYard;
	@Column(name = "DEF_TACKLE_ASSIST", nullable = false)
	private Integer tackleAssist;
	@Column(name = "DEF_PASS_BREAKUP", nullable = false)
	private Integer passBreakUp;
	@Column(name = "DEF_QB_HURRY", nullable = false)
	private Integer quarterbackHurry;
	@Column(name = "DEF_TACKLE_YARD", nullable = false)
	private Integer tackleYard;
	@Column(name = "DEF_SAFETY", nullable = false)
	private Integer safety;
	@Column(name = "DEF_KICK_BLOCK", nullable = false)
	private Integer kickBlock;
	@Column(name = "DEF_TYPE", nullable = false)
	private StatDefSpecTeam defenseType;

	public StatDefenseData() {

	}

//	/**
//	 * @return the playStat
//	 */
//	public TeamPlayStatData getPlayStat() {
//		return playStat;
//	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(TeamPlayStatData playStat) {
		this.playStat = playStat;
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
	public Integer getTackleSolo() {
		return tackleSolo;
	}

	/**
	 * @param tackleSolo the tackleSolo to set
	 */
	public void setTackleSolo(Integer tackleSolo) {
		this.tackleSolo = tackleSolo;
	}

	/**
	 * @return the sack
	 */
	public Integer getSack() {
		return sack;
	}

	/**
	 * @param sack the sack to set
	 */
	public void setSack(Integer sack) {
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
	 * @return the tackleAssist
	 */
	public Integer getTackleAssist() {
		return tackleAssist;
	}

	/**
	 * @param tackleAssist the tackleAssist to set
	 */
	public void setTackleAssist(Integer tackleAssist) {
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

	/**
	 * @return the defenseType
	 */
	public StatDefSpecTeam getDefenseType() {
		return defenseType;
	}

	/**
	 * @param defenseType the defenseType to set
	 */
	public void setDefenseType(StatDefSpecTeam defenseType) {
		this.defenseType = defenseType;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(defenseType, fumbleForced, fumbleRecovered, fumbleTouchdown, fumbleYard,
				interception, interceptionTouchdown, interceptionYard, kickBlock, passBreakUp, playStat,
				quarterbackHurry, sack, safety, tackleAssist, tackleForLoss, tackleSolo, tackleTotal, tackleYard);
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
		if (!(obj instanceof StatDefenseData)) {
			return false;
		}
		StatDefenseData other = (StatDefenseData) obj;
		return defenseType == other.defenseType && Objects.equals(fumbleForced, other.fumbleForced)
				&& Objects.equals(fumbleRecovered, other.fumbleRecovered)
				&& Objects.equals(fumbleTouchdown, other.fumbleTouchdown)
				&& Objects.equals(fumbleYard, other.fumbleYard) && Objects.equals(interception, other.interception)
				&& Objects.equals(interceptionTouchdown, other.interceptionTouchdown)
				&& Objects.equals(interceptionYard, other.interceptionYard)
				&& Objects.equals(kickBlock, other.kickBlock) && Objects.equals(passBreakUp, other.passBreakUp)
				&& Objects.equals(playStat, other.playStat) && Objects.equals(quarterbackHurry, other.quarterbackHurry)
				&& Objects.equals(sack, other.sack) && Objects.equals(safety, other.safety)
				&& Objects.equals(tackleAssist, other.tackleAssist)
				&& Objects.equals(tackleForLoss, other.tackleForLoss) && Objects.equals(tackleSolo, other.tackleSolo)
				&& Objects.equals(tackleTotal, other.tackleTotal) && Objects.equals(tackleYard, other.tackleYard);
	}

	@Override
	public String toString() {
		return "StatDefenseData [playStat=" + playStat + ", tackleTotal=" + tackleTotal + ", tackleSolo=" + tackleSolo
				+ ", sack=" + sack + ", tackleForLoss=" + tackleForLoss + ", interception=" + interception
				+ ", fumbleForced=" + fumbleForced + ", fumbleRecovered=" + fumbleRecovered + ", fumbleYard="
				+ fumbleYard + ", fumbleTouchdown=" + fumbleTouchdown + ", interceptionTouchdown="
				+ interceptionTouchdown + ", interceptionYard=" + interceptionYard + ", tackleAssist=" + tackleAssist
				+ ", passBreakUp=" + passBreakUp + ", quarterbackHurry=" + quarterbackHurry + ", tackleYard="
				+ tackleYard + ", safety=" + safety + ", kickBlock=" + kickBlock + ", defenseType=" + defenseType + "]";
	}

}
