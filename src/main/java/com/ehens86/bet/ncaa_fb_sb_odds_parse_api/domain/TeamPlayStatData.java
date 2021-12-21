package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.defense.StatDefenseData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatPassingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatRushingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.penalty.StatPenaltyData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickoffData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickoffReturnData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatPuntData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatPuntReturnData;

@Entity
@Table(name = "TEAM_PLAY_DATA")
public class TeamPlayStatData extends OidAuditEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -4989489305352649838L;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "TEAM_ID", referencedColumnName = "NCAA_TEAM_ID", nullable = false)
	private TeamData team;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "PLAY_OID", referencedColumnName = "OID", nullable = false)
	private PlayData play;

	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "PASSING_OID", referencedColumnName = "OID", nullable = true)
	private StatPassingData passStat;
	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "RUSHING_OID", referencedColumnName = "OID", nullable = true)
	private StatRushingData rushStat;
	@OneToMany(mappedBy = "playStat",  cascade = CascadeType.ALL)
	private List<StatDefenseData> defenseStat;

	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "KICK_OID", referencedColumnName = "OID", nullable = true)
	private StatKickData kickStat;
	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "KICKOFF_OID", referencedColumnName = "OID", nullable = true)
	private StatKickoffData kickoffStat;
	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "KICKOFF_RETURN_OID", referencedColumnName = "OID", nullable = true)
	private StatKickoffReturnData kickoffReturnStat;

	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "PUNT_OID", referencedColumnName = "OID", nullable = true)
	private StatPuntData puntStat;
	@OneToOne(fetch = FetchType.EAGER,  cascade = CascadeType.ALL)
	@JoinColumn(name = "PUNT_RETURN_OID", referencedColumnName = "OID", nullable = true)
	private StatPuntReturnData puntReturnStat;

	@OneToMany(mappedBy = "playStat",  cascade = CascadeType.ALL)
	private List<StatPenaltyData> penaltyStat;
	
	public TeamPlayStatData() {
		this.penaltyStat = new ArrayList<>();
		this.defenseStat = new ArrayList<>();
	}

	/**
	 * @return the team
	 */
	public TeamData getTeam() {
		return team;
	}

	/**
	 * @param team the team to set
	 */
	public void setTeam(TeamData team) {
		this.team = team;
	}

//	/**
//	 * @return the play
//	 */
//	public PlayData getPlay() {
//		return play;
//	}

	/**
	 * @param play the play to set
	 */
	public void setPlay(PlayData play) {
		this.play = play;
	}

	/**
	 * @return the passStat
	 */
	public StatPassingData getPassStat() {
		return passStat;
	}

	/**
	 * @param passStat the passStat to set
	 */
	public void setPassStat(StatPassingData passStat) {
		this.passStat = passStat;
		passStat.setPlayStat(this);
	}

	/**
	 * @return the rushStat
	 */
	public StatRushingData getRushStat() {
		return rushStat;
	}

	/**
	 * @param rushStat the rushStat to set
	 */
	public void setRushStat(StatRushingData rushStat) {
		this.rushStat = rushStat;
		rushStat.setPlayStat(this);
	}

	/**
	 * @return the defenseStat
	 */
	public List<StatDefenseData> getDefenseStat() {
		return defenseStat;
	}

	/**
	 * @param defenseStat the defenseStat to set
	 */
	public void setDefenseStat(List<StatDefenseData> defenseStat) {
		this.defenseStat = defenseStat;
	}
	
	public void addDefenseStat(StatDefenseData defenseStat) {
		this.defenseStat.add(defenseStat);
		defenseStat.setPlayStat(this);
	}

	/**
	 * @return the kickStat
	 */
	public StatKickData getKickStat() {
		return kickStat;
	}

	/**
	 * @param kickStat the kickStat to set
	 */
	public void setKickStat(StatKickData kickStat) {
		this.kickStat = kickStat;
		kickStat.setPlayStat(this);

	}

	/**
	 * @return the kickoffStat
	 */
	public StatKickoffData getKickoffStat() {
		return kickoffStat;
	}

	/**
	 * @param kickoffStat the kickoffStat to set
	 */
	public void setKickoffStat(StatKickoffData kickoffStat) {
		this.kickoffStat = kickoffStat;
		kickoffStat.setPlayStat(this);
	}

	/**
	 * @return the kickoffReturnStat
	 */
	public StatKickoffReturnData getKickoffReturnStat() {
		return kickoffReturnStat;
	}

	/**
	 * @param kickoffReturnStat the kickoffReturnStat to set
	 */
	public void setKickoffReturnStat(StatKickoffReturnData kickoffReturnStat) {
		this.kickoffReturnStat = kickoffReturnStat;
		kickoffReturnStat.setPlayStat(this);

	}

	/**
	 * @return the puntStat
	 */
	public StatPuntData getPuntStat() {
		return puntStat;
	}

	/**
	 * @param puntStat the puntStat to set
	 */
	public void setPuntStat(StatPuntData puntStat) {
		this.puntStat = puntStat;
		puntStat.setPlayStat(this);

	}

	/**
	 * @return the puntReturnStat
	 */
	public StatPuntReturnData getPuntReturnStat() {
		return puntReturnStat;
	}

	/**
	 * @param puntReturnStat the puntReturnStat to set
	 */
	public void setPuntReturnStat(StatPuntReturnData puntReturnStat) {
		this.puntReturnStat = puntReturnStat;
		puntReturnStat.setPlayStat(this);

	}

	/**
	 * @return the penaltyStat
	 */
	public List<StatPenaltyData> getPenaltyStat() {
		return penaltyStat;
	}

	/**
	 * @param penaltyStat the penaltyStat to set
	 */
	public void setPenaltyStat(List<StatPenaltyData> penaltyStat) {
		this.penaltyStat = penaltyStat;
	}
	
	public void addPenaltyStat(StatPenaltyData penaltyStat) {
		this.penaltyStat.add(penaltyStat);
		penaltyStat.setPlayStat(this);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(defenseStat, kickStat, kickoffReturnStat, kickoffStat, passStat,
				penaltyStat, play, puntReturnStat, puntStat, rushStat, team);
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
		if (!(obj instanceof TeamPlayStatData)) {
			return false;
		}
		TeamPlayStatData other = (TeamPlayStatData) obj;
		return Objects.equals(defenseStat, other.defenseStat) && Objects.equals(kickStat, other.kickStat)
				&& Objects.equals(kickoffReturnStat, other.kickoffReturnStat)
				&& Objects.equals(kickoffStat, other.kickoffStat) && Objects.equals(passStat, other.passStat)
				&& Objects.equals(penaltyStat, other.penaltyStat) && Objects.equals(play, other.play)
				&& Objects.equals(puntReturnStat, other.puntReturnStat) && Objects.equals(puntStat, other.puntStat)
				&& Objects.equals(rushStat, other.rushStat) && Objects.equals(team, other.team);
	}

	@Override
	public String toString() {
		return "TeamPlayStatData [team=" + team + ", play=" + play + ", passStat=" + passStat + ", rushStat=" + rushStat
				+ ", defenseStat=" + defenseStat + ", kickStat=" + kickStat + ", kickoffStat=" + kickoffStat
				+ ", kickoffReturnStat=" + kickoffReturnStat + ", puntStat=" + puntStat + ", puntReturnStat="
				+ puntReturnStat + ", penaltyStat=" + penaltyStat + "]";
	}

}
