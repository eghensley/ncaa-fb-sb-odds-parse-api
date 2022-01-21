package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DriveResultEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;

@Entity
@Table(name = "DRIVE")
public class DriveData extends OidAuditEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6617364525884584673L;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "GAME_OID", referencedColumnName = "NCAA_GAME_ID", nullable = false)
	private GameData game;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "DRIVE_START_POSSESSION_TEAM_ID", referencedColumnName = "NCAA_TEAM_ID", nullable = false)
	private TeamData driveStartPossessionTeam;
	@Column(name = "F_KICKOFF", nullable = false)
	private Boolean kickoff;
	@Column(name = "DRIVE_START_TIME", nullable = false)
	private Integer driveStartTime;
	@Column(name = "DRIVE_END_TIME", nullable = false)
	private Integer driveEndTime;
	@Column(name = "DRIVE_TOTAL_TIME", nullable = false)
	private Integer driveTotalTime;
	@Column(name = "DRIVE_START_PERIOD", nullable = false)
	private PlayPeriodEnum driveStartPeriod;
	@Column(name = "DRIVE_END_PERIOD", nullable = false)
	private PlayPeriodEnum driveEndPeriod;
	@OneToMany(mappedBy="drive", cascade = CascadeType.ALL)
	private List<PlayData> plays;
	@Column(name = "DRIVE_OFFENSE_PLAY_COUNT", nullable = false)
	private Integer driveOffensePlays;
	@Column(name = "DRIVE_START_YARDLINE", nullable = false)
	private Integer driveStartYardline;
	@Column(name = "DRIVE_END_YARDLINE", nullable = false)
	private Integer driveEndYardline;
	@Column(name = "DRIVE_TOTAL_YARD", nullable = false)
	private Integer driveTotalYard;
	@Column(name = "DRIVE_RESULT_POINT", nullable = false)
	private Integer driveResultPoint;
	@Column(name = "DRIVE_RESULT", nullable = false)
	private DriveResultEnum driveResult;
	@Column(name = "DRIVE_OFFENSE_YARD", nullable = false)
	private Integer driveOffenseYard;
	@Column(name = "DRIVE_START_HOME_SCORE", nullable = false)
	private Integer driveStartHomeScore;
	@Column(name = "DRIVE_START_AWAY_SCORE", nullable = false)
	private Integer driveStartAwayScore;
	@Column(name = "DRIVE_RESULT_HOME_SCORE", nullable = false)
	private Integer driveResultHomeScore;
	@Column(name = "DRIVE_RESULT_AWAY_SCORE", nullable = false)
	private Integer driveResultAwayScore;
	@Column(name = "F_DRIVE_RESULT_SCORING_OPP", nullable = false)
	private boolean driveResultScoringOpp;
	
	public DriveData() {
		this.plays = new ArrayList<>();
	}

	/**
	 * @param game the game to set
	 */
	public void setGame(GameData game) {
		this.game = game;
	}

	/**
	 * @return the driveStartPossessionTeam
	 */
	public TeamData getDriveStartPossessionTeam() {
		return driveStartPossessionTeam;
	}

	/**
	 * @param driveStartPossessionTeam the driveStartPossessionTeam to set
	 */
	public void setDriveStartPossessionTeam(TeamData driveStartPossessionTeam) {
		this.driveStartPossessionTeam = driveStartPossessionTeam;
	}

	/**
	 * @return the kickoff
	 */
	public Boolean getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(Boolean kickoff) {
		this.kickoff = kickoff;
	}

	/**
	 * @return the driveStartTime
	 */
	public Integer getDriveStartTime() {
		return driveStartTime;
	}

	/**
	 * @param driveStartTime the driveStartTime to set
	 */
	public void setDriveStartTime(Integer driveStartTime) {
		this.driveStartTime = driveStartTime;
	}

	/**
	 * @return the driveEndTime
	 */
	public Integer getDriveEndTime() {
		return driveEndTime;
	}

	/**
	 * @param driveEndTime the driveEndTime to set
	 */
	public void setDriveEndTime(Integer driveEndTime) {
		this.driveEndTime = driveEndTime;
	}

	/**
	 * @return the driveTotalTime
	 */
	public Integer getDriveTotalTime() {
		return driveTotalTime;
	}

	/**
	 * @param driveTotalTime the driveTotalTime to set
	 */
	public void setDriveTotalTime(Integer driveTotalTime) {
		this.driveTotalTime = driveTotalTime;
	}

	/**
	 * @return the driveStartPeriod
	 */
	public PlayPeriodEnum getDriveStartPeriod() {
		return driveStartPeriod;
	}

	/**
	 * @param driveStartPeriod the driveStartPeriod to set
	 */
	public void setDriveStartPeriod(PlayPeriodEnum driveStartPeriod) {
		this.driveStartPeriod = driveStartPeriod;
	}

	/**
	 * @return the driveEndPeriod
	 */
	public PlayPeriodEnum getDriveEndPeriod() {
		return driveEndPeriod;
	}

	/**
	 * @param driveEndPeriod the driveEndPeriod to set
	 */
	public void setDriveEndPeriod(PlayPeriodEnum driveEndPeriod) {
		this.driveEndPeriod = driveEndPeriod;
	}

	/**
	 * @return the plays
	 */
	public List<PlayData> getPlays() {
		return plays;
	}

	/**
	 * @param plays the plays to set
	 */
	public void setPlays(List<PlayData> plays) {
		this.plays = plays;
	}
	
	public void addPlay(PlayData play) {
		this.plays.add(play);
		play.setDrive(this);
	}

	/**
	 * @return the driveOffensePlays
	 */
	public Integer getDriveOffensePlays() {
		return driveOffensePlays;
	}

	/**
	 * @param driveOffensePlays the driveOffensePlays to set
	 */
	public void setDriveOffensePlays(Integer driveOffensePlays) {
		this.driveOffensePlays = driveOffensePlays;
	}

	/**
	 * @return the driveStartYardline
	 */
	public Integer getDriveStartYardline() {
		return driveStartYardline;
	}

	/**
	 * @param driveStartYardline the driveStartYardline to set
	 */
	public void setDriveStartYardline(Integer driveStartYardline) {
		this.driveStartYardline = driveStartYardline;
	}

	/**
	 * @return the driveEndYardline
	 */
	public Integer getDriveEndYardline() {
		return driveEndYardline;
	}

	/**
	 * @param driveEndYardline the driveEndYardline to set
	 */
	public void setDriveEndYardline(Integer driveEndYardline) {
		this.driveEndYardline = driveEndYardline;
	}

	/**
	 * @return the driveTotalYard
	 */
	public Integer getDriveTotalYard() {
		return driveTotalYard;
	}

	/**
	 * @param driveTotalYard the driveTotalYard to set
	 */
	public void setDriveTotalYard(Integer driveTotalYard) {
		this.driveTotalYard = driveTotalYard;
	}

	/**
	 * @return the driveResultPoint
	 */
	public Integer getDriveResultPoint() {
		return driveResultPoint;
	}

	/**
	 * @param driveResultPoint the driveResultPoint to set
	 */
	public void setDriveResultPoint(Integer driveResultPoint) {
		this.driveResultPoint = driveResultPoint;
	}

	/**
	 * @return the driveResult
	 */
	public DriveResultEnum getDriveResult() {
		return driveResult;
	}

	/**
	 * @param driveResult the driveResult to set
	 */
	public void setDriveResult(DriveResultEnum driveResult) {
		this.driveResult = driveResult;
	}

	/**
	 * @return the driveOffenseYard
	 */
	public Integer getDriveOffenseYard() {
		return driveOffenseYard;
	}

	/**
	 * @param driveOffenseYard the driveOffenseYard to set
	 */
	public void setDriveOffenseYard(Integer driveOffenseYard) {
		this.driveOffenseYard = driveOffenseYard;
	}

	/**
	 * @return the driveStartHomeScore
	 */
	public Integer getDriveStartHomeScore() {
		return driveStartHomeScore;
	}

	/**
	 * @param driveStartHomeScore the driveStartHomeScore to set
	 */
	public void setDriveStartHomeScore(Integer driveStartHomeScore) {
		this.driveStartHomeScore = driveStartHomeScore;
	}

	/**
	 * @return the driveStartAwayScore
	 */
	public Integer getDriveStartAwayScore() {
		return driveStartAwayScore;
	}

	/**
	 * @param driveStartAwayScore the driveStartAwayScore to set
	 */
	public void setDriveStartAwayScore(Integer driveStartAwayScore) {
		this.driveStartAwayScore = driveStartAwayScore;
	}

	/**
	 * @return the driveResultHomeScore
	 */
	public Integer getDriveResultHomeScore() {
		return driveResultHomeScore;
	}

	/**
	 * @param driveResultHomeScore the driveResultHomeScore to set
	 */
	public void setDriveResultHomeScore(Integer driveResultHomeScore) {
		this.driveResultHomeScore = driveResultHomeScore;
	}

	/**
	 * @return the driveResultAwayScore
	 */
	public Integer getDriveResultAwayScore() {
		return driveResultAwayScore;
	}

	/**
	 * @param driveResultAwayScore the driveResultAwayScore to set
	 */
	public void setDriveResultAwayScore(Integer driveResultAwayScore) {
		this.driveResultAwayScore = driveResultAwayScore;
	}

	/**
	 * @return the driveResultScoringOpp
	 */
	public boolean getDriveResultScoringOpp() {
		return driveResultScoringOpp;
	}

	/**
	 * @param driveResultScoringOpp the driveResultScoringOpp to set
	 */
	public void setDriveResultScoringOpp(boolean driveResultScoringOpp) {
		this.driveResultScoringOpp = driveResultScoringOpp;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(driveEndPeriod, driveEndTime, driveEndYardline, driveOffensePlays,
				driveOffenseYard, driveResult, driveResultAwayScore, driveResultHomeScore, driveResultPoint,
				driveResultScoringOpp, driveStartAwayScore, driveStartHomeScore, driveStartPeriod,
				driveStartPossessionTeam, driveStartTime, driveStartYardline, driveTotalTime, driveTotalYard, game,
				kickoff, plays);
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
		if (!(obj instanceof DriveData)) {
			return false;
		}
		DriveData other = (DriveData) obj;
		return driveEndPeriod == other.driveEndPeriod && Objects.equals(driveEndTime, other.driveEndTime)
				&& Objects.equals(driveEndYardline, other.driveEndYardline)
				&& Objects.equals(driveOffensePlays, other.driveOffensePlays)
				&& Objects.equals(driveOffenseYard, other.driveOffenseYard) && driveResult == other.driveResult
				&& Objects.equals(driveResultAwayScore, other.driveResultAwayScore)
				&& Objects.equals(driveResultHomeScore, other.driveResultHomeScore)
				&& Objects.equals(driveResultPoint, other.driveResultPoint)
				&& driveResultScoringOpp == other.driveResultScoringOpp
				&& Objects.equals(driveStartAwayScore, other.driveStartAwayScore)
				&& Objects.equals(driveStartHomeScore, other.driveStartHomeScore)
				&& driveStartPeriod == other.driveStartPeriod
				&& Objects.equals(driveStartPossessionTeam, other.driveStartPossessionTeam)
				&& Objects.equals(driveStartTime, other.driveStartTime)
				&& Objects.equals(driveStartYardline, other.driveStartYardline)
				&& Objects.equals(driveTotalTime, other.driveTotalTime)
				&& Objects.equals(driveTotalYard, other.driveTotalYard) && Objects.equals(game, other.game)
				&& Objects.equals(kickoff, other.kickoff) && Objects.equals(plays, other.plays);
	}

	@Override
	public String toString() {
		return "DriveData [game=" + game + ", driveStartPossessionTeam=" + driveStartPossessionTeam + ", kickoff="
				+ kickoff + ", driveStartTime=" + driveStartTime + ", driveEndTime=" + driveEndTime
				+ ", driveTotalTime=" + driveTotalTime + ", driveStartPeriod=" + driveStartPeriod + ", driveEndPeriod="
				+ driveEndPeriod + ", plays=" + plays + ", driveOffensePlays=" + driveOffensePlays
				+ ", driveStartYardline=" + driveStartYardline + ", driveEndYardline=" + driveEndYardline
				+ ", driveTotalYard=" + driveTotalYard + ", driveResultPoint=" + driveResultPoint + ", driveResult="
				+ driveResult + ", driveOffenseYard=" + driveOffenseYard + ", driveStartHomeScore="
				+ driveStartHomeScore + ", driveStartAwayScore=" + driveStartAwayScore + ", driveResultHomeScore="
				+ driveResultHomeScore + ", driveResultAwayScore=" + driveResultAwayScore + ", driveResultScoringOpp="
				+ driveResultScoringOpp + "]";
	}
	
	
}
