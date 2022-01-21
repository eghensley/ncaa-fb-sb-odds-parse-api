package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DriveResultEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;

public class DrivePojo {
	private String possessionTeamId;
	private Boolean kickoff;
	private Integer driveStartTime;
	private Integer driveEndTime;
	private Integer driveTotalTime;
	private PlayPeriodEnum driveStartPeriod;
	private PlayPeriodEnum driveEndPeriod;
	private List<PlayPojo> drivePlays;
	private Integer driveOffensePlays;
	private Integer driveStartYardline;
	private Integer driveEndYardline;
	private Integer driveTotalYard;
	private Integer driveResultPoint;
	private DriveResultEnum driveResult;
	private Integer driveOffenseYard;
	private Integer driveStartHomeScore;
	private Integer driveStartAwayScore;
	private Integer driveResultHomeScore;
	private Integer driveResultAwayScore;
	private Boolean driveResultScoringOpp;
	
	public DrivePojo() {
		this.drivePlays = new ArrayList<>();
	}

	public DrivePojo(String possessionTeamId, PlayPeriodEnum driveStartPeriod, Integer driveStartTime,
			Integer driveEndTime) {
		this.drivePlays = new ArrayList<>();
		this.possessionTeamId = possessionTeamId;
		this.driveStartPeriod = driveStartPeriod;
		this.driveStartTime = driveStartTime;
		this.driveEndTime = driveEndTime;
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
	 * @return the drivePlays
	 */
	public List<PlayPojo> getDrivePlays() {
		return drivePlays;
	}

	public Integer requireSplitOnside() {
		List<PlayPojo> onsideAttempts = this.drivePlays.stream().filter(
				play -> (play.getPlayType() == PlayTypeEnum.KICKOFF && play.getPlayerStat().get(this.possessionTeamId)
						.getSpecialTeam().getKickoff().get(0).getKickoffOnsideAttempt() == 1))
				.collect(Collectors.toList());
		if (!onsideAttempts.isEmpty()) {
			int onsideAttemptIndex = this.drivePlays.indexOf(onsideAttempts.get(0));
			if (onsideAttemptIndex < this.drivePlays.size() - 1) {
				onsideAttempts.get(0).getPlayerStat().get(this.possessionTeamId).getSpecialTeam().getKickoff().get(0)
						.setKickoffOnsideSuccess(1);
				return onsideAttemptIndex;
			}
		}
		return null;
	}

	/**
	 * @param drivePlays the drivePlays to set
	 */
	public void setDrivePlays(List<PlayPojo> drivePlays) {
		this.drivePlays = drivePlays;
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
	 * @return the possessionTeamId
	 */
	public String getPossessionTeamId() {
		return possessionTeamId;
	}

	/**
	 * @param possessionTeamId the possessionTeamId to set
	 */
	public void setPossessionTeamId(String possessionTeamId) {
		this.possessionTeamId = possessionTeamId;
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

	public void addDriveResultPoint(Integer driveResultPoint) {
		this.driveResultPoint += driveResultPoint;
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

	public void addDriveOffenseYard(Integer driveOffenseYard) {
		this.driveOffenseYard += driveOffenseYard;
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
	public Boolean getDriveResultScoringOpp() {
		return driveResultScoringOpp;
	}

	/**
	 * @param driveResultScoringOpp the driveResultScoringOpp to set
	 */
	public void setDriveResultScoringOpp(Boolean driveResultScoringOpp) {
		this.driveResultScoringOpp = driveResultScoringOpp;
	}

	@Override
	public int hashCode() {
		return Objects.hash(driveEndPeriod, driveEndTime, driveEndYardline, driveOffensePlays, driveOffenseYard,
				drivePlays, driveResult, driveResultAwayScore, driveResultHomeScore, driveResultPoint,
				driveResultScoringOpp, driveStartAwayScore, driveStartHomeScore, driveStartPeriod, driveStartTime,
				driveStartYardline, driveTotalTime, driveTotalYard, kickoff, possessionTeamId);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof DrivePojo)) {
			return false;
		}
		DrivePojo other = (DrivePojo) obj;
		return driveEndPeriod == other.driveEndPeriod && Objects.equals(driveEndTime, other.driveEndTime)
				&& Objects.equals(driveEndYardline, other.driveEndYardline)
				&& Objects.equals(driveOffensePlays, other.driveOffensePlays)
				&& Objects.equals(driveOffenseYard, other.driveOffenseYard)
				&& Objects.equals(drivePlays, other.drivePlays) && driveResult == other.driveResult
				&& Objects.equals(driveResultAwayScore, other.driveResultAwayScore)
				&& Objects.equals(driveResultHomeScore, other.driveResultHomeScore)
				&& Objects.equals(driveResultPoint, other.driveResultPoint)
				&& driveResultScoringOpp == other.driveResultScoringOpp
				&& Objects.equals(driveStartAwayScore, other.driveStartAwayScore)
				&& Objects.equals(driveStartHomeScore, other.driveStartHomeScore)
				&& driveStartPeriod == other.driveStartPeriod && Objects.equals(driveStartTime, other.driveStartTime)
				&& Objects.equals(driveStartYardline, other.driveStartYardline)
				&& Objects.equals(driveTotalTime, other.driveTotalTime)
				&& Objects.equals(driveTotalYard, other.driveTotalYard) && Objects.equals(kickoff, other.kickoff)
				&& Objects.equals(possessionTeamId, other.possessionTeamId);
	}

	@Override
	public String toString() {
		return "DrivePojo [possessionTeamId=" + possessionTeamId + ", kickoff=" + kickoff + ", driveStartTime="
				+ driveStartTime + ", driveEndTime=" + driveEndTime + ", driveTotalTime=" + driveTotalTime
				+ ", driveStartPeriod=" + driveStartPeriod + ", driveEndPeriod=" + driveEndPeriod + ", drivePlays="
				+ drivePlays + ", driveOffensePlays=" + driveOffensePlays + ", driveStartYardline=" + driveStartYardline
				+ ", driveEndYardline=" + driveEndYardline + ", driveTotalYard=" + driveTotalYard
				+ ", driveResultPoint=" + driveResultPoint + ", driveResult=" + driveResult + ", driveOffenseYard="
				+ driveOffenseYard + ", driveStartHomeScore=" + driveStartHomeScore + ", driveStartAwayScore="
				+ driveStartAwayScore + ", driveResultHomeScore=" + driveResultHomeScore + ", driveResultAwayScore="
				+ driveResultAwayScore + ", driveResultScoringOpp=" + driveResultScoringOpp + "]";
	}	


}
