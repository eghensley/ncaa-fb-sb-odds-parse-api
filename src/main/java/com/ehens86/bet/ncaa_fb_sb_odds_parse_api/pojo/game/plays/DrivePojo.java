package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class DrivePojo {
	private String possessionTeamId;
	private boolean kickoff;
	private Integer driveStartTime;
	@JsonIgnore
	private String driveEndTime;
	@JsonIgnore
	private String driveTotalTime;
	private PlayPeriodEnum driveStartPeriod;
	@JsonIgnore
	private PlayPeriodEnum driveEndPeriod;
	private List<PlayPojo> drivePlays;

	public DrivePojo() {
		this.drivePlays = new ArrayList<PlayPojo>();
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
	public String getDriveEndTime() {
		return driveEndTime;
	}

	/**
	 * @param driveEndTime the driveEndTime to set
	 */
	public void setDriveEndTime(String driveEndTime) {
		this.driveEndTime = driveEndTime;
	}

	/**
	 * @return the driveTotalTime
	 */
	public String getDriveTotalTime() {
		return driveTotalTime;
	}

	/**
	 * @param driveTotalTime the driveTotalTime to set
	 */
	public void setDriveTotalTime(String driveTotalTime) {
		this.driveTotalTime = driveTotalTime;
	}

	/**
	 * @return the drivePlays
	 */
	public List<PlayPojo> getDrivePlays() {
		return drivePlays;
	}

	public Integer requireSplitOnside() {
		List<PlayPojo> onsideAttempts = this.drivePlays.stream().filter(play -> (play.getPlayType() == PlayTypeEnum.KICKOFF && play.getPlayerStat()
				.get(this.possessionTeamId).getSpecialTeam().getKickoff().get(0).getKickoffOnsideAttempt() == 1)).collect(Collectors.toList());;
		if (onsideAttempts.size() > 0) {
			int onsideAttemptIndex = this.drivePlays.indexOf(onsideAttempts.get(0));
			if (onsideAttemptIndex < this.drivePlays.size() - 1) {
				onsideAttempts.get(0).getPlayerStat()
				.get(this.possessionTeamId).getSpecialTeam().getKickoff().get(0).setKickoffOnsideSuccess(1);
				return onsideAttemptIndex;
			}
		}
		return null;
	}

	public boolean requireSplitTailKickoff() {		
		if (this.drivePlays.size() > 1 && this.drivePlays.get(this.drivePlays.size() -1).getPlayType() == PlayTypeEnum.KICKOFF){
			return true;
		} else {
			return false;
		}

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
	public boolean getKickoff() {
		return kickoff;
	}

	/**
	 * @return the kickoff
	 */
	public boolean isKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(boolean kickoff) {
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

}
