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

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.FieldZoneEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.TempoTypeEnum;

@Entity
@Table(name = "PLAY")
public class PlayData extends OidAuditEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -4617264146856084743L;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "DRIVE_OID", referencedColumnName = "OID", nullable = false)
	private DriveData drive;
	@Column(name = "PLAY_TEXT", nullable = false, length = 510)
	private String playText;
	@Column(name = "PLAY_DOWN_DISTANCE_TEXT", nullable = false)
	private String driveText;
	@Column(name = "PLAY_PERIOD", nullable = false)
	private PlayPeriodEnum period;
	@Column(name = "PLAY_TYPE", nullable = false)
	private PlayTypeEnum playType;
	@Column(name = "PLAY_START_DOWN", nullable = false)
	private PlayDownEnum playStartDown;
	@Column(name = "PLAY_START_YARD", nullable = false)
	private Integer playStartYard;
	@Column(name = "PLAY_YARD_TO_GAIN", nullable = true)
	private Integer playYardToGain;
	@Column(name = "PLAY_CALL_TYPE", nullable = false)
	private PlayCallTypeEnum playCallType;
	@Column(name = "F_NO_PLAY_PENALTY", nullable = false)
	private Boolean noPlayPenalty;
	@Column(name = "PLAY_TEMPO", nullable = false)
	private TempoTypeEnum playTempo;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "PLAY_START_POSSESSION_TEAM_ID", referencedColumnName = "NCAA_TEAM_ID", nullable = false)
	private TeamData playStartPossessionTeam;
	@Column(name = "PLAY_RESULT_YARD", nullable = true)
	private Integer playResultYard;
	@Column(name = "PLAY_RESULT_POINT", nullable = false)
	private Integer playResultPoints;
	@Column(name = "PLAY_RESULT_FIRST_DOWN", nullable = false)
	private Boolean playResultFirstDown;
	@Column(name = "PLAY_RESULT_TURNOVER", nullable = false)
	private Boolean playResultTurnover;
	@Column(name = "PLAY_RESULT_YARDLINE", nullable = true)
	private Integer playResultYardLine;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "PLAY_RESULT_POSSESSION_TEAM_ID", referencedColumnName = "NCAA_TEAM_ID", nullable = false)
	private TeamData playResultPossessionTeam;
	@OneToMany(mappedBy = "play", cascade = CascadeType.ALL)
	private List<TeamPlayStatData> playStat;
	@Column(name = "PLAY_START_HOME_SCORE", nullable = false)
	private Integer playStartHomeScore;
	@Column(name = "PLAY_START_AWAY_SCORE", nullable = false)
	private Integer playStartAwayScore;
	@Column(name = "PLAY_RESULT_HOME_SCORE", nullable = false)
	private Integer playResultHomeScore;
	@Column(name = "PLAY_RESULT_AWAY_SCORE", nullable = false)
	private Integer playResultAwayScore;
	@Column(name = "F_GARBAGE_TIME", nullable = false)
	private boolean garbageTime;
	@Column(name = "F_PASSING_DOWN", nullable = false)
	private boolean passingDown;
	@Column(name = "F_PLAY_RESULT_SUCCESS", nullable = false)
	private boolean playResultSuccess;
	@Column(name = "F_PLAY_FIELD_ZONE", nullable = true)
	private FieldZoneEnum playFieldZone;
	
	public PlayData() {
		this.playStat = new ArrayList<>();
	}

	/**
	 * @return the playStat
	 */
	public List<TeamPlayStatData> getPlayStat() {
		return playStat;
	}

	/**
	 * @param playStat the playStat to set
	 */
	public void setPlayStat(List<TeamPlayStatData> playStat) {
		this.playStat = playStat;
	}
	
	public void addPlayStat(TeamPlayStatData playStat) {
		this.playStat.add(playStat);
		playStat.setPlay(this);
	}

	/**
	 * @param drive the drive to set
	 */
	public void setDrive(DriveData drive) {
		this.drive = drive;
	}

	/**
	 * @return the playText
	 */
	public String getPlayText() {
		return playText;
	}

	/**
	 * @param playText the playText to set
	 */
	public void setPlayText(String playText) {
		this.playText = playText;
	}

	/**
	 * @return the driveText
	 */
	public String getDriveText() {
		return driveText;
	}

	/**
	 * @param driveText the driveText to set
	 */
	public void setDriveText(String driveText) {
		this.driveText = driveText;
	}

	/**
	 * @return the period
	 */
	public PlayPeriodEnum getPeriod() {
		return period;
	}

	/**
	 * @param period the period to set
	 */
	public void setPeriod(PlayPeriodEnum period) {
		this.period = period;
	}

	/**
	 * @return the playType
	 */
	public PlayTypeEnum getPlayType() {
		return playType;
	}

	/**
	 * @param playType the playType to set
	 */
	public void setPlayType(PlayTypeEnum playType) {
		this.playType = playType;
	}

	/**
	 * @return the playStartDown
	 */
	public PlayDownEnum getPlayStartDown() {
		return playStartDown;
	}

	/**
	 * @param playStartDown the playStartDown to set
	 */
	public void setPlayStartDown(PlayDownEnum playStartDown) {
		this.playStartDown = playStartDown;
	}

	/**
	 * @return the playStartYard
	 */
	public Integer getPlayStartYard() {
		return playStartYard;
	}

	/**
	 * @param playStartYard the playStartYard to set
	 */
	public void setPlayStartYard(Integer playStartYard) {
		this.playStartYard = playStartYard;
	}

	/**
	 * @return the playYardToGain
	 */
	public Integer getPlayYardToGain() {
		return playYardToGain;
	}

	/**
	 * @param playYardToGain the playYardToGain to set
	 */
	public void setPlayYardToGain(Integer playYardToGain) {
		this.playYardToGain = playYardToGain;
	}

	/**
	 * @return the playCallType
	 */
	public PlayCallTypeEnum getPlayCallType() {
		return playCallType;
	}

	/**
	 * @param playCallType the playCallType to set
	 */
	public void setPlayCallType(PlayCallTypeEnum playCallType) {
		this.playCallType = playCallType;
	}

	/**
	 * @return the noPlayPenalty
	 */
	public Boolean getNoPlayPenalty() {
		return noPlayPenalty;
	}

	/**
	 * @param noPlayPenalty the noPlayPenalty to set
	 */
	public void setNoPlayPenalty(Boolean noPlayPenalty) {
		this.noPlayPenalty = noPlayPenalty;
	}

	/**
	 * @return the playTempo
	 */
	public TempoTypeEnum getPlayTempo() {
		return playTempo;
	}

	/**
	 * @param playTempo the playTempo to set
	 */
	public void setPlayTempo(TempoTypeEnum playTempo) {
		this.playTempo = playTempo;
	}

	/**
	 * @return the playStartPossessionTeam
	 */
	public TeamData getPlayStartPossessionTeam() {
		return playStartPossessionTeam;
	}

	/**
	 * @param playStartPossessionTeam the playStartPossessionTeam to set
	 */
	public void setPlayStartPossessionTeam(TeamData playStartPossessionTeam) {
		this.playStartPossessionTeam = playStartPossessionTeam;
	}

	/**
	 * @return the playResultYard
	 */
	public Integer getPlayResultYard() {
		return playResultYard;
	}

	/**
	 * @param playResultYard the playResultYard to set
	 */
	public void setPlayResultYard(Integer playResultYard) {
		this.playResultYard = playResultYard;
	}

	/**
	 * @return the playResultPoints
	 */
	public Integer getPlayResultPoints() {
		return playResultPoints;
	}

	/**
	 * @param playResultPoints the playResultPoints to set
	 */
	public void setPlayResultPoints(Integer playResultPoints) {
		this.playResultPoints = playResultPoints;
	}

	/**
	 * @return the playResultFirstDown
	 */
	public Boolean getPlayResultFirstDown() {
		return playResultFirstDown;
	}

	/**
	 * @param playResultFirstDown the playResultFirstDown to set
	 */
	public void setPlayResultFirstDown(Boolean playResultFirstDown) {
		this.playResultFirstDown = playResultFirstDown;
	}

	/**
	 * @return the playResultTurnover
	 */
	public Boolean getPlayResultTurnover() {
		return playResultTurnover;
	}

	/**
	 * @param playResultTurnover the playResultTurnover to set
	 */
	public void setPlayResultTurnover(Boolean playResultTurnover) {
		this.playResultTurnover = playResultTurnover;
	}

	/**
	 * @return the playResultYardLine
	 */
	public Integer getPlayResultYardLine() {
		return playResultYardLine;
	}

	/**
	 * @param playResultYardLine the playResultYardLine to set
	 */
	public void setPlayResultYardLine(Integer playResultYardLine) {
		this.playResultYardLine = playResultYardLine;
	}

	/**
	 * @return the playResultPossessionTeam
	 */
	public TeamData getPlayResultPossessionTeam() {
		return playResultPossessionTeam;
	}

	/**
	 * @param playResultPossessionTeam the playResultPossessionTeam to set
	 */
	public void setPlayResultPossessionTeam(TeamData playResultPossessionTeam) {
		this.playResultPossessionTeam = playResultPossessionTeam;
	}

	/**
	 * @return the playStartHomeScore
	 */
	public Integer getPlayStartHomeScore() {
		return playStartHomeScore;
	}

	/**
	 * @param playStartHomeScore the playStartHomeScore to set
	 */
	public void setPlayStartHomeScore(Integer playStartHomeScore) {
		this.playStartHomeScore = playStartHomeScore;
	}

	/**
	 * @return the playStartAwayScore
	 */
	public Integer getPlayStartAwayScore() {
		return playStartAwayScore;
	}

	/**
	 * @param playStartAwayScore the playStartAwayScore to set
	 */
	public void setPlayStartAwayScore(Integer playStartAwayScore) {
		this.playStartAwayScore = playStartAwayScore;
	}

	/**
	 * @return the playResultHomeScore
	 */
	public Integer getPlayResultHomeScore() {
		return playResultHomeScore;
	}

	/**
	 * @param playResultHomeScore the playResultHomeScore to set
	 */
	public void setPlayResultHomeScore(Integer playResultHomeScore) {
		this.playResultHomeScore = playResultHomeScore;
	}

	/**
	 * @return the playResultAwayScore
	 */
	public Integer getPlayResultAwayScore() {
		return playResultAwayScore;
	}

	/**
	 * @param playResultAwayScore the playResultAwayScore to set
	 */
	public void setPlayResultAwayScore(Integer playResultAwayScore) {
		this.playResultAwayScore = playResultAwayScore;
	}



	/**
	 * @return the garbageTime
	 */
	public boolean getGarbageTime() {
		return garbageTime;
	}

	/**
	 * @param garbageTime the garbageTime to set
	 */
	public void setGarbageTime(boolean garbageTime) {
		this.garbageTime = garbageTime;
	}

	/**
	 * @return the passingDown
	 */
	public boolean getPassingDown() {
		return passingDown;
	}

	/**
	 * @param passingDown the passingDown to set
	 */
	public void setPassingDown(boolean passingDown) {
		this.passingDown = passingDown;
	}

	/**
	 * @return the playResultSuccess
	 */
	public boolean getPlayResultSuccess() {
		return playResultSuccess;
	}

	/**
	 * @param playResultSuccess the playResultSuccess to set
	 */
	public void setPlayResultSuccess(boolean playResultSuccess) {
		this.playResultSuccess = playResultSuccess;
	}

	/**
	 * @return the playFieldZone
	 */
	public FieldZoneEnum getPlayFieldZone() {
		return playFieldZone;
	}

	/**
	 * @param playFieldZone the playFieldZone to set
	 */
	public void setPlayFieldZone(FieldZoneEnum playFieldZone) {
		this.playFieldZone = playFieldZone;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(drive, driveText, garbageTime, noPlayPenalty, passingDown, period,
				playCallType, playFieldZone, playResultAwayScore, playResultFirstDown, playResultHomeScore,
				playResultPoints, playResultPossessionTeam, playResultSuccess, playResultTurnover, playResultYard,
				playResultYardLine, playStartAwayScore, playStartDown, playStartHomeScore, playStartPossessionTeam,
				playStartYard, playStat, playTempo, playText, playType, playYardToGain);
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
		if (!(obj instanceof PlayData)) {
			return false;
		}
		PlayData other = (PlayData) obj;
		return Objects.equals(drive, other.drive) && Objects.equals(driveText, other.driveText)
				&& garbageTime == other.garbageTime && Objects.equals(noPlayPenalty, other.noPlayPenalty)
				&& passingDown == other.passingDown && period == other.period && playCallType == other.playCallType
				&& playFieldZone == other.playFieldZone
				&& Objects.equals(playResultAwayScore, other.playResultAwayScore)
				&& Objects.equals(playResultFirstDown, other.playResultFirstDown)
				&& Objects.equals(playResultHomeScore, other.playResultHomeScore)
				&& Objects.equals(playResultPoints, other.playResultPoints)
				&& Objects.equals(playResultPossessionTeam, other.playResultPossessionTeam)
				&& playResultSuccess == other.playResultSuccess
				&& Objects.equals(playResultTurnover, other.playResultTurnover)
				&& Objects.equals(playResultYard, other.playResultYard)
				&& Objects.equals(playResultYardLine, other.playResultYardLine)
				&& Objects.equals(playStartAwayScore, other.playStartAwayScore) && playStartDown == other.playStartDown
				&& Objects.equals(playStartHomeScore, other.playStartHomeScore)
				&& Objects.equals(playStartPossessionTeam, other.playStartPossessionTeam)
				&& Objects.equals(playStartYard, other.playStartYard) && Objects.equals(playStat, other.playStat)
				&& playTempo == other.playTempo && Objects.equals(playText, other.playText)
				&& playType == other.playType && Objects.equals(playYardToGain, other.playYardToGain);
	}

	@Override
	public String toString() {
		return "PlayData [drive=" + drive + ", playText=" + playText + ", driveText=" + driveText + ", period=" + period
				+ ", playType=" + playType + ", playStartDown=" + playStartDown + ", playStartYard=" + playStartYard
				+ ", playYardToGain=" + playYardToGain + ", playCallType=" + playCallType + ", noPlayPenalty="
				+ noPlayPenalty + ", playTempo=" + playTempo + ", playStartPossessionTeam=" + playStartPossessionTeam
				+ ", playResultYard=" + playResultYard + ", playResultPoints=" + playResultPoints
				+ ", playResultFirstDown=" + playResultFirstDown + ", playResultTurnover=" + playResultTurnover
				+ ", playResultYardLine=" + playResultYardLine + ", playResultPossessionTeam="
				+ playResultPossessionTeam + ", playStat=" + playStat + ", playStartHomeScore=" + playStartHomeScore
				+ ", playStartAwayScore=" + playStartAwayScore + ", playResultHomeScore=" + playResultHomeScore
				+ ", playResultAwayScore=" + playResultAwayScore + ", garbageTime=" + garbageTime + ", passingDown="
				+ passingDown + ", playResultSuccess=" + playResultSuccess + ", playFieldZone=" + playFieldZone
				+ "]";
	}



}
