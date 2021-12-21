package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.FieldZoneEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.TempoTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PbpPlayerStatPojo;

public class PlayPojo {
	protected String playText;
	protected String driveText;
	protected PlayPeriodEnum period;
	protected PlayTypeEnum playType;
	protected PlayDownEnum playStartDown;
	protected Integer playStartYard;
	protected Integer playYardToGain;
	protected PlayCallTypeEnum playCallType;
	protected PlayResultPojo playResult;
	protected Map<String, PbpPlayerStatPojo> playerStat;
	protected Boolean noPlayPenalty;
	protected TempoTypeEnum playTempo;
	protected String playStartPossessionTeamId;
	protected Integer playStartHomeScore;
	protected Integer playStartAwayScore;
	protected Boolean garbageTime;
	protected Boolean passingDown;
	protected FieldZoneEnum playFieldZone;
	protected Boolean defeat;

	public PlayPojo() {
		this.playResult = new PlayResultPojo();
		this.playerStat = new HashMap<>();
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
	 * @return the playResult
	 */
	public PlayResultPojo getPlayResult() {
		return playResult;
	}

	/**
	 * @param playResult the playResult to set
	 */
	public void setPlayResult(PlayResultPojo playResult) {
		this.playResult = playResult;
	}

	/**
	 * @return the playerStat
	 */
	public Map<String, PbpPlayerStatPojo> getPlayerStat() {
		return playerStat;
	}

	/**
	 * @param playerStat the playerStat to set
	 */
	public void setPlayerStat(Map<String, PbpPlayerStatPojo> playerStat) {
		this.playerStat = playerStat;
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
	 * @return the playStartPossessionTeamId
	 */
	public String getPlayStartPossessionTeamId() {
		return playStartPossessionTeamId;
	}

	/**
	 * @param playStartPossessionTeamId the playStartPossessionTeamId to set
	 */
	public void setPlayStartPossessionTeamId(String playStartPossessionTeamId) {
		this.playStartPossessionTeamId = playStartPossessionTeamId;
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
	 * @return the garbageTime
	 */
	public Boolean getGarbageTime() {
		return garbageTime;
	}

	/**
	 * @param garbageTime the garbageTime to set
	 */
	public void setGarbageTime(Boolean garbageTime) {
		this.garbageTime = garbageTime;
	}

	/**
	 * @return the passingDown
	 */
	public Boolean getPassingDown() {
		return passingDown;
	}

	/**
	 * @param passingDown the passingDown to set
	 */
	public void setPassingDown(Boolean passingDown) {
		this.passingDown = passingDown;
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

	/**
	 * @return the defeat
	 */
	public Boolean getDefeat() {
		return defeat;
	}

	/**
	 * @param defeat the defeat to set
	 */
	public void setDefeat(Boolean defeat) {
		this.defeat = defeat;
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(defeat, driveText, garbageTime, noPlayPenalty, passingDown, period, playCallType,
				playFieldZone, playResult, playStartAwayScore, playStartDown, playStartHomeScore,
				playStartPossessionTeamId, playStartYard, playTempo, playText, playType, playYardToGain, playerStat);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayPojo)) {
			return false;
		}
		PlayPojo other = (PlayPojo) obj;
		return Objects.equals(defeat, other.defeat) && Objects.equals(driveText, other.driveText)
				&& Objects.equals(garbageTime, other.garbageTime) && Objects.equals(noPlayPenalty, other.noPlayPenalty)
				&& Objects.equals(passingDown, other.passingDown) && period == other.period
				&& playCallType == other.playCallType && playFieldZone == other.playFieldZone
				&& Objects.equals(playResult, other.playResult)
				&& Objects.equals(playStartAwayScore, other.playStartAwayScore) && playStartDown == other.playStartDown
				&& Objects.equals(playStartHomeScore, other.playStartHomeScore)
				&& Objects.equals(playStartPossessionTeamId, other.playStartPossessionTeamId)
				&& Objects.equals(playStartYard, other.playStartYard) && playTempo == other.playTempo
				&& Objects.equals(playText, other.playText) && playType == other.playType
				&& Objects.equals(playYardToGain, other.playYardToGain) && Objects.equals(playerStat, other.playerStat);
	}

	@Override
	public String toString() {
		return "PlayPojo [playText=" + playText + ", driveText=" + driveText + ", period=" + period + ", playType="
				+ playType + ", playStartDown=" + playStartDown + ", playStartYard=" + playStartYard
				+ ", playYardToGain=" + playYardToGain + ", playCallType=" + playCallType + ", playResult=" + playResult
				+ ", playerStat=" + playerStat + ", noPlayPenalty=" + noPlayPenalty + ", playTempo=" + playTempo
				+ ", playStartPossessionTeamId=" + playStartPossessionTeamId + ", playStartHomeScore="
				+ playStartHomeScore + ", playStartAwayScore=" + playStartAwayScore + ", garbageTime=" + garbageTime
				+ ", passingDown=" + passingDown + ", playFieldZone=" + playFieldZone + ", defeat=" + defeat + "]";
	}

}
