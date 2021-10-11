package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import java.util.HashMap;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayCallTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayDownEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PlayerStatPojo;

public class PlayPojo {
	private String playText;
	private String driveText;
	private PlayPeriodEnum period;
	private PlayTypeEnum playType;
	private PlayDownEnum playStartDown;
	private Integer playStartYard;
	private Integer playYardToGain;
	private Integer playExpectedPoints;

	private PlayCallTypeEnum playCallType;

	private PlayResultPojo playResult;
	private HashMap<String, PlayerStatPojo> playerStat;

	public PlayPojo() {
		this.playResult = new PlayResultPojo();
		this.playerStat = new HashMap<String, PlayerStatPojo>();
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
	 * @return the playExpectedPoints
	 */
	public Integer getPlayExpectedPoints() {
		return playExpectedPoints;
	}

	/**
	 * @param playExpectedPoints the playExpectedPoints to set
	 */
	public void setPlayExpectedPoints(Integer playExpectedPoints) {
		this.playExpectedPoints = playExpectedPoints;
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
	public HashMap<String, PlayerStatPojo> getPlayerStat() {
		return playerStat;
	}

	/**
	 * @param playerStat the playerStat to set
	 */
	public void setPlayerStat(HashMap<String, PlayerStatPojo> playerStat) {
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

}
