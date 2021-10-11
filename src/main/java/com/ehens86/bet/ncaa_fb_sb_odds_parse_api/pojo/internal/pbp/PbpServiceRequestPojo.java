package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp;

import java.util.Map;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp.PlayByPlayTeamPojo;

public class PbpServiceRequestPojo {
	private DrivePojo drive;
	private PlayPojo play;
	private PlayByPlayPlayPojo playRaw;
	private String playRawText;
	private String[] playTackles;
	private Map<String, HomeAwayEnum> teamDict;
	private Map<String, PlayByPlayTeamPojo> teamAbbrevDict;
	private String possessionTeam;
	private String defenseTeam;

	public PbpServiceRequestPojo(DrivePojo drive, PlayPojo play, PlayByPlayPlayPojo playRaw, String playRawText,
			String[] playTackles, Map<String, HomeAwayEnum> teamDict, Map<String, PlayByPlayTeamPojo> teamAbbrevDict,
			String possessionTeam, String defenseTeam) {
		super();
		this.drive = drive;
		this.play = play;
		this.playRaw = playRaw;
		this.playRawText = playRawText;
		this.playTackles = playTackles;
		this.teamDict = teamDict;
		this.teamAbbrevDict = teamAbbrevDict;
		this.possessionTeam = possessionTeam;
		this.defenseTeam = defenseTeam;
	}

	/**
	 * @return the drive
	 */
	public DrivePojo getDrive() {
		return drive;
	}

	/**
	 * @param drive the drive to set
	 */
	public void setDrive(DrivePojo drive) {
		this.drive = drive;
	}

	/**
	 * @return the play
	 */
	public PlayPojo getPlay() {
		return play;
	}

	/**
	 * @param play the play to set
	 */
	public void setPlay(PlayPojo play) {
		this.play = play;
	}

	/**
	 * @return the playRaw
	 */
	public PlayByPlayPlayPojo getPlayRaw() {
		return playRaw;
	}

	/**
	 * @param playRaw the playRaw to set
	 */
	public void setPlayRaw(PlayByPlayPlayPojo playRaw) {
		this.playRaw = playRaw;
	}

	/**
	 * @return the playRawText
	 */
	public String getPlayRawText() {
		return playRawText;
	}

	/**
	 * @param playRawText the playRawText to set
	 */
	public void setPlayRawText(String playRawText) {
		this.playRawText = playRawText;
	}

	/**
	 * @return the playTackles
	 */
	public String[] getPlayTackles() {
		return playTackles;
	}

	/**
	 * @param playTackles the playTackles to set
	 */
	public void setPlayTackles(String[] playTackles) {
		this.playTackles = playTackles;
	}

	/**
	 * @return the teamDict
	 */
	public Map<String, HomeAwayEnum> getTeamDict() {
		return teamDict;
	}

	/**
	 * @param teamDict the teamDict to set
	 */
	public void setTeamDict(Map<String, HomeAwayEnum> teamDict) {
		this.teamDict = teamDict;
	}

	/**
	 * @return the teamAbbrevDict
	 */
	public Map<String, PlayByPlayTeamPojo> getTeamAbbrevDict() {
		return teamAbbrevDict;
	}

	/**
	 * @param teamAbbrevDict the teamAbbrevDict to set
	 */
	public void setTeamAbbrevDict(Map<String, PlayByPlayTeamPojo> teamAbbrevDict) {
		this.teamAbbrevDict = teamAbbrevDict;
	}

	/**
	 * @return the possessionTeam
	 */
	public String getPossessionTeam() {
		return possessionTeam;
	}

	/**
	 * @param possessionTeam the possessionTeam to set
	 */
	public void setPossessionTeam(String possessionTeam) {
		this.possessionTeam = possessionTeam;
	}

	/**
	 * @return the defenseTeam
	 */
	public String getDefenseTeam() {
		return defenseTeam;
	}

	/**
	 * @param defenseTeam the defenseTeam to set
	 */
	public void setDefenseTeam(String defenseTeam) {
		this.defenseTeam = defenseTeam;
	}

}
