package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayTeamPojo;

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
	private String puntTeam;
	private String puntReturnTeam;
	private String kickoffTeam;
	private String kickoffReturnTeam;

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
		if (Objects.nonNull(this.puntReturnTeam)) {
			return this.puntReturnTeam;
		} else if (Objects.nonNull(this.kickoffReturnTeam)) {
			return this.kickoffReturnTeam;
		} else {
			return possessionTeam;
		}
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
		if (Objects.nonNull(this.puntTeam)) {
			return this.puntTeam;
		} else if (Objects.nonNull(this.kickoffTeam)) {
			return this.kickoffTeam;
		} else {
			return defenseTeam;
		}
	}

	/**
	 * @param defenseTeam the defenseTeam to set
	 */
	public void setDefenseTeam(String defenseTeam) {
		this.defenseTeam = defenseTeam;
	}

	/**
	 * @param puntTeam the puntTeam to set
	 */
	public void setPuntTeam(String puntTeam) {
		this.puntTeam = puntTeam;
	}

	/**
	 * @param puntReturnTeam the puntReturnTeam to set
	 */
	public void setPuntReturnTeam(String puntReturnTeam) {
		this.puntReturnTeam = puntReturnTeam;
	}

	/**
	 * @param kickoffTeam the kickoffTeam to set
	 */
	public void setKickoffTeam(String kickoffTeam) {
		this.kickoffTeam = kickoffTeam;
	}

	/**
	 * @param kickoffReturnTeam the kickoffReturnTeam to set
	 */
	public void setKickoffReturnTeam(String kickoffReturnTeam) {
		this.kickoffReturnTeam = kickoffReturnTeam;
	}

	public boolean evalIfHasPenalty() {
		return !(this.getPlay().getPlayerStat().get(this.getPossessionTeam()).getPenalty().isEmpty()
				&& this.getPlay().getPlayerStat().get(this.defenseTeam).getPenalty().isEmpty());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(playTackles);
		result = prime * result + Objects.hash(defenseTeam, drive, kickoffReturnTeam, kickoffTeam, play, playRaw,
				playRawText, possessionTeam, puntReturnTeam, puntTeam, teamAbbrevDict, teamDict);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PbpServiceRequestPojo)) {
			return false;
		}
		PbpServiceRequestPojo other = (PbpServiceRequestPojo) obj;
		return Objects.equals(defenseTeam, other.defenseTeam) && Objects.equals(drive, other.drive)
				&& Objects.equals(kickoffReturnTeam, other.kickoffReturnTeam)
				&& Objects.equals(kickoffTeam, other.kickoffTeam) && Objects.equals(play, other.play)
				&& Objects.equals(playRaw, other.playRaw) && Objects.equals(playRawText, other.playRawText)
				&& Arrays.equals(playTackles, other.playTackles) && Objects.equals(possessionTeam, other.possessionTeam)
				&& Objects.equals(puntReturnTeam, other.puntReturnTeam) && Objects.equals(puntTeam, other.puntTeam)
				&& Objects.equals(teamAbbrevDict, other.teamAbbrevDict) && Objects.equals(teamDict, other.teamDict);
	}

	@Override
	public String toString() {
		return "PbpServiceRequestPojo [drive=" + drive + ", play=" + play + ", playRaw=" + playRaw + ", playRawText="
				+ playRawText + ", playTackles=" + Arrays.toString(playTackles) + ", teamDict=" + teamDict
				+ ", teamAbbrevDict=" + teamAbbrevDict + ", possessionTeam=" + possessionTeam + ", defenseTeam="
				+ defenseTeam + ", puntTeam=" + puntTeam + ", puntReturnTeam=" + puntReturnTeam + ", kickoffTeam="
				+ kickoffTeam + ", kickoffReturnTeam=" + kickoffReturnTeam + "]";
	}
	
	
}
