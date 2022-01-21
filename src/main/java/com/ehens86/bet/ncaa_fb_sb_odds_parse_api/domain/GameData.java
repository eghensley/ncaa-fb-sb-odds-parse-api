package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;

@Entity
@Table(name = "GAME")
public class GameData extends BaseAuditEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1767959576804906738L;

	@Id
	@Column(name = "NCAA_GAME_ID", updatable = false, nullable = false)
	private String ncaaGameId;
	@ManyToOne
	@JoinColumn(name = "HOME_TEAM_ID", referencedColumnName = "NCAA_TEAM_ID", nullable = false)
	private TeamData teamHome;
	@ManyToOne
	@JoinColumn(name = "AWAY_TEAM_ID", referencedColumnName = "NCAA_TEAM_ID", nullable = false)
	private TeamData teamAway;
	@Column(name = "DATE", nullable = false)
	private LocalDate gameDate;
	@Column(name = "WEEK", nullable = false)
	private Integer week;
	@Column(name = "TIME", nullable = true)
	private LocalTime gameTime;
	@Column(name = "DIVISION", nullable = false)
	private DivisionEnum division;
	@Column(name = "SEASON", nullable = false)
	private Integer season;
	@Column(name = "WINNER", nullable = false)
	private HomeAwayEnum winner;
	@Column(name = "F_VALID", nullable = false)
	private boolean valid;
	@Column(name = "F_PBP_COMPLETE", nullable = false)
	private boolean pbpComplete;
	@ManyToOne(cascade = {CascadeType.ALL})
	@JoinColumn(name = "VENUE_OID", referencedColumnName = "OID", nullable = true)
	private VenueData venue;
	@Column(name = "HOME_SCORE", nullable = true)
	private Integer homeScore;
	@Column(name = "AWAY_SCORE", nullable = true)
	private Integer awayScore;
	@OneToMany(mappedBy="game", cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	private List<DriveData> drives;

	public GameData() {
		this.drives = new ArrayList<>();
	}

	/**
	 * @return the ncaaGameId
	 */
	public String getNcaaGameId() {
		return ncaaGameId;
	}

	/**
	 * @param ncaaGameId the ncaaGameId to set
	 */
	public void setNcaaGameId(String ncaaGameId) {
		this.ncaaGameId = ncaaGameId;
	}

	/**
	 * @return the teamHome
	 */
	public TeamData getTeamHome() {
		return teamHome;
	}

	/**
	 * @param teamHome the teamHome to set
	 */
	public void setTeamHome(TeamData teamHome) {
		this.teamHome = teamHome;
	}

	/**
	 * @return the teamAway
	 */
	public TeamData getTeamAway() {
		return teamAway;
	}

	/**
	 * @param teamAway the teamAway to set
	 */
	public void setTeamAway(TeamData teamAway) {
		this.teamAway = teamAway;
	}

	/**
	 * @return the gameDate
	 */
	public LocalDate getGameDate() {
		return gameDate;
	}

	/**
	 * @param gameDate the gameDate to set
	 */
	public void setGameDate(LocalDate gameDate) {
		this.gameDate = gameDate;
	}

	/**
	 * @return the gameTime
	 */
	public LocalTime getGameTime() {
		return gameTime;
	}

	/**
	 * @param gameTime the gameTime to set
	 */
	public void setGameTime(LocalTime gameTime) {
		this.gameTime = gameTime;
	}

	/**
	 * @return the division
	 */
	public DivisionEnum getDivision() {
		return division;
	}

	/**
	 * @param division the division to set
	 */
	public void setDivision(DivisionEnum division) {
		this.division = division;
	}

	/**
	 * @return the season
	 */
	public Integer getSeason() {
		return season;
	}

	/**
	 * @param season the season to set
	 */
	public void setSeason(Integer season) {
		this.season = season;
	}

	/**
	 * @return the winner
	 */
	public HomeAwayEnum getWinner() {
		return winner;
	}

	/**
	 * @param winner the winner to set
	 */
	public void setWinner(HomeAwayEnum winner) {
		this.winner = winner;
	}

	/**
	 * @return the valid
	 */
	public boolean isValid() {
		return valid;
	}

	/**
	 * @param valid the valid to set
	 */
	public void setValid(boolean valid) {
		this.valid = valid;
	}

	/**
	 * @return the venue
	 */
	public VenueData getVenue() {
		return venue;
	}

	/**
	 * @param venue the venue to set
	 */
	public void setVenue(VenueData venue) {
		this.venue = venue;
	}

	/**
	 * @return the homeScore
	 */
	public Integer getHomeScore() {
		return homeScore;
	}

	/**
	 * @param homeScore the homeScore to set
	 */
	public void setHomeScore(Integer homeScore) {
		this.homeScore = homeScore;
	}

	/**
	 * @return the awayScore
	 */
	public Integer getAwayScore() {
		return awayScore;
	}

	/**
	 * @param awayScore the awayScore to set
	 */
	public void setAwayScore(Integer awayScore) {
		this.awayScore = awayScore;
	}

	/**
	 * @return the drives
	 */
	public List<DriveData> getDrives() {
		return drives;
	}

	/**
	 * @param drives the drives to set
	 */
	public void setDrives(List<DriveData> drives) {
		this.drives = drives;
	}
	
	public void addDrive(DriveData drive) {
		this.drives.add(drive);
		drive.setGame(this);
	}

	/**
	 * @return the pbpComplete
	 */
	public boolean isPbpComplete() {
		return pbpComplete;
	}

	/**
	 * @param pbpComplete the pbpComplete to set
	 */
	public void setPbpComplete(boolean pbpComplete) {
		this.pbpComplete = pbpComplete;
	}

	/**
	 * @return the week
	 */
	public Integer getWeek() {
		return week;
	}

	/**
	 * @param week the week to set
	 */
	public void setWeek(Integer week) {
		this.week = week;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(awayScore, division, drives, gameDate, gameTime, homeScore, ncaaGameId,
				pbpComplete, season, teamAway, teamHome, valid, venue, week, winner);
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
		if (!(obj instanceof GameData)) {
			return false;
		}
		GameData other = (GameData) obj;
		return Objects.equals(awayScore, other.awayScore) && division == other.division
				&& Objects.equals(drives, other.drives) && Objects.equals(gameDate, other.gameDate)
				&& Objects.equals(gameTime, other.gameTime) && Objects.equals(homeScore, other.homeScore)
				&& Objects.equals(ncaaGameId, other.ncaaGameId) && pbpComplete == other.pbpComplete
				&& Objects.equals(season, other.season) && Objects.equals(teamAway, other.teamAway)
				&& Objects.equals(teamHome, other.teamHome) && valid == other.valid
				&& Objects.equals(venue, other.venue) && Objects.equals(week, other.week) && winner == other.winner;
	}

	@Override
	public String toString() {
		return "GameData [ncaaGameId=" + ncaaGameId + ", teamHome=" + teamHome + ", teamAway=" + teamAway
				+ ", gameDate=" + gameDate + ", week=" + week + ", gameTime=" + gameTime + ", division=" + division
				+ ", season=" + season + ", winner=" + winner + ", valid=" + valid + ", pbpComplete=" + pbpComplete
				+ ", venue=" + venue + ", homeScore=" + homeScore + ", awayScore=" + awayScore + ", drives=" + drives
				+ "]";
	}

}
