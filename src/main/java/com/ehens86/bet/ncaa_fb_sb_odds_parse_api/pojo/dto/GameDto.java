package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto;

import java.time.LocalDate;
import java.time.LocalTime;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;

public class GameDto {
	private String ncaaGameId;
	private TeamDto teamHome;
	private TeamDto teamAway;
	private LocalDate gameDate;
	private LocalTime gameTime;
	private DivisionEnum division;
	private Integer season;
	private HomeAwayEnum winner;
	private boolean valid;
	private Integer homeScore;
	private Integer awayScore;
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
	public TeamDto getTeamHome() {
		return teamHome;
	}
	/**
	 * @param teamHome the teamHome to set
	 */
	public void setTeamHome(TeamDto teamHome) {
		this.teamHome = teamHome;
	}
	/**
	 * @return the teamAway
	 */
	public TeamDto getTeamAway() {
		return teamAway;
	}
	/**
	 * @param teamAway the teamAway to set
	 */
	public void setTeamAway(TeamDto teamAway) {
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
	
	
}
