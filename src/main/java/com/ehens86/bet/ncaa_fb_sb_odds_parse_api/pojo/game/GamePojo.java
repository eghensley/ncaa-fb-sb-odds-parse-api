package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.DivisionEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.HomeAwayEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PbpPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.gameInfo.GameInfoVenuePojo;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class GamePojo {
	@JsonIgnore
	private TeamPojo teamHome;
	@JsonIgnore
	private TeamPojo teamAway;
	private String gameDate;
	private String statsUrl;
	private String gameTime;
	private DivisionEnum division;
	private Integer season;
	private HomeAwayEnum winner;
	private boolean valid;
	@JsonIgnore
	private GameInfoVenuePojo venue;
	private PbpPojo plays;
	
	public GamePojo() {
		this.valid = true;
	}

	/**
	 * @return the teamHome
	 */
	public TeamPojo getTeamHome() {
		return teamHome;
	}

	/**
	 * @param teamHome the teamHome to set
	 */
	public void setTeamHome(TeamPojo teamHome) {
		this.teamHome = teamHome;
	}

	/**
	 * @return the teamAway
	 */
	public TeamPojo getTeamAway() {
		return teamAway;
	}

	/**
	 * @param teamAway the teamAway to set
	 */
	public void setTeamAway(TeamPojo teamAway) {
		this.teamAway = teamAway;
	}

	/**
	 * @return the gameDate
	 */
	public String getGameDate() {
		return gameDate;
	}

	/**
	 * @param gameDate the gameDate to set
	 */
	public void setGameDate(String gameDate) {
		this.gameDate = gameDate;
	}

	/**
	 * @return the statsUrl
	 */
	public String getStatsUrl() {
		return statsUrl;
	}

	/**
	 * @param statsUrl the statsUrl to set
	 */
	public void setStatsUrl(String statsUrl) {
		this.statsUrl = statsUrl;
	}

	/**
	 * @return the gameTime
	 */
	public String getGameTime() {
		return gameTime;
	}

	/**
	 * @param gameTime the gameTime to set
	 */
	public void setGameTime(String gameTime) {
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

	public TeamPojo pullTeamById(String ncaaTeamId) {
		if (ncaaTeamId.equals(this.teamHome.getNcaaTeamId())) {
			return this.teamHome;
		} else if (ncaaTeamId.equals(this.teamAway.getNcaaTeamId())) {
			return this.teamAway;
		} else {
			throw new IllegalArgumentException();
		}
	}

	public TeamPojo pullOpponentById(String ncaaTeamId) {
		if (ncaaTeamId.equals(this.teamHome.getNcaaTeamId())) {
			return this.teamAway;
		} else if (ncaaTeamId.equals(this.teamAway.getNcaaTeamId())) {
			return this.teamHome;
		} else {
			throw new IllegalArgumentException();
		}
	}
	
	public void addTeamById(TeamPojo team) {
		if (team.getNcaaTeamId().equals(this.teamHome.getNcaaTeamId())) {
			this.teamHome = team;
		} else if (team.getNcaaTeamId().equals(this.teamAway.getNcaaTeamId())) {
			this.teamAway = team;
		} else {
			throw new IllegalArgumentException();
		}
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
	public GameInfoVenuePojo getVenue() {
		return venue;
	}

	/**
	 * @param venue the venue to set
	 */
	public void setVenue(GameInfoVenuePojo venue) {
		this.venue = venue;
	}

	/**
	 * @return the plays
	 */
	public PbpPojo getPlays() {
		return plays;
	}

	/**
	 * @param plays the plays to set
	 */
	public void setPlays(PbpPojo plays) {
		this.plays = plays;
	}

	public GamePojo(TeamPojo teamHome, TeamPojo teamAway, String gameDate, String statsUrl, String gameTime,
			DivisionEnum division, Integer season, HomeAwayEnum winner, boolean valid, GameInfoVenuePojo venue,
			PbpPojo plays) {
		super();
		this.teamHome = teamHome;
		this.teamAway = teamAway;
		this.gameDate = gameDate;
		this.statsUrl = statsUrl;
		this.gameTime = gameTime;
		this.division = division;
		this.season = season;
		this.winner = winner;
		this.valid = valid;
		this.venue = venue;
		this.plays = plays;
	}



}
