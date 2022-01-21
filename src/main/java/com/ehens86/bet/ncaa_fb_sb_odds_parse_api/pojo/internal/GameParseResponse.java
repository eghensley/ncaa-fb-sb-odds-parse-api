package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import java.util.List;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.GameDto;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.TeamDto;

public class GameParseResponse {
	private List<TeamDto> extractedTeams;
	private List<GameDto> extractedGames;
	private Integer teamsAdded;
	private Integer gamesAdded;

	public GameParseResponse() {
		// Base constructor
	}

	/**
	 * @return the extractedTeams
	 */
	public List<TeamDto> getExtractedTeams() {
		return extractedTeams;
	}

	/**
	 * @param extractedTeams the extractedTeams to set
	 */
	public void setExtractedTeams(List<TeamDto> extractedTeams) {
		this.extractedTeams = extractedTeams;
	}

	/**
	 * @return the extractedGames
	 */
	public List<GameDto> getExtractedGames() {
		return extractedGames;
	}

	/**
	 * @param extractedGames the extractedGames to set
	 */
	public void setExtractedGames(List<GameDto> extractedGames) {
		this.extractedGames = extractedGames;
	}

	/**
	 * @return the teamsAdded
	 */
	public Integer getTeamsAdded() {
		return teamsAdded;
	}

	/**
	 * @param teamsAdded the teamsAdded to set
	 */
	public void setTeamsAdded(Integer teamsAdded) {
		this.teamsAdded = teamsAdded;
	}

	/**
	 * @return the gamesAdded
	 */
	public Integer getGamesAdded() {
		return gamesAdded;
	}

	/**
	 * @param gamesAdded the gamesAdded to set
	 */
	public void setGamesAdded(Integer gamesAdded) {
		this.gamesAdded = gamesAdded;
	}

}
