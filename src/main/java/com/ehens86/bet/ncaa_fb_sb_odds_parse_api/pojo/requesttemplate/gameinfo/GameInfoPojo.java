package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

import java.util.List;
import java.util.Objects;

public class GameInfoPojo {
	private List<Object> liveVideos;
	private GameInfoVenuePojo venue;
	private String inputMD5Sum;
	private GameInfoTeamPojo away;
	private GameInfoChampionshipPojo championship;
	private GameInfoTabsPojo tabs;
	private String id;
	private List<Object> gameLinks;
	private List<GameInfoLinescorePojo> linescores;
	private GameInfoTeamPojo home;
	private GameInfoStatusPojo status;

	public GameInfoPojo() {
		// Base constructor
	}

	/**
	 * @return the liveVideos
	 */
	public List<Object> getLiveVideos() {
		return liveVideos;
	}

	/**
	 * @param liveVideos the liveVideos to set
	 */
	public void setLiveVideos(List<Object> liveVideos) {
		this.liveVideos = liveVideos;
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
	 * @return the inputMD5Sum
	 */
	public String getInputMD5Sum() {
		return inputMD5Sum;
	}

	/**
	 * @param inputMD5Sum the inputMD5Sum to set
	 */
	public void setInputMD5Sum(String inputMD5Sum) {
		this.inputMD5Sum = inputMD5Sum;
	}

	/**
	 * @return the away
	 */
	public GameInfoTeamPojo getAway() {
		return away;
	}

	/**
	 * @param away the away to set
	 */
	public void setAway(GameInfoTeamPojo away) {
		this.away = away;
	}

	/**
	 * @return the championship
	 */
	public GameInfoChampionshipPojo getChampionship() {
		return championship;
	}

	/**
	 * @param championship the championship to set
	 */
	public void setChampionship(GameInfoChampionshipPojo championship) {
		this.championship = championship;
	}

	/**
	 * @return the tabs
	 */
	public GameInfoTabsPojo getTabs() {
		return tabs;
	}

	/**
	 * @param tabs the tabs to set
	 */
	public void setTabs(GameInfoTabsPojo tabs) {
		this.tabs = tabs;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the gameLinks
	 */
	public List<Object> getGameLinks() {
		return gameLinks;
	}

	/**
	 * @param gameLinks the gameLinks to set
	 */
	public void setGameLinks(List<Object> gameLinks) {
		this.gameLinks = gameLinks;
	}

	/**
	 * @return the linescores
	 */
	public List<GameInfoLinescorePojo> getLinescores() {
		return linescores;
	}

	/**
	 * @param linescores the linescores to set
	 */
	public void setLinescores(List<GameInfoLinescorePojo> linescores) {
		this.linescores = linescores;
	}

	/**
	 * @return the home
	 */
	public GameInfoTeamPojo getHome() {
		return home;
	}

	/**
	 * @param home the home to set
	 */
	public void setHome(GameInfoTeamPojo home) {
		this.home = home;
	}

	/**
	 * @return the status
	 */
	public GameInfoStatusPojo getStatus() {
		return status;
	}

	/**
	 * @param status the status to set
	 */
	public void setStatus(GameInfoStatusPojo status) {
		this.status = status;
	}

	@Override
	public int hashCode() {
		return Objects.hash(away, championship, gameLinks, home, id, inputMD5Sum, linescores, liveVideos, status, tabs,
				venue);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameInfoPojo)) {
			return false;
		}
		GameInfoPojo other = (GameInfoPojo) obj;
		return Objects.equals(away, other.away) && Objects.equals(championship, other.championship)
				&& Objects.equals(gameLinks, other.gameLinks) && Objects.equals(home, other.home)
				&& Objects.equals(id, other.id) && Objects.equals(inputMD5Sum, other.inputMD5Sum)
				&& Objects.equals(linescores, other.linescores) && Objects.equals(liveVideos, other.liveVideos)
				&& Objects.equals(status, other.status) && Objects.equals(tabs, other.tabs)
				&& Objects.equals(venue, other.venue);
	}

	@Override
	public String toString() {
		return "GameInfoPojo [liveVideos=" + liveVideos + ", venue=" + venue + ", inputMD5Sum=" + inputMD5Sum
				+ ", away=" + away + ", championship=" + championship + ", tabs=" + tabs + ", id=" + id + ", gameLinks="
				+ gameLinks + ", linescores=" + linescores + ", home=" + home + ", status=" + status + "]";
	}

}
