package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

import java.util.Objects;

public class GameInfoStatusPojo {
    private String distance;
    private String finalMessage;
    private String possession;
    private String yardline;
    private String clock;
    private String updatedTimestamp;
    private String down;
    private String network;
    private String yardsFromGoal;
    private String winner;
    private String startTime;
    private String startTimeEpoch;
    private String gameState;
    private String currentPeriod;
    
	public GameInfoStatusPojo() {
		// Base constructor
	}

	/**
	 * @return the distance
	 */
	public String getDistance() {
		return distance;
	}

	/**
	 * @param distance the distance to set
	 */
	public void setDistance(String distance) {
		this.distance = distance;
	}

	/**
	 * @return the finalMessage
	 */
	public String getFinalMessage() {
		return finalMessage;
	}

	/**
	 * @param finalMessage the finalMessage to set
	 */
	public void setFinalMessage(String finalMessage) {
		this.finalMessage = finalMessage;
	}

	/**
	 * @return the possession
	 */
	public String getPossession() {
		return possession;
	}

	/**
	 * @param possession the possession to set
	 */
	public void setPossession(String possession) {
		this.possession = possession;
	}

	/**
	 * @return the yardline
	 */
	public String getYardline() {
		return yardline;
	}

	/**
	 * @param yardline the yardline to set
	 */
	public void setYardline(String yardline) {
		this.yardline = yardline;
	}

	/**
	 * @return the clock
	 */
	public String getClock() {
		return clock;
	}

	/**
	 * @param clock the clock to set
	 */
	public void setClock(String clock) {
		this.clock = clock;
	}

	/**
	 * @return the updatedTimestamp
	 */
	public String getUpdatedTimestamp() {
		return updatedTimestamp;
	}

	/**
	 * @param updatedTimestamp the updatedTimestamp to set
	 */
	public void setUpdatedTimestamp(String updatedTimestamp) {
		this.updatedTimestamp = updatedTimestamp;
	}

	/**
	 * @return the down
	 */
	public String getDown() {
		return down;
	}

	/**
	 * @param down the down to set
	 */
	public void setDown(String down) {
		this.down = down;
	}

	/**
	 * @return the network
	 */
	public String getNetwork() {
		return network;
	}

	/**
	 * @param network the network to set
	 */
	public void setNetwork(String network) {
		this.network = network;
	}

	/**
	 * @return the yardsFromGoal
	 */
	public String getYardsFromGoal() {
		return yardsFromGoal;
	}

	/**
	 * @param yardsFromGoal the yardsFromGoal to set
	 */
	public void setYardsFromGoal(String yardsFromGoal) {
		this.yardsFromGoal = yardsFromGoal;
	}

	/**
	 * @return the winner
	 */
	public String getWinner() {
		return winner;
	}

	/**
	 * @param winner the winner to set
	 */
	public void setWinner(String winner) {
		this.winner = winner;
	}

	/**
	 * @return the startTime
	 */
	public String getStartTime() {
		return startTime;
	}

	/**
	 * @param startTime the startTime to set
	 */
	public void setStartTime(String startTime) {
		this.startTime = startTime;
	}

	/**
	 * @return the startTimeEpoch
	 */
	public String getStartTimeEpoch() {
		return startTimeEpoch;
	}

	/**
	 * @param startTimeEpoch the startTimeEpoch to set
	 */
	public void setStartTimeEpoch(String startTimeEpoch) {
		this.startTimeEpoch = startTimeEpoch;
	}

	/**
	 * @return the gameState
	 */
	public String getGameState() {
		return gameState;
	}

	/**
	 * @param gameState the gameState to set
	 */
	public void setGameState(String gameState) {
		this.gameState = gameState;
	}

	/**
	 * @return the currentPeriod
	 */
	public String getCurrentPeriod() {
		return currentPeriod;
	}

	/**
	 * @param currentPeriod the currentPeriod to set
	 */
	public void setCurrentPeriod(String currentPeriod) {
		this.currentPeriod = currentPeriod;
	}

	@Override
	public int hashCode() {
		return Objects.hash(clock, currentPeriod, distance, down, finalMessage, gameState, network, possession,
				startTime, startTimeEpoch, updatedTimestamp, winner, yardline, yardsFromGoal);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameInfoStatusPojo)) {
			return false;
		}
		GameInfoStatusPojo other = (GameInfoStatusPojo) obj;
		return Objects.equals(clock, other.clock) && Objects.equals(currentPeriod, other.currentPeriod)
				&& Objects.equals(distance, other.distance) && Objects.equals(down, other.down)
				&& Objects.equals(finalMessage, other.finalMessage) && Objects.equals(gameState, other.gameState)
				&& Objects.equals(network, other.network) && Objects.equals(possession, other.possession)
				&& Objects.equals(startTime, other.startTime) && Objects.equals(startTimeEpoch, other.startTimeEpoch)
				&& Objects.equals(updatedTimestamp, other.updatedTimestamp) && Objects.equals(winner, other.winner)
				&& Objects.equals(yardline, other.yardline) && Objects.equals(yardsFromGoal, other.yardsFromGoal);
	}

	@Override
	public String toString() {
		return "GameInfoStatus [distance=" + distance + ", finalMessage=" + finalMessage + ", possession=" + possession
				+ ", yardline=" + yardline + ", clock=" + clock + ", updatedTimestamp=" + updatedTimestamp + ", down="
				+ down + ", network=" + network + ", yardsFromGoal=" + yardsFromGoal + ", winner=" + winner
				+ ", startTime=" + startTime + ", startTimeEpoch=" + startTimeEpoch + ", gameState=" + gameState
				+ ", currentPeriod=" + currentPeriod + "]";
	}
    
    
}
