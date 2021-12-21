package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

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

	}

	public GameInfoStatusPojo(String distance, String finalMessage, String possession, String yardline, String clock,
			String updatedTimestamp, String down, String network, String yardsFromGoal, String winner, String startTime,
			String startTimeEpoch, String gameState, String currentPeriod) {
		super();
		this.distance = distance;
		this.finalMessage = finalMessage;
		this.possession = possession;
		this.yardline = yardline;
		this.clock = clock;
		this.updatedTimestamp = updatedTimestamp;
		this.down = down;
		this.network = network;
		this.yardsFromGoal = yardsFromGoal;
		this.winner = winner;
		this.startTime = startTime;
		this.startTimeEpoch = startTimeEpoch;
		this.gameState = gameState;
		this.currentPeriod = currentPeriod;
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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((clock == null) ? 0 : clock.hashCode());
		result = prime * result + ((currentPeriod == null) ? 0 : currentPeriod.hashCode());
		result = prime * result + ((distance == null) ? 0 : distance.hashCode());
		result = prime * result + ((down == null) ? 0 : down.hashCode());
		result = prime * result + ((finalMessage == null) ? 0 : finalMessage.hashCode());
		result = prime * result + ((gameState == null) ? 0 : gameState.hashCode());
		result = prime * result + ((network == null) ? 0 : network.hashCode());
		result = prime * result + ((possession == null) ? 0 : possession.hashCode());
		result = prime * result + ((startTime == null) ? 0 : startTime.hashCode());
		result = prime * result + ((startTimeEpoch == null) ? 0 : startTimeEpoch.hashCode());
		result = prime * result + ((updatedTimestamp == null) ? 0 : updatedTimestamp.hashCode());
		result = prime * result + ((winner == null) ? 0 : winner.hashCode());
		result = prime * result + ((yardline == null) ? 0 : yardline.hashCode());
		result = prime * result + ((yardsFromGoal == null) ? 0 : yardsFromGoal.hashCode());
		return result;
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
		if (clock == null) {
			if (other.clock != null) {
				return false;
			}
		} else if (!clock.equals(other.clock)) {
			return false;
		}
		if (currentPeriod == null) {
			if (other.currentPeriod != null) {
				return false;
			}
		} else if (!currentPeriod.equals(other.currentPeriod)) {
			return false;
		}
		if (distance == null) {
			if (other.distance != null) {
				return false;
			}
		} else if (!distance.equals(other.distance)) {
			return false;
		}
		if (down == null) {
			if (other.down != null) {
				return false;
			}
		} else if (!down.equals(other.down)) {
			return false;
		}
		if (finalMessage == null) {
			if (other.finalMessage != null) {
				return false;
			}
		} else if (!finalMessage.equals(other.finalMessage)) {
			return false;
		}
		if (gameState == null) {
			if (other.gameState != null) {
				return false;
			}
		} else if (!gameState.equals(other.gameState)) {
			return false;
		}
		if (network == null) {
			if (other.network != null) {
				return false;
			}
		} else if (!network.equals(other.network)) {
			return false;
		}
		if (possession == null) {
			if (other.possession != null) {
				return false;
			}
		} else if (!possession.equals(other.possession)) {
			return false;
		}
		if (startTime == null) {
			if (other.startTime != null) {
				return false;
			}
		} else if (!startTime.equals(other.startTime)) {
			return false;
		}
		if (startTimeEpoch == null) {
			if (other.startTimeEpoch != null) {
				return false;
			}
		} else if (!startTimeEpoch.equals(other.startTimeEpoch)) {
			return false;
		}
		if (updatedTimestamp == null) {
			if (other.updatedTimestamp != null) {
				return false;
			}
		} else if (!updatedTimestamp.equals(other.updatedTimestamp)) {
			return false;
		}
		if (winner == null) {
			if (other.winner != null) {
				return false;
			}
		} else if (!winner.equals(other.winner)) {
			return false;
		}
		if (yardline == null) {
			if (other.yardline != null) {
				return false;
			}
		} else if (!yardline.equals(other.yardline)) {
			return false;
		}
		if (yardsFromGoal == null) {
			if (other.yardsFromGoal != null) {
				return false;
			}
		} else if (!yardsFromGoal.equals(other.yardsFromGoal)) {
			return false;
		}
		return true;
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
