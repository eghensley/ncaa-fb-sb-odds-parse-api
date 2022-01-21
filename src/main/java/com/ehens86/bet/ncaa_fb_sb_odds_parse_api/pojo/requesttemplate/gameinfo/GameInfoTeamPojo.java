package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

import java.util.Objects;

public class GameInfoTeamPojo {
	private int score;
	private GameInfoTeamNamePojo names;
	private String color;
	private String seed;
	private String teamRecord;
    private String rank;
    
    public GameInfoTeamPojo() {
    	
    }

	public GameInfoTeamPojo(int score, GameInfoTeamNamePojo names, String color, String seed, String teamRecord,
			String rank) {
		super();
		this.score = score;
		this.names = names;
		this.color = color;
		this.seed = seed;
		this.teamRecord = teamRecord;
		this.rank = rank;
	}

	/**
	 * @return the score
	 */
	public int getScore() {
		return score;
	}

	/**
	 * @param score the score to set
	 */
	public void setScore(int score) {
		this.score = score;
	}

	/**
	 * @return the names
	 */
	public GameInfoTeamNamePojo getNames() {
		return names;
	}

	/**
	 * @param names the names to set
	 */
	public void setNames(GameInfoTeamNamePojo names) {
		this.names = names;
	}

	/**
	 * @return the color
	 */
	public String getColor() {
		return color;
	}

	/**
	 * @param color the color to set
	 */
	public void setColor(String color) {
		this.color = color;
	}

	/**
	 * @return the seed
	 */
	public String getSeed() {
		return seed;
	}

	/**
	 * @param seed the seed to set
	 */
	public void setSeed(String seed) {
		this.seed = seed;
	}

	/**
	 * @return the record
	 */
	public String getTeamRecord() {
		return teamRecord;
	}

	/**
	 * @param record the record to set
	 */
	public void setRecord(String teamRecord) {
		this.teamRecord = teamRecord;
	}

	/**
	 * @return the rank
	 */
	public String getRank() {
		return rank;
	}

	/**
	 * @param rank the rank to set
	 */
	public void setRank(String rank) {
		this.rank = rank;
	}

	@Override
	public int hashCode() {
		return Objects.hash(color, names, rank, score, seed, teamRecord);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameInfoTeamPojo)) {
			return false;
		}
		GameInfoTeamPojo other = (GameInfoTeamPojo) obj;
		return Objects.equals(color, other.color) && Objects.equals(names, other.names)
				&& Objects.equals(rank, other.rank) && score == other.score && Objects.equals(seed, other.seed)
				&& Objects.equals(teamRecord, other.teamRecord);
	}

	@Override
	public String toString() {
		return "GameInfoTeamPojo [score=" + score + ", names=" + names + ", color=" + color + ", seed=" + seed
				+ ", teamRecord=" + teamRecord + ", rank=" + rank + "]";
	}
    
    
}
