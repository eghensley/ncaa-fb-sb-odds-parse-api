package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

public class GameInfoTeamPojo {
	private int score;
	private GameInfoTeamNamePojo names;
	private String color;
	private String seed;
	private String record;
    private String rank;
    
    public GameInfoTeamPojo() {
    	
    }

	public GameInfoTeamPojo(int score, GameInfoTeamNamePojo names, String color, String seed, String record,
			String rank) {
		super();
		this.score = score;
		this.names = names;
		this.color = color;
		this.seed = seed;
		this.record = record;
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
	public String getRecord() {
		return record;
	}

	/**
	 * @param record the record to set
	 */
	public void setRecord(String record) {
		this.record = record;
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
		final int prime = 31;
		int result = 1;
		result = prime * result + ((color == null) ? 0 : color.hashCode());
		result = prime * result + ((names == null) ? 0 : names.hashCode());
		result = prime * result + ((rank == null) ? 0 : rank.hashCode());
		result = prime * result + ((record == null) ? 0 : record.hashCode());
		result = prime * result + score;
		result = prime * result + ((seed == null) ? 0 : seed.hashCode());
		return result;
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
		if (color == null) {
			if (other.color != null) {
				return false;
			}
		} else if (!color.equals(other.color)) {
			return false;
		}
		if (names == null) {
			if (other.names != null) {
				return false;
			}
		} else if (!names.equals(other.names)) {
			return false;
		}
		if (rank == null) {
			if (other.rank != null) {
				return false;
			}
		} else if (!rank.equals(other.rank)) {
			return false;
		}
		if (record == null) {
			if (other.record != null) {
				return false;
			}
		} else if (!record.equals(other.record)) {
			return false;
		}
		if (score != other.score) {
			return false;
		}
		if (seed == null) {
			if (other.seed != null) {
				return false;
			}
		} else if (!seed.equals(other.seed)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "GameInfoTeamPojo [score=" + score + ", names=" + names + ", color=" + color + ", seed=" + seed
				+ ", record=" + record + ", rank=" + rank + "]";
	}
    
    
}
