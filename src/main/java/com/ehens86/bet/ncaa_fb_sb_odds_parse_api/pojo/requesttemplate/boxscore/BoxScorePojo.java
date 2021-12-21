package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore;

import java.util.List;

public class BoxScorePojo {
    private String inputMD5Sum;
    private String updatedTimestamp;
    private BoxScoreMetaPojo meta;
    private List<BoxScoreTablePojo> tables;
    
    public BoxScorePojo() {
    	
    }

	public BoxScorePojo(String inputMD5Sum, String updatedTimestamp, BoxScoreMetaPojo meta,
			List<BoxScoreTablePojo> tables) {
		super();
		this.inputMD5Sum = inputMD5Sum;
		this.updatedTimestamp = updatedTimestamp;
		this.meta = meta;
		this.tables = tables;
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
	 * @return the meta
	 */
	public BoxScoreMetaPojo getMeta() {
		return meta;
	}

	/**
	 * @param meta the meta to set
	 */
	public void setMeta(BoxScoreMetaPojo meta) {
		this.meta = meta;
	}

	/**
	 * @return the tables
	 */
	public List<BoxScoreTablePojo> getTables() {
		return tables;
	}

	/**
	 * @param tables the tables to set
	 */
	public void setTables(List<BoxScoreTablePojo> tables) {
		this.tables = tables;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((inputMD5Sum == null) ? 0 : inputMD5Sum.hashCode());
		result = prime * result + ((meta == null) ? 0 : meta.hashCode());
		result = prime * result + ((tables == null) ? 0 : tables.hashCode());
		result = prime * result + ((updatedTimestamp == null) ? 0 : updatedTimestamp.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BoxScorePojo)) {
			return false;
		}
		BoxScorePojo other = (BoxScorePojo) obj;
		if (inputMD5Sum == null) {
			if (other.inputMD5Sum != null) {
				return false;
			}
		} else if (!inputMD5Sum.equals(other.inputMD5Sum)) {
			return false;
		}
		if (meta == null) {
			if (other.meta != null) {
				return false;
			}
		} else if (!meta.equals(other.meta)) {
			return false;
		}
		if (tables == null) {
			if (other.tables != null) {
				return false;
			}
		} else if (!tables.equals(other.tables)) {
			return false;
		}
		if (updatedTimestamp == null) {
			if (other.updatedTimestamp != null) {
				return false;
			}
		} else if (!updatedTimestamp.equals(other.updatedTimestamp)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "BoxScorePojo [inputMD5Sum=" + inputMD5Sum + ", updatedTimestamp=" + updatedTimestamp + ", meta=" + meta
				+ ", tables=" + tables + "]";
	}
    
    
}
