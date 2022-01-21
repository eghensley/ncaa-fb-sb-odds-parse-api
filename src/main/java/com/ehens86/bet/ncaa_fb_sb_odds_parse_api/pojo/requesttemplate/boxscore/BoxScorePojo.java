package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore;

import java.util.List;
import java.util.Objects;

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
		return Objects.hash(inputMD5Sum, meta, tables, updatedTimestamp);
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
		return Objects.equals(inputMD5Sum, other.inputMD5Sum) && Objects.equals(meta, other.meta)
				&& Objects.equals(tables, other.tables) && Objects.equals(updatedTimestamp, other.updatedTimestamp);
	}

	@Override
	public String toString() {
		return "BoxScorePojo [inputMD5Sum=" + inputMD5Sum + ", updatedTimestamp=" + updatedTimestamp + ", meta=" + meta
				+ ", tables=" + tables + "]";
	}
    
    
}
