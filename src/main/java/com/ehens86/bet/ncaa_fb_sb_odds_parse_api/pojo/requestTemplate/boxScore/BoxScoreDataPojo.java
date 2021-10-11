package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore;

import java.util.ArrayList;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BoxScoreDataPojo {
    private ArrayList<BoxScoreRowPojo> row;
    @JsonProperty("class") 
    private String rowClass;
    
    public BoxScoreDataPojo() {
    	
    }

	public BoxScoreDataPojo(ArrayList<BoxScoreRowPojo> row, String rowClass) {
		super();
		this.row = row;
		this.rowClass = rowClass;
	}

	/**
	 * @return the row
	 */
	public ArrayList<BoxScoreRowPojo> getRow() {
		return row;
	}

	/**
	 * @param row the row to set
	 */
	public void setRow(ArrayList<BoxScoreRowPojo> row) {
		this.row = row;
	}

	/**
	 * @return the rowClass
	 */
	public String getRowClass() {
		return rowClass;
	}

	/**
	 * @param rowClass the rowClass to set
	 */
	public void setRowClass(String rowClass) {
		this.rowClass = rowClass;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((row == null) ? 0 : row.hashCode());
		result = prime * result + ((rowClass == null) ? 0 : rowClass.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BoxScoreDataPojo)) {
			return false;
		}
		BoxScoreDataPojo other = (BoxScoreDataPojo) obj;
		if (row == null) {
			if (other.row != null) {
				return false;
			}
		} else if (!row.equals(other.row)) {
			return false;
		}
		if (rowClass == null) {
			if (other.rowClass != null) {
				return false;
			}
		} else if (!rowClass.equals(other.rowClass)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "BoxScoreDataPojo [row=" + row + ", rowClass=" + rowClass + "]";
	}
    
    
}
