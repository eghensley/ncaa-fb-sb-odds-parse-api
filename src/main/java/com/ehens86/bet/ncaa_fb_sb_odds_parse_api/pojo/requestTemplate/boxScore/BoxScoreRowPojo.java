package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.boxScore;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BoxScoreRowPojo {
    @JsonProperty("class") 
    private String rowClass;
    private String display;
    
    public BoxScoreRowPojo() {
    	
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

	/**
	 * @return the display
	 */
	public String getDisplay() {
		return display;
	}

	/**
	 * @param display the display to set
	 */
	public void setDisplay(String display) {
		this.display = display;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((display == null) ? 0 : display.hashCode());
		result = prime * result + ((rowClass == null) ? 0 : rowClass.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BoxScoreRowPojo)) {
			return false;
		}
		BoxScoreRowPojo other = (BoxScoreRowPojo) obj;
		if (display == null) {
			if (other.display != null) {
				return false;
			}
		} else if (!display.equals(other.display)) {
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
		return "BoxScoreRowPojo [rowClass=" + rowClass + ", display=" + display + "]";
	}
    
    
}
