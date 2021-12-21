package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore;

import java.util.List;

public class BoxScoreTablePojo {
    private String id;
    private String headerColor;
    private String headerClass;
    private List<BoxScoreDataPojo> data;
    
    public BoxScoreTablePojo() {
    	
    }

	public BoxScoreTablePojo(String id, String headerColor, String headerClass, List<BoxScoreDataPojo> data) {
		super();
		this.id = id;
		this.headerColor = headerColor;
		this.headerClass = headerClass;
		this.data = data;
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
	 * @return the headerColor
	 */
	public String getHeaderColor() {
		return headerColor;
	}

	/**
	 * @param headerColor the headerColor to set
	 */
	public void setHeaderColor(String headerColor) {
		this.headerColor = headerColor;
	}

	/**
	 * @return the headerClass
	 */
	public String getHeaderClass() {
		return headerClass;
	}

	/**
	 * @param headerClass the headerClass to set
	 */
	public void setHeaderClass(String headerClass) {
		this.headerClass = headerClass;
	}

	/**
	 * @return the data
	 */
	public List<BoxScoreDataPojo> getData() {
		return data;
	}

	/**
	 * @param data the data to set
	 */
	public void setData(List<BoxScoreDataPojo> data) {
		this.data = data;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((data == null) ? 0 : data.hashCode());
		result = prime * result + ((headerClass == null) ? 0 : headerClass.hashCode());
		result = prime * result + ((headerColor == null) ? 0 : headerColor.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BoxScoreTablePojo)) {
			return false;
		}
		BoxScoreTablePojo other = (BoxScoreTablePojo) obj;
		if (data == null) {
			if (other.data != null) {
				return false;
			}
		} else if (!data.equals(other.data)) {
			return false;
		}
		if (headerClass == null) {
			if (other.headerClass != null) {
				return false;
			}
		} else if (!headerClass.equals(other.headerClass)) {
			return false;
		}
		if (headerColor == null) {
			if (other.headerColor != null) {
				return false;
			}
		} else if (!headerColor.equals(other.headerColor)) {
			return false;
		}
		if (id == null) {
			if (other.id != null) {
				return false;
			}
		} else if (!id.equals(other.id)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "BoxScoreTablePojo [id=" + id + ", headerColor=" + headerColor + ", headerClass=" + headerClass
				+ ", data=" + data + "]";
	}
    
    
}
