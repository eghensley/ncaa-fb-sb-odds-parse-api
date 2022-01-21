package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.boxscore;

import java.util.List;
import java.util.Objects;

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
		return Objects.hash(data, headerClass, headerColor, id);
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
		return Objects.equals(data, other.data) && Objects.equals(headerClass, other.headerClass)
				&& Objects.equals(headerColor, other.headerColor) && Objects.equals(id, other.id);
	}

	@Override
	public String toString() {
		return "BoxScoreTablePojo [id=" + id + ", headerColor=" + headerColor + ", headerClass=" + headerClass
				+ ", data=" + data + "]";
	}
    
    
}
