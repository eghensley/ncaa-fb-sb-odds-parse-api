package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.scoringSummary;

import java.util.ArrayList;

public class ScoringSummaryPeriodPojo {
    private String title;
    private ArrayList<ScoringSummarySummaryPojo> summary;
    
    public ScoringSummaryPeriodPojo() {
    	
    }

	public ScoringSummaryPeriodPojo(String title, ArrayList<ScoringSummarySummaryPojo> summary) {
		super();
		this.title = title;
		this.summary = summary;
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * @param title the title to set
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * @return the summary
	 */
	public ArrayList<ScoringSummarySummaryPojo> getSummary() {
		return summary;
	}

	/**
	 * @param summary the summary to set
	 */
	public void setSummary(ArrayList<ScoringSummarySummaryPojo> summary) {
		this.summary = summary;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((summary == null) ? 0 : summary.hashCode());
		result = prime * result + ((title == null) ? 0 : title.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof ScoringSummaryPeriodPojo)) {
			return false;
		}
		ScoringSummaryPeriodPojo other = (ScoringSummaryPeriodPojo) obj;
		if (summary == null) {
			if (other.summary != null) {
				return false;
			}
		} else if (!summary.equals(other.summary)) {
			return false;
		}
		if (title == null) {
			if (other.title != null) {
				return false;
			}
		} else if (!title.equals(other.title)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "ScoringSummaryPeriodPojo [title=" + title + ", summary=" + summary + "]";
	}
    
    
}
