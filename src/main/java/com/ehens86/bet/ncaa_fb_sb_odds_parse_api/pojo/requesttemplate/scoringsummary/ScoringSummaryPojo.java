package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.scoringsummary;

import java.util.List;

public class ScoringSummaryPojo {
	private ScoringSummaryMetaPojo meta;
	private List<ScoringSummaryPeriodPojo> periods;

	public ScoringSummaryPojo() {

	}

	public ScoringSummaryPojo(ScoringSummaryMetaPojo meta, List<ScoringSummaryPeriodPojo> periods) {
		super();
		this.meta = meta;
		this.periods = periods;
	}

	/**
	 * @return the meta
	 */
	public ScoringSummaryMetaPojo getMeta() {
		return meta;
	}

	/**
	 * @param meta the meta to set
	 */
	public void setMeta(ScoringSummaryMetaPojo meta) {
		this.meta = meta;
	}

	/**
	 * @return the periods
	 */
	public List<ScoringSummaryPeriodPojo> getPeriods() {
		return periods;
	}

	/**
	 * @param periods the periods to set
	 */
	public void setPeriods(List<ScoringSummaryPeriodPojo> periods) {
		this.periods = periods;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((meta == null) ? 0 : meta.hashCode());
		result = prime * result + ((periods == null) ? 0 : periods.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof ScoringSummaryPojo)) {
			return false;
		}
		ScoringSummaryPojo other = (ScoringSummaryPojo) obj;
		if (meta == null) {
			if (other.meta != null) {
				return false;
			}
		} else if (!meta.equals(other.meta)) {
			return false;
		}
		if (periods == null) {
			if (other.periods != null) {
				return false;
			}
		} else if (!periods.equals(other.periods)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "ScoringSummaryPojo [meta=" + meta + ", periods=" + periods + "]";
	}

}
