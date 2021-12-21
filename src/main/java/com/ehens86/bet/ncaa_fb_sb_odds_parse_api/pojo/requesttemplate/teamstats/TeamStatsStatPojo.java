package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.teamstats;

import java.util.List;
import java.util.stream.Collectors;

public class TeamStatsStatPojo {
	private String stat;
	private String data;
	private List<TeamStatsBreakdownPojo> breakdown;

	public TeamStatsStatPojo() {

	}

	public TeamStatsStatPojo(String stat, String data, List<TeamStatsBreakdownPojo> breakdown) {
		super();
		this.stat = stat;
		this.data = data;
		this.breakdown = breakdown;
	}

	/**
	 * @return the stat
	 */
	public String getStat() {
		return stat;
	}

	/**
	 * @param stat the stat to set
	 */
	public void setStat(String stat) {
		this.stat = stat;
	}

	/**
	 * @return the data
	 */
	public String getData() {
		return data;
	}

	/**
	 * @param data the data to set
	 */
	public void setData(String data) {
		this.data = data;
	}

	/**
	 * @return the breakdown
	 */
	public List<TeamStatsBreakdownPojo> getBreakdown() {
		return breakdown;
	}

	/**
	 * @param breakdown the breakdown to set
	 */
	public void setBreakdown(List<TeamStatsBreakdownPojo> breakdown) {
		this.breakdown = breakdown;
	}

	public String pullValueByKey(String key) {
		return this.breakdown.stream().filter(s -> key.equals(s.getStat())).collect(Collectors.toList()).get(0)
				.getData();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((breakdown == null) ? 0 : breakdown.hashCode());
		result = prime * result + ((data == null) ? 0 : data.hashCode());
		result = prime * result + ((stat == null) ? 0 : stat.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatsStatPojo)) {
			return false;
		}
		TeamStatsStatPojo other = (TeamStatsStatPojo) obj;
		if (breakdown == null) {
			if (other.breakdown != null) {
				return false;
			}
		} else if (!breakdown.equals(other.breakdown)) {
			return false;
		}
		if (data == null) {
			if (other.data != null) {
				return false;
			}
		} else if (!data.equals(other.data)) {
			return false;
		}
		if (stat == null) {
			if (other.stat != null) {
				return false;
			}
		} else if (!stat.equals(other.stat)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TeamStatsStatPojo [stat=" + stat + ", data=" + data + ", breakdown=" + breakdown + "]";
	}

}
