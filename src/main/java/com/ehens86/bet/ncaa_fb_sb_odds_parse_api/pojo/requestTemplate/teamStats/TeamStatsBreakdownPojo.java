package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.teamStats;

public class TeamStatsBreakdownPojo {
    private String stat;
    private String data;
    
    public TeamStatsBreakdownPojo() {
    	
    }

	public TeamStatsBreakdownPojo(String stat, String data) {
		super();
		this.stat = stat;
		this.data = data;
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((data == null) ? 0 : data.hashCode());
		result = prime * result + ((stat == null) ? 0 : stat.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TeamStatsBreakdownPojo)) {
			return false;
		}
		TeamStatsBreakdownPojo other = (TeamStatsBreakdownPojo) obj;
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
		return "TeamStatsBreakdownPojo [stat=" + stat + ", data=" + data + "]";
	}
    
    
}
