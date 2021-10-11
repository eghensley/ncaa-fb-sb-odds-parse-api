package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp;

import java.util.List;
import java.util.stream.Collectors;

public class PlayByPlayPojo {
	private String inputMD5Sum;
	private String updatedTimestamp;
	private PlayByPlayMetaPojo meta;
	private List<PlayByPlayPeriodPojo> periods;

	public PlayByPlayPojo() {

	}

	public PlayByPlayPojo(String inputMD5Sum, String updatedTimestamp, PlayByPlayMetaPojo meta,
			List<PlayByPlayPeriodPojo> periods) {
		super();
		this.inputMD5Sum = inputMD5Sum;
		this.updatedTimestamp = updatedTimestamp;
		this.meta = meta;
		this.periods = periods;
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
	public PlayByPlayMetaPojo getMeta() {
		return meta;
	}

	/**
	 * @param meta the meta to set
	 */
	public void setMeta(PlayByPlayMetaPojo meta) {
		this.meta = meta;
	}

	/**
	 * @return the periods
	 */
	public List<PlayByPlayPeriodPojo> getPeriods() {
		return periods;
	}

	/**
	 * @param periods the periods to set
	 */
	public void setPeriods(List<PlayByPlayPeriodPojo> periods) {
		this.periods = periods;
	}

	public String pullOpponent(String teamId) {
		return this.meta.getTeams().stream().filter(team -> !teamId.equals(team.getId())).collect(Collectors.toList())
				.get(0).getId();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((inputMD5Sum == null) ? 0 : inputMD5Sum.hashCode());
		result = prime * result + ((meta == null) ? 0 : meta.hashCode());
		result = prime * result + ((periods == null) ? 0 : periods.hashCode());
		result = prime * result + ((updatedTimestamp == null) ? 0 : updatedTimestamp.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayByPlayPojo)) {
			return false;
		}
		PlayByPlayPojo other = (PlayByPlayPojo) obj;
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
		if (periods == null) {
			if (other.periods != null) {
				return false;
			}
		} else if (!periods.equals(other.periods)) {
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
		return "PlayByPlayPojo [inputMD5Sum=" + inputMD5Sum + ", updatedTimestamp=" + updatedTimestamp + ", meta="
				+ meta + ", periods=" + periods + "]";
	}

}
