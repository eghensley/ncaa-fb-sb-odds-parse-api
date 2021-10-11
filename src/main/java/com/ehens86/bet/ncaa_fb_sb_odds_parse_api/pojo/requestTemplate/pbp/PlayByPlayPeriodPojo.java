package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requestTemplate.pbp;

import java.util.List;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.PlayPeriodEnum;

public class PlayByPlayPeriodPojo {
	private String title;
	private String shortTitle;
	private List<PlayByPlayPossessionPojo> possessions;

	public PlayByPlayPeriodPojo() {

	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}

	public PlayPeriodEnum getTitleEnum() {
		if ("1ST".equals(this.title)) {
			return PlayPeriodEnum.FIRST;
		} else if ("2ND".equals(this.title)) {
			return PlayPeriodEnum.SECOND;
		} else if ("3RD".equals(this.title)) {
			return PlayPeriodEnum.THIRD;
		} else if ("4TH".equals(this.title)) {
			return PlayPeriodEnum.FOURTH;
		} else {
			throw new IllegalArgumentException(String.format("No PlayPeriodEnum for %s", this.title));
		}
	}

	/**
	 * @param title the title to set
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * @return the shortTitle
	 */
	public String getShortTitle() {
		return shortTitle;
	}

	/**
	 * @param shortTitle the shortTitle to set
	 */
	public void setShortTitle(String shortTitle) {
		this.shortTitle = shortTitle;
	}

	/**
	 * @return the possessions
	 */
	public List<PlayByPlayPossessionPojo> getPossessions() {
		return possessions;
	}

	/**
	 * @param possessions the possessions to set
	 */
	public void setPossessions(List<PlayByPlayPossessionPojo> possessions) {
		this.possessions = possessions;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((possessions == null) ? 0 : possessions.hashCode());
		result = prime * result + ((shortTitle == null) ? 0 : shortTitle.hashCode());
		result = prime * result + ((title == null) ? 0 : title.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayByPlayPeriodPojo)) {
			return false;
		}
		PlayByPlayPeriodPojo other = (PlayByPlayPeriodPojo) obj;
		if (possessions == null) {
			if (other.possessions != null) {
				return false;
			}
		} else if (!possessions.equals(other.possessions)) {
			return false;
		}
		if (shortTitle == null) {
			if (other.shortTitle != null) {
				return false;
			}
		} else if (!shortTitle.equals(other.shortTitle)) {
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
		return "PlayByPlayPeriodPojo [title=" + title + ", shortTitle=" + shortTitle + ", possessions=" + possessions
				+ "]";
	}

}
