package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive;

import java.util.Objects;

public class RollforwardLastPlayPojo {
	private Integer lastPlayEndYardLine;
	private String lastDriveText;

	public RollforwardLastPlayPojo() {

	}

	public RollforwardLastPlayPojo(Integer lastPlayEndYardLine, String lastDriveText) {
		super();
		this.lastPlayEndYardLine = lastPlayEndYardLine;
		this.lastDriveText = lastDriveText;
	}

	/**
	 * @return the lastPlayEndYardLine
	 */
	public Integer getLastPlayEndYardLine() {
		return lastPlayEndYardLine;
	}

	/**
	 * @param lastPlayEndYardLine the lastPlayEndYardLine to set
	 */
	public void setLastPlayEndYardLine(Integer lastPlayEndYardLine) {
		this.lastPlayEndYardLine = lastPlayEndYardLine;
	}

	/**
	 * @return the lastDriveText
	 */
	public String getLastDriveText() {
		return lastDriveText;
	}

	/**
	 * @param lastDriveText the lastDriveText to set
	 */
	public void setLastDriveText(String lastDriveText) {
		this.lastDriveText = lastDriveText;
	}

	@Override
	public int hashCode() {
		return Objects.hash(lastDriveText, lastPlayEndYardLine);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof RollforwardLastPlayPojo)) {
			return false;
		}
		RollforwardLastPlayPojo other = (RollforwardLastPlayPojo) obj;
		return Objects.equals(lastDriveText, other.lastDriveText)
				&& Objects.equals(lastPlayEndYardLine, other.lastPlayEndYardLine);
	}

	@Override
	public String toString() {
		return "RollforwardLastPlayPojo [lastPlayEndYardLine=" + lastPlayEndYardLine + ", lastDriveText="
				+ lastDriveText + "]";
	}

}
