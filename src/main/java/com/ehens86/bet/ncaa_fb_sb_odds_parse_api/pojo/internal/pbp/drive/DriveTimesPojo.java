package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive;

import java.util.Objects;

public class DriveTimesPojo {
	private Integer startDrive;
	private Integer endDrive;

	public DriveTimesPojo(Integer startDrive) {
		super();
		this.startDrive = startDrive;
	}

	/**
	 * @return the startDrive
	 */
	public Integer getStartDrive() {
		return startDrive;
	}

	/**
	 * @param startDrive the startDrive to set
	 */
	public void setStartDrive(Integer startDrive) {
		this.startDrive = startDrive;
	}

	/**
	 * @return the endDrive
	 */
	public Integer getEndDrive() {
		return endDrive;
	}

	/**
	 * @param endDrive the endDrive to set
	 */
	public void setEndDrive(Integer endDrive) {
		this.endDrive = endDrive;
	}

	@Override
	public int hashCode() {
		return Objects.hash(endDrive, startDrive);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof DriveTimesPojo)) {
			return false;
		}
		DriveTimesPojo other = (DriveTimesPojo) obj;
		return Objects.equals(endDrive, other.endDrive) && Objects.equals(startDrive, other.startDrive);
	}

	@Override
	public String toString() {
		return "DriveTimesPojo [startDrive=" + startDrive + ", endDrive=" + endDrive + "]";
	}

}
