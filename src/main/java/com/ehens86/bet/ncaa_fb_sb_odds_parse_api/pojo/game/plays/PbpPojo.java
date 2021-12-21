package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class PbpPojo {
	private List<DrivePojo> drives;

	public PbpPojo() {
		this.drives = new ArrayList<>();
	}

	/**
	 * @return the drives
	 */
	public List<DrivePojo> getDrives() {
		return drives;
	}

	/**
	 * @param drives the drives to set
	 */
	public void setDrives(List<DrivePojo> drives) {
		this.drives = drives;
	}

	@Override
	public int hashCode() {
		return Objects.hash(drives);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PbpPojo)) {
			return false;
		}
		PbpPojo other = (PbpPojo) obj;
		return Objects.equals(drives, other.drives);
	}

	@Override
	public String toString() {
		return "PbpPojo [drives=" + drives + "]";
	}

}
