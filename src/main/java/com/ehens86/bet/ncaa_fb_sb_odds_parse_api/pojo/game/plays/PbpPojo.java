package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays;

import java.util.ArrayList;
import java.util.List;

public class PbpPojo {
	private List<DrivePojo> drives;
	
	public PbpPojo() {
		this.drives = new ArrayList<DrivePojo>();
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
	
	
	
}
