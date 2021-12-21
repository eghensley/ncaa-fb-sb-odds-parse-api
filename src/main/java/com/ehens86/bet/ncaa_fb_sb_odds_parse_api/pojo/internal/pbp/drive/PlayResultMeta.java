package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.pbp.drive;

import java.util.Objects;

public class PlayResultMeta {
	private Integer drive;
	private Integer play;
	private Integer points;

	public PlayResultMeta(Integer drive, Integer play, Integer points) {
		super();
		this.drive = drive;
		this.play = play;
		this.points = points;
	}

	/**
	 * @return the drive
	 */
	public Integer getDrive() {
		return drive;
	}

	/**
	 * @param drive the drive to set
	 */
	public void setDrive(Integer drive) {
		this.drive = drive;
	}

	/**
	 * @return the play
	 */
	public Integer getPlay() {
		return play;
	}

	/**
	 * @param play the play to set
	 */
	public void setPlay(Integer play) {
		this.play = play;
	}

	/**
	 * @return the points
	 */
	public Integer getPoints() {
		return points;
	}

	/**
	 * @param points the points to set
	 */
	public void setPoints(Integer points) {
		this.points = points;
	}

	@Override
	public int hashCode() {
		return Objects.hash(drive, play, points);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PlayResultMeta)) {
			return false;
		}
		PlayResultMeta other = (PlayResultMeta) obj;
		return Objects.equals(drive, other.drive) && Objects.equals(play, other.play)
				&& Objects.equals(points, other.points);
	}

	@Override
	public String toString() {
		return "PlayResultMeta [drive=" + drive + ", play=" + play + ", points=" + points + "]";
	}

}
