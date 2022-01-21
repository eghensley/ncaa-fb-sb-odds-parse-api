package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.exceptions;

public class PbpValidationException extends IllegalArgumentException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -366069689033508313L;

	public PbpValidationException(String errorMessage) {
		super(errorMessage);
	}
}
