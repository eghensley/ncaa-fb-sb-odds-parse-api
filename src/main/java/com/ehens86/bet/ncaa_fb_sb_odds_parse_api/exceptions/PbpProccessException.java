package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.exceptions;

public class PbpProccessException extends RuntimeException {
	/**
	 * 
	 */
	private static final long serialVersionUID = 2430555056775846072L;

	public PbpProccessException(String errorMessage, Throwable err) {
		super(errorMessage, err);
	}
}
