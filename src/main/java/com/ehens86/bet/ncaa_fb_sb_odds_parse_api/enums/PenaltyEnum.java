package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums;

public enum PenaltyEnum {
	PASS_INTERFERENCE(true, 15),
	FALSE_START(true, 5),
	PERSONAL_FOUL(false, 15),
	KICKOFF_OUT_OF_BOUNDS(false, 10),
	HOLDING(false, 10),
	OFFSIDE(true, 5),
	US(true, 15),
	ILLEGAL_PARTICIPATION(true, 15),
	INTENTIONAL_GROUNDING(false, 10),
	DELAY_OF_GAME(true, 5),
	UNSPORTSMANLIKE_CONDUCT(false, 15),
	ILLEGAL_FORMATION(true, 5),
	ROUGHING_THE_PASSER(false, 15),
	ILLEGAL_SHIFT(true, 5),
	CHOP_BLOCK(false, 15),
	ILLEGAL_SUBSTITUTION(true, 5),
	UNNECESSARY_ROUGHNESS(false, 15),
	ILLEGAL_USE_OF_THE_HANDS(false, 5),
	INELIGIBLE_RECEIVER_DOWNFIELD(false, 5),
	ILLEGAL_BLOCK_IN_BACK(false, 10),
	FACE_MASK(false, 15),
	SUBSTITUTION_INFRACTION(true, 5),
	TOO_MANY_MEN_ON_THE_FIELD(true, 5),
	ILLEGAL_BLOCK(false, 10),
	BLOCK_BELOW_THE_WAIST(false, 15),
	TARGETING(false, 15),
	ROUGHING_THE_KICKER(true, 15),
	ILLEGAL_MOTION(true, 5),
	ILLEGAL_FORWARD_PASS(true, 5),
	FAIR_CATCH_INTERFERENCE(false, 15),
	ILLEGAL_PROCEDURE(true, 5),
	ENCROACHMENT(true, 5),
	ILLEGAL_SNAP(true, 5),
	HL(false, 10),
	RUNNING_INTO_THE_KICKER(true, 5),
	DO(true, 5),
	HD(false, 5),
	OF(false, 5),
	PLAYER_DISQUALIFICATION(false, 15),
	KICK_CATCHING_INTERFERENCE(false, 15)
	;

	private boolean noPlay;
	private Integer yards;
	
	PenaltyEnum(boolean noPlay, Integer yards) {
		this.noPlay = noPlay;
		this.yards = yards;
	}

	/**
	 * @return the noPlay
	 */
	public boolean getNoPlay() {
		return noPlay;
	}

	/**
	 * @return the yards
	 */
	public Integer getYards() {
		return yards;
	}

}
