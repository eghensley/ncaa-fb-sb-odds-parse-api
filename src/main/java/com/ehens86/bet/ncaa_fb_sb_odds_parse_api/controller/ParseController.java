package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.controller;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.GamesService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.OddsService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@CrossOrigin()
@RestController
@RequestMapping("parse")
@Api(value = "Parsing System")
public class ParseController {

	@Autowired
	OddsService oddsService;

	@Autowired
	GamesService gamesService;
	
	@Value("${credentials.admin.password}")
	private String loginKey;

	private static String loginFailed = "Admin login failed";

	@ApiOperation(value = "Parse game odds")
	@PostMapping("dayOdds")
	public ResponseEntity<ParseResponse> parseDayOdds(
			@RequestHeader(value = "password", required = true) String attemptedPassword) {
		if (loginKey.equals(attemptedPassword)) {
			ParseResponse response = oddsService.parseDayOdds();
			return new ResponseEntity<ParseResponse>(response, response.getStatus());
		} else {
			String errorMsg = loginFailed;
			ParseResponse response = new ParseResponse(null, 1, 0, HttpStatus.FORBIDDEN, errorMsg);
			return new ResponseEntity<ParseResponse>(response, response.getStatus());
		}
	}

	@ApiOperation(value = "Parse week FCS games")
	@PostMapping("games/fcs")
	public ResponseEntity<ParseResponse> parseFcsGames (
			@RequestHeader(value = "password", required = true) String attemptedPassword) {
		if (loginKey.equals(attemptedPassword)) {
			ParseResponse response = gamesService.parseWeekFcsGames();
			return new ResponseEntity<ParseResponse>(response, response.getStatus());
		} else {
			String errorMsg = loginFailed;
			ParseResponse response = new ParseResponse(null, 1, 0, HttpStatus.FORBIDDEN, errorMsg);
			return new ResponseEntity<ParseResponse>(response, response.getStatus());
		}
	}

}