package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.controller;

import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.logging.log4j.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.ParseService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.PbpService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@CrossOrigin()
@RestController
@RequestMapping("parse")
@Api(value = "Parsing System")
public class ParseController {

	private static final Logger LOG = Logger.getLogger(ParseController.class.toString());

	@Autowired
	ParseService gamesService;
	@Autowired
	PbpService pbpService;

	@Value("${credentials.admin.password}")
	private String loginKey;

	private static String loginFailed = "Admin login failed";

	@ApiOperation(value = "Add Teams From FCS Week")
	@PostMapping("games/fcs/teams")
	public ResponseEntity<ParseResponse> addTeams(
			@RequestHeader(value = "password", required = true) String attemptedPassword,
			@RequestHeader(value = "debug", required = false) String debug,
			@RequestHeader(value = "stack", required = false) String stack, @RequestBody ParseRequest request) {

		if (Objects.nonNull(debug) && NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equalsIgnoreCase(debug)) {
			ThreadContext.put(NcaaConstants.CONTEXT_DEBUG_KEY, NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE);
		} else {
			ThreadContext.put(NcaaConstants.CONTEXT_DEBUG_KEY, NcaaConstants.CONTEXT_DEBUG_VALUE_FALSE);
		}
		if (Objects.nonNull(stack) && NcaaConstants.CONTEXT_STACK_VALUE_TRUE.equalsIgnoreCase(stack)) {
			ThreadContext.put(NcaaConstants.CONTEXT_STACK_KEY, NcaaConstants.CONTEXT_STACK_VALUE_TRUE);
		} else {
			ThreadContext.put(NcaaConstants.CONTEXT_STACK_KEY, NcaaConstants.CONTEXT_STACK_VALUE_FALSE);
		}
		final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
		String methodStartInfo = String.format(" -- Request Received [%s]", ste[1].getMethodName());
		LOG.log(Level.INFO, methodStartInfo);

		if (loginKey.equals(attemptedPassword)) {
			ParseResponse response = gamesService.addTeamsFromFcsWeek(request);
			ThreadContext.remove(NcaaConstants.CONTEXT_DEBUG_KEY);
			ThreadContext.remove(NcaaConstants.CONTEXT_STACK_KEY);
			return new ResponseEntity<>(response, response.getStatus());
		} else {
			String errorMsg = loginFailed;
			LOG.log(Level.WARNING, errorMsg);
			ParseResponse response = new ParseResponse(null, 1, 0, HttpStatus.FORBIDDEN, errorMsg);
			ThreadContext.remove(NcaaConstants.CONTEXT_DEBUG_KEY);
			ThreadContext.remove(NcaaConstants.CONTEXT_STACK_KEY);
			return new ResponseEntity<>(response, response.getStatus());
		}
	}

	@ApiOperation(value = "Parse PBP Stats from Game")
	@PostMapping("games/fcs/pbp")
	public ResponseEntity<ParseResponse> addPbp(
			@RequestHeader(value = "password", required = true) String attemptedPassword,
			@RequestHeader(value = "debug", required = false) String debug,
			@RequestHeader(value = "stack", required = false) String stack, @RequestBody ParseRequest request) {

		if (Objects.nonNull(debug) && NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE.equalsIgnoreCase(debug)) {
			ThreadContext.put(NcaaConstants.CONTEXT_DEBUG_KEY, NcaaConstants.CONTEXT_DEBUG_VALUE_TRUE);
		} else {
			ThreadContext.put(NcaaConstants.CONTEXT_DEBUG_KEY, NcaaConstants.CONTEXT_DEBUG_VALUE_FALSE);
		}
		if (Objects.nonNull(stack) && NcaaConstants.CONTEXT_STACK_VALUE_TRUE.equalsIgnoreCase(stack)) {
			ThreadContext.put(NcaaConstants.CONTEXT_STACK_KEY, NcaaConstants.CONTEXT_STACK_VALUE_TRUE);
		} else {
			ThreadContext.put(NcaaConstants.CONTEXT_STACK_KEY, NcaaConstants.CONTEXT_STACK_VALUE_FALSE);
		}
		final StackTraceElement[] ste = Thread.currentThread().getStackTrace();
		String methodStartInfo = String.format(" -- Request Received [%s] [GameId: %s]", ste[1].getMethodName(),
				request.getGameId());
		LOG.log(Level.INFO, methodStartInfo);

		if (loginKey.equals(attemptedPassword)) {
			ParseResponse response = pbpService.addPbpData(request);
			ThreadContext.remove(NcaaConstants.CONTEXT_DEBUG_KEY);
			ThreadContext.remove(NcaaConstants.CONTEXT_STACK_KEY);
			return new ResponseEntity<>(response, response.getStatus());
		} else {
			String errorMsg = loginFailed;
			LOG.log(Level.WARNING, errorMsg);
			ParseResponse response = new ParseResponse(null, 1, 0, HttpStatus.FORBIDDEN, errorMsg);
			ThreadContext.remove(NcaaConstants.CONTEXT_DEBUG_KEY);
			ThreadContext.remove(NcaaConstants.CONTEXT_STACK_KEY);
			return new ResponseEntity<>(response, response.getStatus());
		}
	}

}