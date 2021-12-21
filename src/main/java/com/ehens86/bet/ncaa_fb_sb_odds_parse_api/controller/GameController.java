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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.GetResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.GameService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@CrossOrigin()
@RestController
@RequestMapping("game")
@Api(value = "Game APIs")
public class GameController {
	private static final Logger LOG = Logger.getLogger(ParseController.class.toString());

	@Autowired
	GameService gameService;

	@Value("${credentials.admin.password}")
	private String loginKey;

	private static String loginFailed = "Admin login failed";

	@ApiOperation(value = "Get game object by ID")
	@GetMapping("/{gameId}")
	public ResponseEntity<GetResponse> getGame(
			@RequestHeader(value = "password", required = true) String attemptedPassword,
			@RequestHeader(value = "debug", required = false) String debug,
			@RequestHeader(value = "stack", required = false) String stack, @PathVariable("gameId") String gameId) {

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
			GetResponse response = gameService.fetchGame(gameId);
			ThreadContext.remove(NcaaConstants.CONTEXT_DEBUG_KEY);
			ThreadContext.remove(NcaaConstants.CONTEXT_STACK_KEY);
			return new ResponseEntity<>(response, response.getStatus());
		} else {
			String errorMsg = loginFailed;
			LOG.log(Level.WARNING, errorMsg);
			GetResponse response = new GetResponse(0, HttpStatus.FORBIDDEN, errorMsg);
			ThreadContext.remove(NcaaConstants.CONTEXT_DEBUG_KEY);
			ThreadContext.remove(NcaaConstants.CONTEXT_STACK_KEY);
			return new ResponseEntity<>(response, response.getStatus());
		}
	}
}
