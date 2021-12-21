package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.logging.log4j.ThreadContext;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.constants.NcaaConstants;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.exceptions.PbpProccessException;

@Service
public class LoggingUtils {

	public void logException(Exception e, String input) {

		int depth;
		boolean pbp;
		if (e.getClass().equals(PbpProccessException.class)) {
			depth = 1;
			pbp = true;
		} else {
			depth = 0;
			pbp = false;
		}
		final StackTraceElement[] st = e.getStackTrace();

		String className = st[depth].getClassName();
		String[] splitClassName = className.split("\\.");
		int classNameLen = splitClassName.length;
		String classNameShort = splitClassName[classNameLen - 1];
		String logName = String.format("%s.%s", classNameShort, st[depth].getMethodName());
		Logger log = Logger.getLogger(logName);



		if (Boolean.FALSE.equals(pbp)) {
			String inputLog = String.format("INPUT: %s", input);
			log.log(Level.INFO, inputLog);
			if (NcaaConstants.CONTEXT_STACK_VALUE_TRUE.equals(ThreadContext.get(NcaaConstants.CONTEXT_STACK_KEY))) {
				log.log(Level.INFO, e.getMessage(), e);
			} else {
				log.log(Level.SEVERE, String.format("(%s:%s) - %s", st[depth].getFileName(), st[depth].getLineNumber(),
						e.toString()));
			}
			throw new PbpProccessException(String.format("%s -- %s", e.toString(), inputLog), e);
		} else {
			log.log(Level.SEVERE, String.format("(%s:%s) - Propogated to %s", st[depth].getFileName(), st[depth].getLineNumber(),
					st[depth].getMethodName()));
			throw new PbpProccessException(input, e);
		}
		
	}

	public void logInfo(String input) {

		final StackTraceElement[] ste = Thread.currentThread().getStackTrace();

		String className = ste[2].getClassName();
		String[] splitClassName = className.split("\\.");
		int classNameLen = splitClassName.length;
		String classNameShort = splitClassName[classNameLen - 1];
		String logName = String.format("%s.%s", classNameShort, ste[2].getMethodName());

		Logger log = Logger.getLogger(logName);

		log.log(Level.INFO, input);

	}
}
