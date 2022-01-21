package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.io.IOException;
import java.net.MalformedURLException;

import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.UrlParseRequest;
import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

public final class UrlUtils {

	private static final String S_URL_PARSING_ERROR_S = "%s url parsing error: %s";

	// private static static constructor to prevent instantiation
	private UrlUtils() {
		throw new UnsupportedOperationException();
	}

	public static UrlParseRequest parse(String baseUrl) {
		HtmlPage page = null;

		WebClient client = new WebClient();
		client.getOptions().setCssEnabled(false);
		client.getOptions().setJavaScriptEnabled(false);

		try {
			page = client.getPage(baseUrl);
		} catch (FailingHttpStatusCodeException e) {
			client.close();
			LoggingUtils.logException(e, String.format(S_URL_PARSING_ERROR_S, FailingHttpStatusCodeException.class,
					e.getLocalizedMessage()));
			return new UrlParseRequest(page, e.getLocalizedMessage(), false);
		} catch (MalformedURLException e) {
			client.close();
			LoggingUtils.logException(e,
					String.format(S_URL_PARSING_ERROR_S, MalformedURLException.class, e.getLocalizedMessage()));
			return new UrlParseRequest(page, e.getLocalizedMessage(), false);
		} catch (IOException e) {
			client.close();
			LoggingUtils.logException(e,
					String.format(S_URL_PARSING_ERROR_S, IOException.class, e.getLocalizedMessage()));
			return new UrlParseRequest(page, e.getLocalizedMessage(), false);
		}
		client.close();
		return new UrlParseRequest(page, null, true);
	}

	public static Object get(String uri, @SuppressWarnings("rawtypes") Class target) {
		try {
			RestTemplate restTemplate = new RestTemplate();
			@SuppressWarnings("unchecked")
			Object result = restTemplate.getForObject(uri, target);

			return result;
		} catch (Exception e) {
			String errorStr = String.format("ERROR: url parse failed for %s with %s", uri, e.getMessage());
			if (hasCause(e, HttpClientErrorException.class)) {
				throw e;
			}
			LoggingUtils.logException(e, errorStr);
			throw new IllegalArgumentException(errorStr);
		}
	}

	static boolean hasCause(Throwable e, Class<? extends Throwable> cl) {
		return cl.isInstance(e) || e.getCause() != null && hasCause(e.getCause(), cl);
	}

}
