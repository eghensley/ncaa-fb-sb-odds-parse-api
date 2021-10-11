package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.UrlParseRequest;
import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

@Service
public class UrlUtils {

	private Logger LOG = Logger.getLogger(UrlUtils.class.toString());


	public UrlParseRequest parse(String baseUrl) {
		HtmlPage page = null;

		WebClient client = new WebClient();
		client.getOptions().setCssEnabled(false);
		client.getOptions().setJavaScriptEnabled(false);

		try {
			page = client.getPage(baseUrl);
		} catch (FailingHttpStatusCodeException e) {
			LOG.log(Level.WARNING, String.format("%s url parsing error: %s", FailingHttpStatusCodeException.class, e.getLocalizedMessage()));
			client.close();
			return new UrlParseRequest(page, e.getLocalizedMessage(), false);
		} catch (MalformedURLException e) {
			LOG.log(Level.WARNING, String.format("%s url parsing error: %s", MalformedURLException.class, e.getLocalizedMessage()));
			client.close();
			return new UrlParseRequest(page, e.getLocalizedMessage(), false);
		} catch (IOException e) {
			LOG.log(Level.WARNING, String.format("%s url parsing error: %s", IOException.class, e.getLocalizedMessage()));
			client.close();
			return new UrlParseRequest(page, e.getLocalizedMessage(), false);
		}
		// System.out.println(page.asXml());
		client.close();
		return new UrlParseRequest(page, null, true);
	}
	
	public Object get(String uri, @SuppressWarnings("rawtypes") Class target) {
		try {
	    //final String uri = "http://localhost:8080/springrestexample/employees.xml";

	    RestTemplate restTemplate = new RestTemplate();
	    @SuppressWarnings("unchecked")
		Object result = restTemplate.getForObject(uri, target);

	    return result;
		} catch (Exception e) {
			String errorStr = String.format("ERROR: url parse failed for %s with %s", uri, e.getMessage());
			LOG.log(Level.SEVERE, errorStr);
			throw new IllegalArgumentException(errorStr);
		}
	}

};
