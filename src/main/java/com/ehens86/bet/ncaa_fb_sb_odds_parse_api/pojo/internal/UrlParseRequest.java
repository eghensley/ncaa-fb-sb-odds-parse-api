package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import com.gargoylesoftware.htmlunit.html.HtmlPage;

public class UrlParseRequest {

	private HtmlPage page;
	private String errorStr;
	private boolean success;
	
	public UrlParseRequest() {
		
	}
	
	public UrlParseRequest(HtmlPage page, String errorStr, boolean success) {
		this.page = page;
		this.errorStr = errorStr;
		this.success = success;
	}

	/**
	 * @return the page
	 */
	public HtmlPage getPage() {
		return page;
	}

	/**
	 * @param page the page to set
	 */
	public void setPage(HtmlPage page) {
		this.page = page;
	}

	/**
	 * @return the errorStr
	 */
	public String getErrorStr() {
		return errorStr;
	}

	/**
	 * @param errorStr the errorStr to set
	 */
	public void setErrorStr(String errorStr) {
		this.errorStr = errorStr;
	}

	/**
	 * @return the success
	 */
	public boolean getSuccess() {
		return success;
	}

	/**
	 * @param success the success to set
	 */
	public void setSuccess(boolean success) {
		this.success = success;
	}
	
	
	
}