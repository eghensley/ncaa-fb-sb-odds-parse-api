package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import java.util.Date;

import org.springframework.http.HttpStatus;

public class ParseResponse extends GeneralApiResponse {
	private ParseRequest request;
	private Integer itemsFound;
	private Integer itemsCompleted;
	private Object payload;
	
	public ParseResponse() {
		this.timestamp = new Date();
	}
	
	public ParseResponse(ParseRequest request) {
		this.timestamp = new Date();
		this.request = request;
		this.itemsFound = 0;
		this.itemsCompleted = 0;
	}
	
	public ParseResponse(ParseRequest request, Integer itemsFound, Integer itemsCompleted, HttpStatus status, String errorMsg) {
		this.request = request;
		this.itemsFound = itemsFound;
		this.itemsCompleted = itemsCompleted;
		this.status = status;
		this.timestamp = new Date();
		this.errorMsg = errorMsg;
	}

	public void addResponseMsg(HttpStatus status, String errorMsg) {
		this.status = status;
		this.errorMsg = errorMsg;
		this.timestamp = new Date();
	}


	/**
	 * @return the request
	 */
	public ParseRequest getRequest() {
		return request;
	}

	/**
	 * @param request the request to set
	 */
	public void setRequest(ParseRequest request) {
		this.request = request;
	}

	/**
	 * @return the itemsFound
	 */
	public Integer getItemsFound() {
		return itemsFound;
	}

	/**
	 * @param itemsFound the itemsFound to set
	 */
	public void setItemsFound(Integer itemsFound) {
		this.itemsFound = itemsFound;
	}

	/**
	 * @return the itemsCompleted
	 */
	public Integer getItemsCompleted() {
		return itemsCompleted;
	}

	/**
	 * @param itemsCompleted the itemsCompleted to set
	 */
	public void setItemsCompleted(Integer itemsCompleted) {
		this.itemsCompleted = itemsCompleted;
	}

	/**
	 * @return the payload
	 */
	public Object getPayload() {
		return payload;
	}

	/**
	 * @param payload the payload to set
	 */
	public void setPayload(Object payload) {
		this.payload = payload;
	}
	
}