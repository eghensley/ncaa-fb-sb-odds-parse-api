package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import java.util.Date;
import java.util.Objects;

import org.springframework.http.HttpStatus;

public class GeneralApiResponse {

	protected HttpStatus status;
	protected Date timestamp;
	protected String errorMsg;
	
	public GeneralApiResponse() {
		
	}
	
	public GeneralApiResponse(HttpStatus status, String errorMsg) {
		this.status = status;
		this.timestamp = new Date();
		this.errorMsg = errorMsg;
	}

	/**
	 * @return the status
	 */
	public HttpStatus getStatus() {
		return status;
	}

	/**
	 * @param status the status to set
	 */
	public void setStatus(HttpStatus status) {
		this.status = status;
	}

	/**
	 * @return the timestamp
	 */
	public Date getTimestamp() {
		return timestamp;
	}

	/**
	 * @param timestamp the timestamp to set
	 */
	public void setTimestamp(Date timestamp) {
		this.timestamp = timestamp;
	}

	/**
	 * @return the errorMsg
	 */
	public String getErrorMsg() {
		return errorMsg;
	}

	/**
	 * @param errorMsg the errorMsg to set
	 */
	public void setErrorMsg(String errorMsg) {
		this.errorMsg = errorMsg;
	}

	@Override
	public int hashCode() {
		return Objects.hash(errorMsg, status, timestamp);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GeneralApiResponse)) {
			return false;
		}
		GeneralApiResponse other = (GeneralApiResponse) obj;
		return Objects.equals(errorMsg, other.errorMsg) && status == other.status
				&& Objects.equals(timestamp, other.timestamp);
	}

	@Override
	public String toString() {
		return "GeneralApiResponse [status=" + status + ", timestamp=" + timestamp + ", errorMsg=" + errorMsg + "]";
	}
	
	
	
}