package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal;

import java.util.Date;
import java.util.Objects;

import org.springframework.http.HttpStatus;

public class GetResponse extends GeneralApiResponse {
	private Integer itemsFound;
	private Object payload;

	public GetResponse() {
		this.timestamp = new Date();
	}

	public GetResponse(Integer itemsFound, Object payload, String errorMsg) {
		this.itemsFound = itemsFound;
		this.status = HttpStatus.OK;
		this.timestamp = new Date();
		this.payload = payload;
		this.errorMsg = errorMsg;
	}
	
	public GetResponse(Integer itemsFound, HttpStatus status,
			String errorMsg) {
		this.itemsFound = itemsFound;
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + Objects.hash(itemsFound, payload);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (!(obj instanceof GetResponse)) {
			return false;
		}
		GetResponse other = (GetResponse) obj;
		return Objects.equals(itemsFound, other.itemsFound) && Objects.equals(payload, other.payload);
	}

	@Override
	public String toString() {
		return "GetResponse [itemsFound=" + itemsFound + ", payload=" + payload + "]";
	}



}