package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "VENUE")
public class VenueData extends OidAuditEntity implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6928155903298979975L;
	@Column(name = "CITY", nullable = false)
	private String city;
	@Column(name = "NAME", nullable = false)
	private String name;
	@Column(name = "STATE", nullable = false)
	private String state;

	public VenueData() {

	}

	/**
	 * @return the city
	 */
	public String getCity() {
		return city;
	}

	/**
	 * @param city the city to set
	 */
	public void setCity(String city) {
		this.city = city;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the state
	 */
	public String getState() {
		return state;
	}

	/**
	 * @param state the state to set
	 */
	public void setState(String state) {
		this.state = state;
	}

	@Override
	public int hashCode() {
		return Objects.hash(city, name, state);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof VenueData)) {
			return false;
		}
		VenueData other = (VenueData) obj;
		return Objects.equals(city, other.city) && Objects.equals(name, other.name)
				&& Objects.equals(state, other.state);
	}

	@Override
	public String toString() {
		return "VenueData [city=" + city + ", name=" + name + ", state=" + state + "]";
	}

}
