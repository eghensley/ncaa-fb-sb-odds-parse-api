package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain;

import java.util.Date;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@EntityListeners(AuditingEntityListener.class)
@MappedSuperclass
public class BaseAuditEntity {

	@Version
	@Column(name = "VERSION", nullable = false)
	private Integer version;

	@CreatedBy
	@Column(name = "CREATE_USER", nullable = false)
	private String createUser;

	@CreatedDate
	@Column(name = "CREATE_TS", nullable = false)
	private Date createTs;

	@LastModifiedBy
	@Column(name = "UPDATE_USER", nullable = true)
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TS", nullable = true)
	private Date updateTs;

	public BaseAuditEntity() {
		// Base constructor
	}

	public Integer getVersion() {
		return version;
	}

	public void setVersion(Integer version) {
		this.version = version;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Date getCreateTs() {
		return createTs;
	}

	public void setCreateTs(Date createTs) {
		this.createTs = createTs;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Date getUpdateTs() {
		return updateTs;
	}

	public void setUpdateTs(Date updateTs) {
		this.updateTs = updateTs;
	}

	@Override
	public int hashCode() {
		return Objects.hash(createTs, createUser, updateTs, updateUser, version);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BaseAuditEntity)) {
			return false;
		}
		BaseAuditEntity other = (BaseAuditEntity) obj;
		return Objects.equals(createTs, other.createTs) && Objects.equals(createUser, other.createUser)
				&& Objects.equals(updateTs, other.updateTs)
				&& Objects.equals(updateUser, other.updateUser) && Objects.equals(version, other.version);
	}

}
