package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.lang.reflect.Field;

import javax.persistence.EntityManager;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;

import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class EntityManagerUtils {

	private final EntityManager entityManager;
	
	@Autowired
	EntityManagerUtils(EntityManager entityManager){
		this.entityManager = entityManager;
	}
	
	public Object unwrapLazyLoad(Object persistedObject) {
		return Hibernate.unproxy(persistedObject);
	}
	
	@SuppressWarnings("unchecked")
	public Object fetchPersisted(Object dto, @SuppressWarnings("rawtypes") Class entity) {
		Object id = getEntityId(dto);
		if (id == null) {
			return dto;
		} else {
			Object persistedObject = entityManager.find(entity, id);
			if (persistedObject == null) {
				throw new IllegalArgumentException(String.format("Invalid object OID: %s", id));
			}
			return persistedObject;
		}
	}
	
	private Object getEntityId(@NotNull Object dto) {
		for (Field field : dto.getClass().getSuperclass().getDeclaredFields()) {
			if(field.getAnnotation(Id.class) != null) {
				try {
					field.setAccessible(true);
					return field.get(dto);
				} catch (IllegalAccessException e) {
					throw new IllegalArgumentException(e);
				}
			}
		}
		throw new IllegalArgumentException("DTO lacks annotated @ID field");
	}
	
	@SuppressWarnings("unchecked")
	public Object fetchPersistedModel(Object dao, @SuppressWarnings("rawtypes") Class entity) {
		Object id = getModelId(dao);
		if (id == null) {
			return dao;
		} else {
			return entityManager.find(entity, id);
		}
	}
	
	private Object getModelId(@NotNull Object dto) {
		for (Field field : dto.getClass().getSuperclass().getDeclaredFields()) {
			if(field.getAnnotation(Id.class) != null) {
				try {
					field.setAccessible(true);
					return field.get(dto);
				} catch (IllegalAccessException e) {
					throw new IllegalArgumentException(e);
				}
			}
		}
		return null;
	}
}

