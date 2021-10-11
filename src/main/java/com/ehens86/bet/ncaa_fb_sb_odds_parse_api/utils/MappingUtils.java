package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;

@Service
public class MappingUtils {

	@SuppressWarnings("unused")
	private static final ObjectMapper mapper = new ObjectMapper();
	private static final ModelMapper modelMapper = new ModelMapper();
	
	private final EntityManagerUtils entityManagerUtils;
	
	public MappingUtils(EntityManagerUtils entityManagerUtils) {
		this.entityManagerUtils = entityManagerUtils;
	}
	
	@SuppressWarnings("unchecked")
	public Object mapToDto(Object entity, @SuppressWarnings("rawtypes") Class dto) {
		return modelMapper.map(entity, dto);
	}
	
	@SuppressWarnings("unchecked")
	public Object mapToModel(Object dto, @SuppressWarnings("rawtypes") Class entity) {
		Object model = entityManagerUtils.fetchPersisted(dto, entity);
		modelMapper.map(dto, model);
		return modelMapper.map(model, entity);
	}
	
	public Object mapModelToModel(Object dto) {
		Object model = entityManagerUtils.fetchPersistedModel(dto, dto.getClass());
		modelMapper.map(model, dto);
		return model;
	}
}
