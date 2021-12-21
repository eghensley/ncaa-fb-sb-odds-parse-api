package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.dto.TeamDto;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.TeamPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.GetResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.TeamRepository;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;

@Service
public class TeamService {

	private final TeamRepository teamRepo;
	private final MappingUtils mappingUtils;
	private final LoggingUtils loggingUtils;

	public TeamService(TeamRepository teamRepo, MappingUtils mappingUtils, LoggingUtils loggingUtils) {
		this.teamRepo = teamRepo;
		this.mappingUtils = mappingUtils;
		this.loggingUtils = loggingUtils;
	}

	public GetResponse fetchTeam(String teamId) {
		Integer noInfoFound = 0;
		Integer infoFound = 1;
		TeamDto teamDto;
		try {
			Optional<TeamData> teamDataOpt = teamRepo.findByNcaaTeamId(teamId);
			if (teamDataOpt.isPresent()) {
				teamDto = (TeamDto) mappingUtils.mapToDto(teamDataOpt.get(), TeamDto.class);
				return new GetResponse(infoFound, teamDto, null);
			} else {
				String logStr = String.format("No team found for ID: %s", teamId);
				loggingUtils.logInfo(logStr);
				return new GetResponse(noInfoFound, null, logStr);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, String.format("ERROR: Team fetch failed for ID: %s", teamId));
			return new GetResponse(noInfoFound, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	@Transactional
	public TeamData addTeam(TeamPojo parsedTeam, List<TeamDto> extractedTeams) {
		TeamData team;

		String newTeamLogInfoStrFormat = "Team found to be added: %s - %s";
		String existingTeamLogInfoStrFormat = "Team already exists: %s - %s";

		if (Boolean.FALSE.equals(teamRepo.findByNcaaTeamId(parsedTeam.getNcaaTeamId()).isPresent())) {
			team = (TeamData) mappingUtils.mapToDto(parsedTeam, TeamData.class);
			loggingUtils.logInfo(String.format(newTeamLogInfoStrFormat, team.getTeamName(), team.getTeamNickname()));
			teamRepo.save(team);
			
			TeamDto teamDto = (TeamDto) mappingUtils.mapToDto(parsedTeam, TeamDto.class);
			extractedTeams.add(teamDto);
		} else {
			team = teamRepo.findByNcaaTeamId(parsedTeam.getNcaaTeamId()).get();
			loggingUtils
					.logInfo(String.format(existingTeamLogInfoStrFormat, team.getTeamName(), team.getTeamNickname()));
		}
		return team;
	}

}
