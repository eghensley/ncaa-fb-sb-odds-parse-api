package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
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

	public TeamService(TeamRepository teamRepo, MappingUtils mappingUtils) {
		this.teamRepo = teamRepo;
		this.mappingUtils = mappingUtils;
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
				LoggingUtils.logInfo(logStr);
				return new GetResponse(noInfoFound, null, logStr);
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Team fetch failed for ID: %s", teamId));
			return new GetResponse(noInfoFound, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	public Map<String, TeamData> compileTeamMap(String homeTeamId, String awayTeamId) {
		try {
			Map<String, TeamData> teamMap = new HashMap<>();
			teamMap.put(homeTeamId, retrieveTeam(homeTeamId));
			teamMap.put(awayTeamId, retrieveTeam(awayTeamId));
			return teamMap;
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Compile Team Map failed for Home ID: %s | Away ID: %s",
					homeTeamId, awayTeamId));
			return new HashMap<>();
		}
	}

	private TeamData retrieveTeam(String teamId) {
		try {
			Optional<TeamData> teamDataOpt = teamRepo.findByNcaaTeamId(teamId);
			if (teamDataOpt.isPresent()) {
				return teamDataOpt.get();
			} else {
				throw new IllegalArgumentException(String.format("Team not found: %s", teamId));
			}
		} catch (Exception e) {
			LoggingUtils.logException(e, String.format("ERROR: Retrieve Team failed for ID: %s", teamId));
			return null;
		}
	}

	@Transactional
	public TeamData addTeam(TeamPojo parsedTeam, List<TeamDto> extractedTeams) {
		TeamData team;

		String newTeamLogInfoStrFormat = "Team found to be added: %s - %s";
		String existingTeamLogInfoStrFormat = "Team already exists: %s - %s";
		Optional<TeamData> teamOpt = teamRepo.findByNcaaTeamId(parsedTeam.getNcaaTeamId());

		if (teamOpt.isPresent()) {
			team = teamOpt.get();
			LoggingUtils
					.logInfo(String.format(existingTeamLogInfoStrFormat, team.getTeamName(), team.getTeamNickname()));
		} else {
			team = (TeamData) mappingUtils.mapToDto(parsedTeam, TeamData.class);
			LoggingUtils.logInfo(String.format(newTeamLogInfoStrFormat, team.getTeamName(), team.getTeamNickname()));
			teamRepo.save(team);

			TeamDto teamDto = (TeamDto) mappingUtils.mapToDto(parsedTeam, TeamDto.class);
			extractedTeams.add(teamDto);
		}
		return team;
	}

}
