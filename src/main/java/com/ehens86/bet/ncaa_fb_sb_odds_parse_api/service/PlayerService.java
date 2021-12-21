package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayerData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.PlayerRepository;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;

@Service
public class PlayerService {

	private final PlayerRepository playerRepo;
	private final MappingUtils mappingUtils;
	private final LoggingUtils loggingUtils;
	
	public PlayerService(PlayerRepository playerRepo, MappingUtils mappingUtils, LoggingUtils loggingUtils) {
		this.playerRepo = playerRepo;
		this.mappingUtils = mappingUtils;
		this.loggingUtils = loggingUtils;
	}
	
	@Transactional
	public PlayerData resolvePlayer(String firstName, String lastName, TeamData team) {
		PlayerData player;
		
		Optional<PlayerData> playerOpt = playerRepo.findPlayerByNames(firstName, lastName);
		if (playerOpt.isEmpty()) {
			player = new PlayerData();
			player.setFirstName(firstName);
			player.setLastName(lastName);
			player.setTeam(team);
			playerRepo.save(player);
		} else {
			player = playerOpt.get();
			if (!player.getTeam().getNcaaTeamId().equals(team.getNcaaTeamId())) {
				throw new IllegalArgumentException(String.format("Player %s %s already exists for team %s", firstName, lastName, player.getTeam().getTeamNameSeo()));
			}
		}
		return player;
	}
}
