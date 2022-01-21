package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.GameData;

@Repository
public interface GameRepository extends JpaRepository<GameData, String> {
	Optional<GameData> findByNcaaGameId(String gameId);

	Optional<List<GameData>> findByWeekAndSeason(Integer week, Integer season);

}
