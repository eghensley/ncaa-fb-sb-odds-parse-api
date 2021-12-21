package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;


@Repository
public interface TeamRepository extends JpaRepository<TeamData, String> {
	Optional<TeamData> findByNcaaTeamId(String ncaaTeamId);

}
