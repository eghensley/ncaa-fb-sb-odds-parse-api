package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayerData;

@Repository
public interface PlayerRepository extends JpaRepository<PlayerData, String> {

	@Query(value = "SELECT * FROM ncaafb.player p \n" + 
			"WHERE p.first_name=:firstName \n" + 
			"AND p.last_name=:lastName", nativeQuery = true)
	Optional<PlayerData> findPlayerByNames(@Param("firstName") String firstName, @Param("lastName") String lastName);
}
