package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.DriveData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.GameData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.PlayerData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.TeamPlayStatData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.defense.StatDefenseData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatPassingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatReceivingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.offense.StatRushingData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.penalty.StatPenaltyData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickoffData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatKickoffReturnData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatPuntData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.domain.stats.specialteam.StatPuntReturnData;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.KickTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.StatDefSpecTeam;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.StatPosNegEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.enums.StatTypeEnum;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.GamePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.DrivePojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.plays.PlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.PbpPlayerStatPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.PlayerStatPenaltyPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatPassingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatReceivingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.offense.pbp.PbpPlayerStatRushingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.pbp.PlayByPlayPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.repository.TeamRepository;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service.casablanca.PlayByPlayService;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.LoggingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.UrlUtils;

@Service
public class PbpService {
	private final UrlUtils urlUtils;
	private final PlayByPlayService playByPlayService;
	private final GameService gameService;
	private final LoggingUtils loggingUtils;
	private final MappingUtils mappingUtils;
	private final TeamRepository teamRepo;
	private final PlayerService playerService;

	public PbpService(PlayByPlayService playByPlayService, UrlUtils urlUtils, GameService gameService,
			LoggingUtils loggingUtils, MappingUtils mappingUtils, TeamRepository teamRepo,
			PlayerService playerService) {
		this.playByPlayService = playByPlayService;
		this.urlUtils = urlUtils;
		this.gameService = gameService;
		this.loggingUtils = loggingUtils;
		this.mappingUtils = mappingUtils;
		this.teamRepo = teamRepo;
		this.playerService = playerService;
	}

	public ParseResponse addPbpData(ParseRequest req) {
		String pbpParseCompleteLogStr = "PBP Parse already complete for: %s - %s v %s";

		GameData game;
		Integer infoFound = 0;
		Integer infoCompleted = 0;

		try {

			game = gameService.getGame(req.getGameId().toString());
			if (game.isPbpComplete()) {
				String gamePbpParseComplete = String.format(pbpParseCompleteLogStr, req.getGameId(),
						game.getTeamHome().getTeamNameSeo(), game.getTeamAway().getTeamNameSeo());
				return new ParseResponse(req, 0, 0, HttpStatus.BAD_REQUEST, gamePbpParseComplete);
			}
			infoFound += 1;
			String playByPlayUrl = String.format("https://data.ncaa.com/casablanca/game/%s/pbp.json",
					game.getNcaaGameId());

			PlayByPlayPojo playByPlayRaw = (PlayByPlayPojo) urlUtils.get(playByPlayUrl, PlayByPlayPojo.class);
			GamePojo gamePojo = (GamePojo) mappingUtils.mapToDto(game, GamePojo.class);

			playByPlayService.parsePbP(playByPlayRaw, gamePojo);

			Map<String, TeamData> teamMap = new HashMap<>();
			Optional<TeamData> homeTeamDataOpt = teamRepo.findByNcaaTeamId(gamePojo.getTeamHome().getNcaaTeamId());
			Optional<TeamData> awayTeamDataOpt = teamRepo.findByNcaaTeamId(gamePojo.getTeamAway().getNcaaTeamId());
			if (homeTeamDataOpt.isPresent()) {
				teamMap.put(gamePojo.getTeamHome().getNcaaTeamId(), homeTeamDataOpt.get());
			} else {
				throw new IllegalArgumentException(
						String.format("Team not found: %s", gamePojo.getTeamHome().getNcaaTeamId()));
			}
			if (awayTeamDataOpt.isPresent()) {
				teamMap.put(gamePojo.getTeamAway().getNcaaTeamId(), awayTeamDataOpt.get());
			} else {
				throw new IllegalArgumentException(
						String.format("Team not found: %s", gamePojo.getTeamAway().getNcaaTeamId()));
			}

			Map<String, Set<String>> teamPlayerMap = new HashMap<>();
			teamPlayerMap.put(gamePojo.getTeamHome().getNcaaTeamId(), new HashSet<String>());
			teamPlayerMap.put(gamePojo.getTeamAway().getNcaaTeamId(), new HashSet<String>());
			Map<String, List<PlayerData>> teamPlayerDataMap = new HashMap<>();

			pullPlayerListFromPlays(teamPlayerMap, gamePojo);

			Set<String> intersection = new HashSet<>(teamPlayerMap.get(gamePojo.getTeamHome().getNcaaTeamId()));
			intersection.retainAll(teamPlayerMap.get(gamePojo.getTeamAway().getNcaaTeamId()));
			if (!intersection.isEmpty()) {
				throw new IllegalArgumentException(
						String.format("Player overlap found between teams: %s", intersection));
			}

			Set<String> playTeamIdsx = teamPlayerMap.keySet();
			for (String playTeamId : playTeamIdsx) {
				addTeamPlayers(teamPlayerDataMap, teamPlayerMap, playTeamId, teamMap.get(playTeamId));
			}

			StatPosNegEnum posNeg = StatPosNegEnum.POS;
			StatTypeEnum statType = StatTypeEnum.PLAYER;

//			Hibernate.initialize(game.getDrives());

			Integer homeScore = 0;
			Integer awayScore = 0;

			for (DrivePojo drive : gamePojo.getPlays().getDrives()) {
				DriveData driveData = (DriveData) mappingUtils.mapToDto(drive, DriveData.class);
				driveData.setPlays(new ArrayList<>());
				driveData.setDriveStartPossessionTeam(teamMap.get(drive.getPossessionTeamId()));
				for (PlayPojo play : drive.getDrivePlays()) {
					PlayData playData = (PlayData) mappingUtils.mapToDto(play, PlayData.class);
					playData.setPlayStartPossessionTeam(teamMap.get(play.getPlayStartPossessionTeamId()));
					playData.setPlayResultPossessionTeam(
							teamMap.get(play.getPlayResult().getPlayResultPossessionTeamId()));
					Set<String> playTeamIds = play.getPlayerStat().keySet();
					for (String playTeamId : playTeamIds) {
						TeamPlayStatData teamPlayStatData = addTeamPlayStats(playTeamId, play, posNeg, statType,
								teamPlayerDataMap, teamMap.get(playTeamId));
						playData.addPlayStat(teamPlayStatData);
					}
					driveData.addPlay(playData);
				}
				homeScore = driveData.getDriveResultHomeScore();
				awayScore = driveData.getDriveResultAwayScore();
				game.addDrive(driveData);
			}

			if (game.getHomeScore() != homeScore) {
				throw new IllegalArgumentException("game.getHomeScore() != homeScore");
			}
			if (game.getAwayScore() != awayScore) {
				throw new IllegalArgumentException("game.getAwayScore() != awayScore");
			}

			game.setPbpComplete(true);
//			gameService.saveGame(game);
			// TODO add offensive line
			// TODO add team game summary stats
			// TODO add player game summary stats
			// TODO add indicator features

			infoCompleted += 1;
			return new ParseResponse(req, infoFound, infoCompleted, HttpStatus.OK, "");
		} catch (Exception e) {
			loggingUtils.logException(e, String.format("ERROR: PBP Parse failed for ID: %s", req.getGameId()));
			return new ParseResponse(req, infoFound, infoCompleted, HttpStatus.BAD_REQUEST, e.toString());
		}
	}

	private TeamPlayStatData addTeamPlayStats(String playTeamId, PlayPojo play, StatPosNegEnum posNeg,
			StatTypeEnum statType, Map<String, List<PlayerData>> teamPlayerDataMap, TeamData team) {
		String innerErrorMsg = "Team play stat addition failed - %s";
		String teamNickname = team.getTeamNickname();
		try {
			TeamPlayStatData teamPlayStat = new TeamPlayStatData();
			teamPlayStat.setTeam(team);
			PbpPlayerStatPojo stats = play.getPlayerStat().get(playTeamId);

			translatePenalty(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat, teamNickname);
			translateRush(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat, teamNickname);
			for (PbpPlayerStatDefenseProductionPojo kcov : stats.getSpecialTeam().getKickCoverage()) {
				translateDefense(kcov, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat,
						StatDefSpecTeam.KICKOFF, teamNickname);
			}
			for (PbpPlayerStatDefenseProductionPojo def : stats.getDefense().getDefenseProduction()) {
				translateDefense(def, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat,
						StatDefSpecTeam.DEF, teamNickname);
			}
			for (PbpPlayerStatDefenseProductionPojo pcov : stats.getSpecialTeam().getPuntCoverage()) {
				translateDefense(pcov, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat,
						StatDefSpecTeam.PUNT, teamNickname);
			}

			translatePunt(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat, teamNickname);
			translatePuntReturn(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat, teamNickname);
			translateKickoffReturn(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat,
					teamNickname);
			translateKickoff(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat, teamNickname);
			translateKick(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat, teamNickname);
			translatePass(stats, posNeg, statType, teamPlayerDataMap.get(playTeamId), teamPlayStat, teamNickname);
			return teamPlayStat;
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, playTeamId));
			return null;
		}
	}

	private void addTeamPlayers(Map<String, List<PlayerData>> teamPlayerDataMap, Map<String, Set<String>> teamPlayerMap,
			String playTeamId, TeamData team) {
		String innerErrorMsg = "Team player addition failed - %s";
		try {
			teamPlayerDataMap.put(playTeamId, new ArrayList<>());

			Set<String> playerNames = teamPlayerMap.get(playTeamId);
			for (String name : playerNames) {
				translatePlayer(teamPlayerDataMap, team, playTeamId, name);
			}
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, playTeamId));
		}
	}

	private void translatePlayer(Map<String, List<PlayerData>> teamPlayerDataMap, TeamData team, String playTeamId,
			String name) {
		String innerErrorMsg = "Player data translation failed - %s";
		try {
			String[] nameSplit = name.split(" ");
			String firstName;
			String lastName;
			if (nameSplit.length > 2) {
				throw new IllegalArgumentException("long name!");
			}
			if (nameSplit.length < 2) {
				firstName = nameSplit[0];
				lastName = team.getTeamNickname();
			} else {
				firstName = nameSplit[0];
				lastName = nameSplit[1];
			}
			PlayerData player = playerService.resolvePlayer(firstName, lastName, team);
			teamPlayerDataMap.get(playTeamId).add(player);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, name));
		}
	}

	private void pullPlayerListFromPlays(Map<String, Set<String>> teamPlayerMap, GamePojo gamePojo) {
		String errorMsg = "Player list pull failed";
		try {
			for (DrivePojo drive : gamePojo.getPlays().getDrives()) {
				for (PlayPojo play : drive.getDrivePlays()) {
					Set<String> playTeamIds = play.getPlayerStat().keySet();

					for (String playTeamId : playTeamIds) {

						PbpPlayerStatPojo stats = play.getPlayerStat().get(playTeamId);
						for (PlayerStatPenaltyPojo pen : stats.getPenalty()) {
							teamPlayerMap.get(playTeamId).add(pen.getPlayerName());
						}

						for (PbpPlayerStatDefenseProductionPojo def : stats.getDefense().getDefenseProduction()) {
							teamPlayerMap.get(playTeamId).add(def.getPlayerName());
						}
						for (PbpPlayerStatRushingPojo rush : stats.getOffense().getRushingStat()) {
							teamPlayerMap.get(playTeamId).add(rush.getPlayerName());
						}
						for (PbpPlayerStatPassingPojo pass : stats.getOffense().getPassingStat()) {
							teamPlayerMap.get(playTeamId).add(pass.getPlayerName());
						}
						for (PbpPlayerStatReceivingPojo rec : stats.getOffense().getReceivingStat()) {
							teamPlayerMap.get(playTeamId).add(rec.getPlayerName());
						}
						for (PbpPlayerStatDefenseProductionPojo kcov : stats.getSpecialTeam().getKickCoverage()) {
							teamPlayerMap.get(playTeamId).add(kcov.getPlayerName());
						}
						for (PbpPlayerStatKickingPojo kick : stats.getSpecialTeam().getKicking()) {
							teamPlayerMap.get(playTeamId).add(kick.getPlayerName());
						}
						for (PbpPlayerStatKickoffPojo ko : stats.getSpecialTeam().getKickoff()) {
							teamPlayerMap.get(playTeamId).add(ko.getPlayerName());
						}
						for (PbpPlayerStatKickReturnPojo kret : stats.getSpecialTeam().getKickReturn()) {
							teamPlayerMap.get(playTeamId).add(kret.getPlayerName());
						}
						for (PbpPlayerStatDefenseProductionPojo pcov : stats.getSpecialTeam().getPuntCoverage()) {
							teamPlayerMap.get(playTeamId).add(pcov.getPlayerName());
						}
						for (PbpPlayerStatPuntingPojo punt : stats.getSpecialTeam().getPunting()) {
							teamPlayerMap.get(playTeamId).add(punt.getPlayerName());
						}
						for (PbpPlayerStatPuntReturnPojo pret : stats.getSpecialTeam().getPuntReturn()) {
							teamPlayerMap.get(playTeamId).add(pret.getPlayerName());
						}
					}
				}
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translatePass(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Pass data translation failed";
		try {
			for (PbpPlayerStatPassingPojo stat : stats.getOffense().getPassingStat()) {
				translatePassHelper(stats, posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translatePassHelper(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, PbpPlayerStatPassingPojo stat,
			String teamNickname) {
		String innerErrorMsg = "Pass data translation failed - %s";
		try {
			StatPassingData statData = (StatPassingData) mappingUtils.mapToDto(stat, StatPassingData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));

			translateReceiver(stats, posNeg, statType, posMatches, statData, teamNickname);
			teamPlayStat.setPassStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}
	}

	private void translateReceiver(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, StatPassingData passData, String teamNickname) {
		String errorMsg = "Receiver data translation failed";
		try {
			for (PbpPlayerStatReceivingPojo stat : stats.getOffense().getReceivingStat()) {
				translateReceiverHelper(posNeg, statType, posMatches, passData, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translateReceiverHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			StatPassingData passData, PbpPlayerStatReceivingPojo stat, String teamNickname) {
		String innerErrorMsg = "Receiver data translation failed - %s";

		try {
			StatReceivingData statData = (StatReceivingData) mappingUtils.mapToDto(stat, StatReceivingData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			passData.setReceiving(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}

	}

	private void translateKick(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Kick data translation failed";
		try {
			for (PbpPlayerStatKickingPojo stat : stats.getSpecialTeam().getKicking()) {
				translateKickHelper(posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translateKickHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			TeamPlayStatData teamPlayStat, PbpPlayerStatKickingPojo stat, String teamNickname) {
		String innerErrorMsg = "Kick data translation failed - %s";

		try {
			StatKickData statData = new StatKickData();
			if (stat.getExtraPointAttempt() > 0) {
				statData.setFieldGoal(stat.getExtraPoint());
				statData.setFieldGoalAttempt(stat.getExtraPointAttempt());
				statData.setFieldGoalBlock(stat.getExtraPointBlock());
				statData.setFieldGoalMiss(stat.getExtraPointMiss());
				statData.setFieldGoalYard(stat.getExtraPointYard());
				statData.setKickType(KickTypeEnum.PAT);
			} else {
				statData.setFieldGoal(stat.getFieldGoal());
				statData.setFieldGoalAttempt(stat.getFieldGoalAttempt());
				statData.setFieldGoalBlock(stat.getFieldGoalBlock());
				statData.setFieldGoalMiss(stat.getFieldGoalMiss());
				statData.setFieldGoalYard(stat.getFieldGoalYard());
				statData.setKickType(KickTypeEnum.FG);
			}
			statData.setTotalPoint(stat.getTotalPoint());
			statData.setKickMissReason(stat.getKickMissReason());

			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			teamPlayStat.setKickStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}
	}

	private void translateKickoff(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Kickoff data translation failed";
		try {
			for (PbpPlayerStatKickoffPojo stat : stats.getSpecialTeam().getKickoff()) {
				translateKickoffHelper(posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translateKickoffHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			TeamPlayStatData teamPlayStat, PbpPlayerStatKickoffPojo stat, String teamNickname) {
		String innerErrorMsg = "Kickoff data translation failed - %s";

		try {
			StatKickoffData statData = (StatKickoffData) mappingUtils.mapToDto(stat, StatKickoffData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			teamPlayStat.setKickoffStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}

	}

	private void translateKickoffReturn(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Kickoff return data translation failed";
		try {
			for (PbpPlayerStatKickReturnPojo stat : stats.getSpecialTeam().getKickReturn()) {
				translateKickoffReturnHelper(posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translateKickoffReturnHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			TeamPlayStatData teamPlayStat, PbpPlayerStatKickReturnPojo stat, String teamNickname) {
		String innerErrorMsg = "Kickoff return data translation failed - %s";
		try {
			StatKickoffReturnData statData = (StatKickoffReturnData) mappingUtils.mapToDto(stat,
					StatKickoffReturnData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			teamPlayStat.setKickoffReturnStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}
	}

	private void translatePunt(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Punt data translation failed";
		try {
			for (PbpPlayerStatPuntingPojo stat : stats.getSpecialTeam().getPunting()) {
				translatePuntHelper(posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translatePuntHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			TeamPlayStatData teamPlayStat, PbpPlayerStatPuntingPojo stat, String teamNickname) {
		String innerErrorMsg = "Punt data translation failed - %s";

		try {
			StatPuntData statData = (StatPuntData) mappingUtils.mapToDto(stat, StatPuntData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));

			if (statData.getPuntLandYard() >= 80) {
				statData.setPuntInTwenty(1);
			} else {
				statData.setPuntInTwenty(0);
			}
			if (statData.getPuntLandYard() >= 90) {
				statData.setPuntInTen(1);
			} else {
				statData.setPuntInTen(0);
			}
			if (statData.getPuntLandYard() >= 95) {
				statData.setPuntInFive(1);
			} else {
				statData.setPuntInFive(0);
			}
			teamPlayStat.setPuntStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}

	}

	private void translatePuntReturn(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Punt return data translation failed";
		try {
			for (PbpPlayerStatPuntReturnPojo stat : stats.getSpecialTeam().getPuntReturn()) {
				translatePuntReturnHelper(posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translatePuntReturnHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			TeamPlayStatData teamPlayStat, PbpPlayerStatPuntReturnPojo stat, String teamNickname) {
		String innerErrorMsg = "Punt return data translation failed - %s";
		try {
			StatPuntReturnData statData = (StatPuntReturnData) mappingUtils.mapToDto(stat, StatPuntReturnData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			teamPlayStat.setPuntReturnStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}
	}

	private void translatePenalty(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Panalty data translation failed";
		try {
			for (PlayerStatPenaltyPojo stat : stats.getPenalty()) {
				translatePenaltyHelper(posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translatePenaltyHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			TeamPlayStatData teamPlayStat, PlayerStatPenaltyPojo stat, String teamNickname) {
		String innerErrorMsg = "Penalty data translation failed - %s";
		try {
			StatPenaltyData statData = (StatPenaltyData) mappingUtils.mapToDto(stat, StatPenaltyData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			teamPlayStat.addPenaltyStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}

	}

	private void translateDefense(PbpPlayerStatDefenseProductionPojo stat, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, StatDefSpecTeam defType, String teamNickname) {
		String innerErrorMsg = "Defense data translation failed - %s";
		try {
			StatDefenseData statData = (StatDefenseData) mappingUtils.mapToDto(stat, StatDefenseData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			statData.setDefenseType(defType);
			teamPlayStat.addDefenseStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}
	}

	private void translateRush(PbpPlayerStatPojo stats, StatPosNegEnum posNeg, StatTypeEnum statType,
			List<PlayerData> posMatches, TeamPlayStatData teamPlayStat, String teamNickname) {
		String errorMsg = "Rush data translation failed";
		try {
			for (PbpPlayerStatRushingPojo stat : stats.getOffense().getRushingStat()) {
				translateRushHelper(posNeg, statType, posMatches, teamPlayStat, stat, teamNickname);
			}
		} catch (Exception e) {
			loggingUtils.logException(e, errorMsg);
		}
	}

	private void translateRushHelper(StatPosNegEnum posNeg, StatTypeEnum statType, List<PlayerData> posMatches,
			TeamPlayStatData teamPlayStat, PbpPlayerStatRushingPojo stat, String teamNickname) {
		String innerErrorMsg = "Rush data translation failed - %s";
		try {
			StatRushingData statData = (StatRushingData) mappingUtils.mapToDto(stat, StatRushingData.class);
			statData.setPosNeg(posNeg);
			statData.setStatType(statType);
			statData.setPlayer(matchName(stat.getPlayerName(), teamNickname, posMatches));
			teamPlayStat.setRushStat(statData);
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, stat.toString()));
		}
	}

	private PlayerData matchName(String name, String teamNickname, List<PlayerData> posMatches) {
		String first;
		String last;
		String innerErrorMsg = "Player match failed - %s";

		try {
			String[] splitName = name.split(" ");
			if (splitName.length == 2) {
				first = splitName[0];
				last = splitName[1];
			} else {
				first = splitName[0];
				last = teamNickname;
			}

			for (PlayerData posMatch : posMatches) {
				if (posMatch.getFirstName().equals(first) && posMatch.getLastName().equals(last)) {
					return posMatch;
				}
			}
			throw new IllegalArgumentException(String.format("No player match found for %s", name));
		} catch (Exception ee) {
			loggingUtils.logException(ee, String.format(innerErrorMsg, name));
			return null;
		}
	}

}
