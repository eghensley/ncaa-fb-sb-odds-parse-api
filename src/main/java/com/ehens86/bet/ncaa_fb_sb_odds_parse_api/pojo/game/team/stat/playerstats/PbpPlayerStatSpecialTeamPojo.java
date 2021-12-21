package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.defense.pbp.PbpPlayerStatDefenseProductionPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickingPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatKickoffPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntReturnPojo;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.game.team.stat.playerstats.specialteams.pbp.PbpPlayerStatPuntingPojo;

public class PbpPlayerStatSpecialTeamPojo {
	private List<PbpPlayerStatKickingPojo> kicking;
	private List<PbpPlayerStatPuntingPojo> punting;
	private List<PbpPlayerStatPuntReturnPojo> puntReturn;
	private List<PbpPlayerStatKickReturnPojo> kickReturn;
	private List<PbpPlayerStatDefenseProductionPojo> kickCoverage;
	private List<PbpPlayerStatDefenseProductionPojo> puntCoverage;
	private List<PbpPlayerStatKickoffPojo> kickoff;

	public PbpPlayerStatSpecialTeamPojo() {
		this.kicking = new ArrayList<>();
		this.punting = new ArrayList<>();
		this.puntReturn = new ArrayList<>();
		this.kickReturn = new ArrayList<>();
		this.kickCoverage = new ArrayList<>();
		this.kickoff = new ArrayList<>();
		this.puntCoverage = new ArrayList<>();
	}

	public PbpPlayerStatSpecialTeamPojo(List<PbpPlayerStatKickingPojo> kicking, List<PbpPlayerStatPuntingPojo> punting,
			List<PbpPlayerStatPuntReturnPojo> puntReturn, List<PbpPlayerStatKickReturnPojo> kickReturn,
			List<PbpPlayerStatDefenseProductionPojo> kickCoverage,
			List<PbpPlayerStatDefenseProductionPojo> puntCoverage, List<PbpPlayerStatKickoffPojo> kickoff) {
		super();
		this.kicking = kicking;
		this.punting = punting;
		this.puntReturn = puntReturn;
		this.kickReturn = kickReturn;
		this.kickCoverage = kickCoverage;
		this.puntCoverage = puntCoverage;
		this.kickoff = kickoff;
	}

	public PbpPlayerStatKickReturnPojo findKickReturnByName(String playerName) {
		if (this.kickReturn.stream()
				.filter(name -> playerName.equals(name.getPlayerName()) && name.getKickReturn() == 1)
				.collect(Collectors.toList()).isEmpty()) {
			this.kickReturn.add(new PbpPlayerStatKickReturnPojo(playerName));
		}
		return this.kickReturn.stream().filter(name -> playerName.equals(name.getPlayerName()))
				.collect(Collectors.toList()).get(0);
	}

	public PbpPlayerStatPuntReturnPojo findPuntReturnByName(String playerName) {
		if (puntReturn.stream().filter(name -> playerName.equals(name.getPlayerName()) && name.getPuntReturn() == 1)
				.collect(Collectors.toList()).isEmpty()) {
			if (!puntReturn.stream().filter(name -> playerName.equals(name.getPlayerName()))
					.collect(Collectors.toList()).isEmpty()) {
				puntReturn.stream().filter(name -> playerName.equals(name.getPlayerName())).collect(Collectors.toList())
						.get(0).setPuntReturn(1);
			} else {
				this.puntReturn.add(new PbpPlayerStatPuntReturnPojo(playerName));
			}
		}
		return puntReturn.stream().filter(name -> playerName.equals(name.getPlayerName())).collect(Collectors.toList())
				.get(0);
	}

	public PbpPlayerStatDefenseProductionPojo findKickCoverageWithTurnover() {
		return this.kickCoverage.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveTurnover)
				.collect(Collectors.toList()).get(0);
	}

	public PbpPlayerStatDefenseProductionPojo findPuntCoverageWithTurnover() {
		return this.puntCoverage.stream().filter(PbpPlayerStatDefenseProductionPojo::resolveTurnover)
				.collect(Collectors.toList()).get(0);
	}
	
	public PbpPlayerStatKickReturnPojo findKickoffReturner() {
		if (kickReturn.stream().filter(returner -> returner.getKickReturn() == 1).collect(Collectors.toList())
				.size() != 1) {
			throw new IllegalArgumentException("Number of returners != 1");
		}
		return kickReturn.stream().filter(returner -> returner.getKickReturn() == 1).collect(Collectors.toList())
				.get(0);
	}

	/**
	 * @return the kicking
	 */
	public List<PbpPlayerStatKickingPojo> getKicking() {
		return kicking;
	}

	/**
	 * @param kicking the kicking to set
	 */
	public void setKicking(List<PbpPlayerStatKickingPojo> kicking) {
		this.kicking = kicking;
	}

	/**
	 * @return the punting
	 */
	public List<PbpPlayerStatPuntingPojo> getPunting() {
		return punting;
	}

	/**
	 * @param punting the punting to set
	 */
	public void setPunting(List<PbpPlayerStatPuntingPojo> punting) {
		this.punting = punting;
	}

	/**
	 * @return the puntReturn
	 */
	public List<PbpPlayerStatPuntReturnPojo> getPuntReturn() {
		return puntReturn;
	}

	/**
	 * @param puntReturn the puntReturn to set
	 */
	public void setPuntReturn(List<PbpPlayerStatPuntReturnPojo> puntReturn) {
		this.puntReturn = puntReturn;
	}

	/**
	 * @return the kickReturn
	 */
	public List<PbpPlayerStatKickReturnPojo> getKickReturn() {
		return kickReturn;
	}

	/**
	 * @param kickReturn the kickReturn to set
	 */
	public void setKickReturn(List<PbpPlayerStatKickReturnPojo> kickReturn) {
		this.kickReturn = kickReturn;
	}

	/**
	 * @return the kickCoverage
	 */
	public List<PbpPlayerStatDefenseProductionPojo> getKickCoverage() {
		return kickCoverage;
	}

	/**
	 * @param kickCoverage the kickCoverage to set
	 */
	public void setKickCoverage(List<PbpPlayerStatDefenseProductionPojo> kickCoverage) {
		this.kickCoverage = kickCoverage;
	}

	/**
	 * @return the puntCoverage
	 */
	public List<PbpPlayerStatDefenseProductionPojo> getPuntCoverage() {
		return puntCoverage;
	}

	/**
	 * @param puntCoverage the puntCoverage to set
	 */
	public void setPuntCoverage(List<PbpPlayerStatDefenseProductionPojo> puntCoverage) {
		this.puntCoverage = puntCoverage;
	}

	/**
	 * @return the kickoff
	 */
	public List<PbpPlayerStatKickoffPojo> getKickoff() {
		return kickoff;
	}

	/**
	 * @param kickoff the kickoff to set
	 */
	public void setKickoff(List<PbpPlayerStatKickoffPojo> kickoff) {
		this.kickoff = kickoff;
	}

	public PbpPlayerStatDefenseProductionPojo findPuntCoverageByName(String playerName) {
		if (this.puntCoverage.stream().filter(name -> playerName.equals(name.getPlayerName()))
				.collect(Collectors.toList()).isEmpty()) {
			PbpPlayerStatDefenseProductionPojo newCoverage = new PbpPlayerStatDefenseProductionPojo();
			newCoverage.applyBase(playerName);
			this.puntCoverage.add(newCoverage);
		}
		return this.puntCoverage.stream().filter(name -> playerName.equals(name.getPlayerName()))
				.collect(Collectors.toList()).get(0);
	}

	public List<PbpPlayerStatPuntReturnPojo> findPuntReturnWithFumble() {
		return puntReturn.stream().filter(name -> name.getPuntReturnFumble() == 1).collect(Collectors.toList());
	}

	public List<PbpPlayerStatKickReturnPojo> findKickReturnWithFumble() {
		return this.kickReturn.stream().filter(name -> name.getKickReturnFumble() == 1).collect(Collectors.toList());
	}

	public void applyPuntBlock(String playerName) {
		if (puntReturn.stream().filter(name -> playerName.equals(name.getPlayerName())).collect(Collectors.toList())
				.isEmpty()) {
			if (!this.puntReturn.isEmpty() && this.puntReturn.size() == 1) {
				this.puntReturn.get(0).setPuntReturnBlock(0);
			}

			PbpPlayerStatPuntReturnPojo block = new PbpPlayerStatPuntReturnPojo(playerName);
			block.setPuntReturn(0);
			block.setPuntReturnBlock(1);
			block.setPuntReturnFairCatch(0);
			block.setPuntReturnFumble(0);
			block.setPuntReturnStartYard(0);
			block.setPuntReturnYard(0);
			this.puntReturn.add(block);
		} else {
			puntReturn.stream().filter(name -> playerName.equals(name.getPlayerName())).collect(Collectors.toList())
					.get(0).setPuntReturnBlock(1);
		}
	}

	public PbpPlayerStatDefenseProductionPojo findKickCoverageByName(String playerName) {
		if (this.kickCoverage.stream().filter(name -> playerName.equals(name.getPlayerName()))
				.collect(Collectors.toList()).isEmpty()) {
			PbpPlayerStatDefenseProductionPojo newCoverage = new PbpPlayerStatDefenseProductionPojo();
			newCoverage.applyBase(playerName);
			this.kickCoverage.add(newCoverage);
		}
		return this.kickCoverage.stream().filter(name -> playerName.equals(name.getPlayerName()))
				.collect(Collectors.toList()).get(0);
	}

	public PbpPlayerStatPuntReturnPojo findPuntReturner() {
		if (puntReturn.stream().filter(returner -> returner.getPuntReturn() == 1).collect(Collectors.toList())
				.size() != 1) {
			if (puntReturn.stream().filter(returner -> returner.getPuntReturnBlock() == 1).collect(Collectors.toList())
					.size() == 1) {
				return null;
			} else {
				throw new IllegalArgumentException("Number of returners != 1");
			}
		}
		return puntReturn.stream().filter(returner -> returner.getPuntReturn() == 1).collect(Collectors.toList())
				.get(0);
	}

}
