package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.service;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.stereotype.Service;

import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.ParseResponse;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.internal.UrlParseRequest;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.MappingUtils;
import com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils.UrlUtils;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

@Service
public class OddsService {
	private static final Logger LOG = Logger.getLogger(OddsService.class.toString());

	private final UrlUtils urlUtils;
	private final MappingUtils mappingUtils;
	
	public OddsService(UrlUtils urlUtils, MappingUtils mappingUtils) {
		this.urlUtils = urlUtils;
		this.mappingUtils = mappingUtils;
	}
	
	public ParseResponse parseDayOdds() {
//		String url = String.format("https://www.sportsbookreview.com/betting-odds/college-football/totals/?date=20160917");
		String url = String.format("https://www.ncaa.com/scoreboard/football/fcs/2021/01");

		UrlParseRequest urlParseRequest;

		HtmlPage page;
		Integer infoFound = null;
		Integer infoCompleted = 0;
		String errorStr = null;
		List<HtmlElement> teamList;

//		FighterData fighterData;

		try {
			urlParseRequest = urlUtils.parse(url);
			errorStr = urlParseRequest.getErrorStr();
			if (urlParseRequest.getSuccess()) {

				page = urlParseRequest.getPage();
				LOG.info("Completed Day Odds Parse");

				parseBooks(page);

				teamList = page.getByXPath("/html/body/div[1]/div/div/div/section/div/div[3]/div[3]/div/div[1]/section/div[1]/div/div/a/span");
				infoFound = teamList.size();
				
				for (HtmlElement team: teamList) {
					System.out.println(team.asText());
				}
				LOG.info(String.format("%s items found", infoFound));

//				if (fighterHTMLList.isEmpty()) {
//					errorStr = String.format("No fighter info found for fighterId: %s", fighterId);
//					LOG.log(Level.INFO, String.format(COMPLETION_MESSAGE, infoFound, infoCompleted));
//					throw new IllegalArgumentException(errorStr);
//				} else {
//					fighterData = new FighterData();
//					parseFighterName(page, fighterData);
//					parseFighterHeight(page, fighterData);
//					parseFighterReach(page, fighterData);
//					parseFighterStance(page, fighterData);
//					parseFighterDob(page, fighterData);
//					fighterData.setFighterId(fighterId);
//					fighterRepo.save(fighterData);
//					LOG.log(Level.INFO,
//							String.format(SUCCESSFUL_SAVE_MESSAGE, fighterData.getFighterName(), fighterId));
//
//				}
//				LOG.log(Level.INFO, String.format(COMPLETION_MESSAGE, infoFound, infoCompleted));
//				return fighterData;
				return new ParseResponse();
			} else {
				LOG.log(Level.WARNING, errorStr);
				throw new IllegalArgumentException(errorStr);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, errorStr);
			throw new IllegalArgumentException(errorStr);
		}

	}
	
	
	private Object parseBooks(HtmlPage page) {
		List<HtmlElement> bookList;
//		bookList = page.getByXPath("//*[@id=\"bettingOddsGridContainer\"]/div[3]/div[2]/div");
//		bookList = page.getByXPath("//*[@id=\"bettingOddsGridContainer\"]/div[3]/div/div[2]");
//		bookList = page.getByXPath("//*[@id=\"bettingOddsGridContainer\"]/div[3]/div/div[3]");
		bookList = page.getByXPath("/html/body/div[1]/div/main/div/div/div/div[2]/div/div");
		for (HtmlElement book: bookList) {
			System.out.println(book.asXml());
		}
		return true;
	}
	
}
