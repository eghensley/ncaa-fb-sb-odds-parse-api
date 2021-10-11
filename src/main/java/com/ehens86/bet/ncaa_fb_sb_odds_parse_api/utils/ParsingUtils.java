package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Service
public class ParsingUtils {
	private static final Logger LOG = Logger.getLogger(ParsingUtils.class.toString());

	public Integer parseString(String val) {
		try {
			if ("".equals(val)) {
				return 0;
			} else {
				return Integer.valueOf(val);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}

	public Double parseStringToDouble(String val) {
		try {
			if ("".equals(val)) {
				return 0.0;
			} else {
				return Double.valueOf(val);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();
			throw new IllegalArgumentException(e.toString());
		}
	}
	
	public Integer splitValue(String value, String delim, Integer index) {
		try {
			ArrayList<Integer> mults = new ArrayList<Integer>(Arrays.asList(1, 1));

			Integer delimCount = StringUtils.countMatches(value, delim);
			if (Integer.valueOf("2").equals(delimCount)) {
				if ('-' == value.charAt(0)) {
					mults.set(0, -1);
				} else {
					mults.set(1, -1);
				}
			} else if (Integer.valueOf("0").equals(delimCount)) {
				System.out.println(value);
			} else if (delimCount > 2) {
				System.out.println(value);
			}

			Integer delimIndex = value.indexOf(delim);
			if (Integer.valueOf(value.length()).equals(delimIndex + 1)) {
				value = value + "0";
			}
			if (Integer.valueOf("0").equals(delimIndex)) {
				value = "0" + value;
			}

			String[] splitValue = value.split(delim);
			splitValue = Arrays.stream(splitValue).filter(x -> x.length() > 0).toArray(String[]::new);

			String indexedValue = splitValue[index];
			return Integer.valueOf(indexedValue) * mults.get(index);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.toString());
			e.printStackTrace();

			throw new IllegalArgumentException(e.toString());
		}
	}
}
