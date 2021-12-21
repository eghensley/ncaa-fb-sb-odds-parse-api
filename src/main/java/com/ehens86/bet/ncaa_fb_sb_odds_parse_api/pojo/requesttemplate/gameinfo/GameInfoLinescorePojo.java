package com.ehens86.bet.ncaa_fb_sb_odds_parse_api.pojo.requesttemplate.gameinfo;

public class GameInfoLinescorePojo {
    private String v;
    private String h;
    private String per;
    
    public GameInfoLinescorePojo() {
    	
    }

	public GameInfoLinescorePojo(String v, String h, String per) {
		super();
		this.v = v;
		this.h = h;
		this.per = per;
	}

	/**
	 * @return the v
	 */
	public String getV() {
		return v;
	}

	/**
	 * @param v the v to set
	 */
	public void setV(String v) {
		this.v = v;
	}

	/**
	 * @return the h
	 */
	public String getH() {
		return h;
	}

	/**
	 * @param h the h to set
	 */
	public void setH(String h) {
		this.h = h;
	}

	/**
	 * @return the per
	 */
	public String getPer() {
		return per;
	}

	/**
	 * @param per the per to set
	 */
	public void setPer(String per) {
		this.per = per;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((h == null) ? 0 : h.hashCode());
		result = prime * result + ((per == null) ? 0 : per.hashCode());
		result = prime * result + ((v == null) ? 0 : v.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof GameInfoLinescorePojo)) {
			return false;
		}
		GameInfoLinescorePojo other = (GameInfoLinescorePojo) obj;
		if (h == null) {
			if (other.h != null) {
				return false;
			}
		} else if (!h.equals(other.h)) {
			return false;
		}
		if (per == null) {
			if (other.per != null) {
				return false;
			}
		} else if (!per.equals(other.per)) {
			return false;
		}
		if (v == null) {
			if (other.v != null) {
				return false;
			}
		} else if (!v.equals(other.v)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "GameInfoLinescorePojo [v=" + v + ", h=" + h + ", per=" + per + "]";
	}
    
    
    
}
