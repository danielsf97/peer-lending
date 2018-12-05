public class Bid {

    private String investor;
    private long value;
    private float rate;

    public Bid(String investor, long value, float rate) {
        this.investor = investor;
        this.value = value;
        this.rate = rate;
    }

    public String getInvestor() {
        return investor;
    }

    public long getValue() {
        return value;
    }

    public float getRate() {
        return rate;
    }
}
