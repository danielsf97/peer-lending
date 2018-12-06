public class Subscription {
    private String investor;
    private long value;

    public Subscription(String investor, long value) {
        this.investor = investor;
        this.value = value;
    }

    public String getInvestor() {
        return investor;
    }

    public long getValue() {
        return value;
    }
}
