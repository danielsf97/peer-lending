import zmq.socket.pubsub.Sub;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;

public class Emission {
    private long value;
    private float fixedRate;
    private LocalDateTime startingDateTime;
    private static int duration = 10;
    private ArrayList<Subscription> subscriptions;

    Emission(long value) {
        this.value = value;
        this.startingDateTime = LocalDateTime.now();
        this.subscriptions = new ArrayList<>();
    }

    public static int getDuration() {
        return duration;
    }

    public long getValue() {
        return value;
    }

    public float getFixedRate() {
        return fixedRate;
    }

    public LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }

    public void addSubscription(String investor, long value) {
        this.subscriptions.add(new Subscription(investor, value));
    }

    public Pair<ArrayList<Pair<String,Long>>,ArrayList<String>> getWinnersLosers() {
        ArrayList<Pair<String,Long>> winners = new ArrayList<>();
        ArrayList<String> losers = null;
        Iterator it = subscriptions.iterator();
        Subscription subscription = null;
        long sum = 0;

        while(sum < value && it.hasNext()){
            subscription = (Subscription) it.next();
            sum += subscription.getValue();
            winners.add(new Pair(subscription.getInvestor(), subscription.getValue()));
        }

        if(sum >= value) {
            losers = new ArrayList<>();
            while(it.hasNext()){
                subscription = (Subscription) it.next();
                losers.add(subscription.getInvestor());
            }
        }

        if (sum > value) {
            long diff = sum - value;
            int size = winners.size();
            Pair pair = winners.get(size - 1);
            pair.setSecond(diff);
            return new Pair<>(winners, losers);
        }

        return new Pair<>(winners, losers);
    }
}
