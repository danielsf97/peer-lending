
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.TreeSet;

public class Auction {
    private long value;
    private float maxRate;
    private LocalDateTime startingDateTime;
    private static int duration = 10;
    private TreeSet<Bid> bids;

    Auction(long value, float maxRate) {
        this.value = value;
        this.maxRate = maxRate;
        this.startingDateTime = LocalDateTime.now();
        this.bids = new TreeSet<>(new Comparator<Bid>() {
            @Override
            public int compare(Bid bid1, Bid bid2) {
                if(bid1.getRate() < bid2.getRate()) return -1;
                else if(bid1.getRate() > bid2.getRate()) return 1;
                return 0;
            }
        });
    }

    static int getDuration() {
        return duration;
    }

    long getValue() {
        return value;
    }

    float getMaxRate() {
        return maxRate;
    }

    LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }

    public int addBid(String investor, long value, float rate) {
        boolean found = false;
        Bid bid = null;

        if(rate > maxRate) return -1;

        Iterator it = bids.iterator();

        while(!found && it.hasNext()) {
            bid = (Bid) it.next();
            if(bid.getInvestor().equals(investor)) found = true;
        }

        if(found) {
            bids.remove(bid);
        }

        this.bids.add(new Bid(investor, value, rate));

        if(found) return 1;
        else return 0;
    }

    Pair<ArrayList<Pair<String,Long>>,ArrayList<String>> getWinnersLosers() {
        ArrayList<Pair<String,Long>> winners = new ArrayList<>();
        ArrayList<String> losers = null;
        Iterator it = bids.iterator();
        Bid bid;
        long sum = 0;

        while(sum < value && it.hasNext()){
            bid = (Bid) it.next();
            sum += bid.getValue();
            winners.add(new Pair(bid.getInvestor(), bid.getValue()));
        }

        if(sum >= value){
            losers = new ArrayList<>();
            while(it.hasNext()){
                bid = (Bid) it.next();
                losers.add(bid.getInvestor());
            }
        }

        if(sum == value) {
            return new Pair<>(winners, losers);
        }
        else if (sum > value ){
            long diff = sum - value;
            int size = winners.size();
            Pair pair = winners.get(size - 1);
            long update = (long) pair.getSecond() - diff;
            pair.setSecond(update);
            return new Pair<>(winners, losers);
        }
        else
            return new Pair<>(null, losers);
    }

    float getMaxBidRate(int n) {
        Bid bid = null;
        Iterator it = bids.iterator();

        while(it.hasNext() && n > 0){
            bid = (Bid) it.next();
            n--;
        }

        if(n == 0)
            return bid.getRate();
        else
            return -1;
    }
}
