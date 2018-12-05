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

    public static int getDuration() {
        return duration;
    }

    public long getValue() {
        return value;
    }

    public float getMaxRate() {
        return maxRate;
    }

    public LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }

    public int addBid(String investor, long value, float rate){
        boolean found = false;
        Bid bid = null;

        if(rate > maxRate) return -1;

        Iterator it = bids.iterator();

        while(!found && it.hasNext()){
            bid = (Bid) it.next();
            if(bid.getInvestor().equals(investor)) found = true;
        }

        if(found){
            bids.remove(bid);
        }

        this.bids.add(new Bid(investor, value, rate));

        if(found) return 1;
        else return 0;
    }

    public ArrayList<String> getWinners(){
        ArrayList<String> res = new ArrayList<>();
        Iterator it = bids.iterator();
        Bid bid = null;
        long sum = 0;

        while(sum < value && it.hasNext()){
            bid = (Bid) it.next();
            sum += bid.getValue();
            res.add(bid.getInvestor());
        }

        if(sum >= value)
            return res;
        else return null;
    }
}
