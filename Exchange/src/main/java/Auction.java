
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.TreeSet;


/**
 * Representa um leilão.
 *
 */
public class Auction {
    private long value;
    private float maxRate;
    private LocalDateTime startingDateTime;
    private static int duration = 10;
    private TreeSet<Bid> bids;


    /**
     * Construtor parametrizado.
     *
     * @param value   Valor do leilão.
     * @param maxRate Taxa máxima admitida.
     */
    Auction(long value, float maxRate) {
        this.value = value;
        this.maxRate = maxRate;
        this.startingDateTime = LocalDateTime.now();
        this.bids = new TreeSet<>(new Comparator<Bid>() {
            @Override
            public int compare(Bid bid1, Bid bid2) {
                if (bid1.getRate() < bid2.getRate()) return -1;
                else if (bid1.getRate() > bid2.getRate()) return 1;
                return 0;
            }
        });
    }


    /**
     * Retorna a duração de um leilão.
     *
     * @return a duração de um leilão.
     */
    static int getDuration() {
        return duration;
    }


    /**
     * Retorna o valor de um leilão.
     *
     * @return o valor de um leilão.
     */
    long getValue() {
        return value;
    }


    /**
     * Retorna a taxa máxima admitida no leilão.
     *
     * @return a taxa máxima admitida no leilão.
     */
    float getMaxRate() {
        return maxRate;
    }


    /**
     * Retorna a data e hora de começo do leilão.
     *
     * @return a data e hora de começo do leilão.
     */
    LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }


    /**
     * Adiciona uma licitação ao leilão.
     *
     * @param investor Investidor que efetuou a licitação.
     * @param value    Valor da licitação.
     * @param rate     Taxa da licitação.
     * @return -1 em caso de invalidez da licitação
     *          0 em caso de sucesso (nova licitação)
     *          1 em caso de sucesso (licitação alterada)
     */
    public int addBid(String investor, String clientSession,long value, float rate) {
        boolean found = false;
        Bid bid = null;

        if (rate > maxRate) return -1;

        Iterator it = bids.iterator();

        while (!found && it.hasNext()) {
            bid = (Bid) it.next();
            if (bid.getInvestor().equals(investor)) found = true;
        }

        if (found) {
            bids.remove(bid);
        }

        this.bids.add(new Bid(investor, clientSession, value, rate));

        if (found) return 1;
        else return 0;
    }


    /**
     * Retorna os vencedores e perdedores de um leilão.
     *
     * @return um par constituído pelos vencedores e perdedores de um leilão.
     */
    Pair<ArrayList<Pair<String, Long>>, ArrayList<String>> getWinnersLosers() {
        ArrayList<Pair<String, Long>> winners = new ArrayList<>();
        ArrayList<String> losers = null;
        Iterator it = bids.iterator();
        Bid bid;
        long sum = 0;

        while (sum < value && it.hasNext()) {
            bid = (Bid) it.next();
            sum += bid.getValue();
            winners.add(new Pair(bid.getInvestor(), bid.getValue()));
        }

        if (sum >= value) {
            losers = new ArrayList<>();
            while (it.hasNext()) {
                bid = (Bid) it.next();
                losers.add(bid.getInvestor());
            }
        }

        if (sum == value) {
            return new Pair<>(winners, losers);
        }
        else if (sum > value) {
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


    /**
     * Retorna a taxa da licitação com a taxa mais alta.
     *
     * @param n Número de vencedores do leilão.
     * @return Valor da taxa mais alta.
     */
    float getMaxBidRate(int n) {
        Bid bid = null;
        Iterator it = bids.iterator();

        while (it.hasNext() && n > 0) {
            bid = (Bid) it.next();
            n--;
        }

        if (n == 0)
            return bid.getRate();
        else
            return -1;
    }
}