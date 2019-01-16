import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;


/**
 * Representa uma emissão.
 *
 */
public class Emission {
    private long value;
    private float fixedRate;
    private LocalDateTime startingDateTime;
    private static int duration = 1;
    private ArrayList<Subscription> subscriptions;


    /**
     * Construtor parametrizado.
     *
     * @param value     Valor da emissão.
     */
    Emission(long value) {
        this.value = value;
        this.startingDateTime = LocalDateTime.now();
        this.subscriptions = new ArrayList<>();
    }


    /**
     * Devolve a duração de uma emissão.
     *
     * @return a duração de uma emissão.
     */
    public static int getDuration() {
        return duration;
    }


    /**
     * Devolve o valor de uma emissão.
     *
     * @return o valor de uma emissão.
     */
    public long getValue() {
        return value;
    }


    /**
     * Devolve a taxa fixa de uma emissão.
     *
     * @return a taxa fixa de uma emissão.
     */
    public float getFixedRate() {
        return fixedRate;
    }


    /**
     * Devolve a data e hora de começo de uma emissão.
     *
     * @return a data e hora de começo de uma emissão.
     */
    public LocalDateTime getStartingDateTime() {
        return startingDateTime;
    }


    /**
     * Adiciona uma subscrição à emissão.
     *
     * @param investor      Investidor responsável pela subscrição.
     * @param value         Valor da subscrição.
     */
    public void addSubscription(String investor, String clientSession, long value) {
        this.subscriptions.add(new Subscription(investor, clientSession, value));
    }


    /**
     * Devolve os vencedores e perdedores de uma emissão.
     *
     * @return um par cujo primeiro elemento são os vencedores e segundo elemento
     *         os perdedores de uma emissão.
     */
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
