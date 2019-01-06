/**
 * Representa uma subscrição de uma emissão.
 *
 */
public class Subscription {
    private String investor;
    private long value;


    /**
     * Construtor parametrizado.
     *
     * @param investor      Investidor responsável pela subscrição.
     * @param value         Valor da subscrição.
     */
    public Subscription(String investor, long value) {
        this.investor = investor;
        this.value = value;
    }


    /**
     * Devolve o investidor responsável pela subscrição.
     *
     * @return o investidor responsável pela subscrição.
     */
    public String getInvestor() {
        return investor;
    }


    /**
     * Devolve o valor de uma subscrição.
     *
     * @return o valor de uma subscrição.
     */
    public long getValue() {
        return value;
    }
}
