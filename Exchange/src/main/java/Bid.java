/**
 * Representa uma licitação.
 *
 */
public class Bid {

    private String investor;
    private long value;
    private float rate;


    /**
     * Construtor parametrizado.
     *
     * @param investor      Investidor criador da licitação.
     * @param value         Valor da licitação.
     * @param rate          Taxa de juro requerida.
     */
    public Bid(String investor, long value, float rate) {
        this.investor = investor;
        this.value = value;
        this.rate = rate;
    }


    /**
     * Devolve o investidor de uma licitação.
     *
     * @return o investidor de uma licitação.
     */
    public String getInvestor() {
        return investor;
    }


    /**
     * Devolve o valor de uma licitação.
     *
     * @return o valor de uma licitação.
     */
    public long getValue() {
        return value;
    }


    /**
     * Devolve a taxa de uma licitação.
     *
     * @return a taxa de uma licitação.
     */
    public float getRate() {
        return rate;
    }
}
