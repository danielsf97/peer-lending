/**
 * Representa uma licitação.
 *
 */
public class Bid {

    private String investor;
    private String clientSession;
    private long value;
    private float rate;


    /**
     * Construtor parametrizado.
     *
     * @param investor      Investidor criador da licitação.
     * @param value         Valor da licitação.
     * @param rate          Taxa de juro requerida.
     * @param clientSession Pid do Ator Sessão do cliente.
     */
    public Bid(String investor, String clientSession, long value, float rate) {
        this.investor = investor;
        this.clientSession = clientSession;
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
     * Devolve o pid do ator sessão do cliente.
     *
     * @return o pid do ator sessão do cliente.
     */
    public String getClientSession(){
        return clientSession;
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
