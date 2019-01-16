/**
 * Representa uma subscrição de uma emissão.
 *
 */
public class Subscription {
    private String investor;
    private String clientSession;
    private long value;


    /**
     * Construtor parametrizado.
     *
     * @param investor      Investidor responsável pela subscrição.
     * @param value         Valor da subscrição.
     * @param clientSession Pid do Ator Sessão do cliente.
     */
    public Subscription(String investor, String clientSession, long value) {
        this.investor = investor;
        this.value = value;
        this.clientSession = clientSession;
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
     * Devolve o pid do ator sessão do cliente.
     *
     * @return o pid do ator sessão do cliente.
     */
    public String getClientSession(){
        return clientSession;
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
