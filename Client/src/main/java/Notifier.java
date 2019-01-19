import org.zeromq.ZMQ;


/**
 * Responsável por receber as notificações através de subscrições
 * e enviá-las ao investidor respetivo.
 *
 */
public class Notifier extends Thread {

    private Investor investor;
    private ZMQ.Socket sub;


    /**
     * Construtor parametrizado.
     * @param investor      Investidor para o qual se destinam as notificações.
     * @param sub           Socket sub.
     */
    public Notifier(Investor investor, ZMQ.Socket sub) {
        this.investor = investor;
        this.sub = sub;
    }


    /**
     * Corre o notifier, recebendo as notificações e enviando-as
     * ao investidor.
     *
     */
    public void run() {
        String msg;

        while(true) {

            byte[] msgB = sub.recv();
            msg = new String(msgB);
            investor.addNotification(msg.split("_")[1]);
        }
    }
}
