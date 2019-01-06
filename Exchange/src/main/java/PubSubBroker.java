import org.zeromq.ZMQ;

/**
 * Broker para ligação entre os sockets do tipo sub do Cliente
 * e do tipo Pub das Exchanges.
 *
 */
public class PubSubBroker {

    public static void main(String[] args) {
        int pubs_port = 12347; // Integer.parseInt(args[0]);
        int subs_port = 12346; // Integer.parseInt(args[1]);
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket pubs = context.socket(ZMQ.XSUB);
        ZMQ.Socket subs = context.socket(ZMQ.XPUB);
        pubs.bind("tcp://*:" + pubs_port);
        subs.bind("tcp://*:" + subs_port);
        ZMQ.proxy(pubs, subs, null);
    }
}