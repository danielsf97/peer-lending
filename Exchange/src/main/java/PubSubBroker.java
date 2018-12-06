import org.zeromq.ZMQ;

public class PubSubBroker {
    public static void main(String[] args) {
        int pubs_port = 12347; // Integer.parseInt(args[0]);
        int subs_port = 12346; // Integer.parseInt(args[1]);
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket pubs = context.socket(ZMQ.XSUB);
        ZMQ.Socket subs = context.socket(ZMQ.XPUB);
        System.out.println(pubs.bind("tcp://*:" + args[0]));
        System.out.println(subs.bind("tcp://*:" + args[1]));
        ZMQ.proxy(pubs, subs, null);
    }
}
