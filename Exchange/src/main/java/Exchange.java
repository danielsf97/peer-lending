
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Exchange {

    public static class ExchangeData {
        public int socket_push_port;
        public int socket_pull_port;
        public List<String> companies;

        public ExchangeData(int socket_pull_port, int socket_push_port, ArrayList<String> companies) {
            this.socket_push_port = socket_push_port;
            this.socket_pull_port = socket_pull_port;
            this.companies = companies;
        }
    }

    private static final HashMap<Integer, ExchangeData> exchanges = new HashMap<>() {
        {
            // Exchange 0
            put(0, new ExchangeData(1241, 1251, new ArrayList<>() {
                {
                    add("empA");
                }
            }));
            // Exchange 1
            put(1, new ExchangeData(1242, 1252, new ArrayList<>() {
                {
                    add("empB");
                }
            }));
            // Exchange 2
            put(2, new ExchangeData(1243, 1253, new ArrayList<>() {
                {
                    add("empC");
                }
            }));
        }
    };

    private ZMQ.Context context;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;

    public Exchange(ExchangeData data) {
        this.context = ZMQ.context(1);
        this.push = this.context.socket(ZMQ.PUSH);
        this.pull = this.context.socket(ZMQ.PULL);
        this.push.bind("tcp://*:" + data.socket_push_port);
        this.pull.bind("tcp://*:" + data.socket_pull_port);
    }

    private void start() {
        try {
            while (true) {
                byte[] b = this.pull.recv();

                Protos.MessageWrapper msg = Protos.MessageWrapper.parseFrom(b);
                switch(msg.getInnerMessageCase()) {
                    case AUCTIONREQ:
                        Protos.AuctionReq auctionreq = msg.getAuctionreq();
                        break;
                }
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int exchange_id = Integer.parseInt(args[0]);
        Exchange exchange = new Exchange(exchanges.get(exchange_id));
        exchange.start();
    }

}