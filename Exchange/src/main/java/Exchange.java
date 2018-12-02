
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.HashMap;

public class Exchange {

    public static class ExchangeData {
        public int socket_push_port;
        public int socket_pull_port;
        public ArrayList<String> companies;

        public ExchangeData(int socket_pull_port, int socket_push_port, ArrayList<String> companies) {
            this.socket_push_port = socket_push_port;
            this.socket_pull_port = socket_pull_port;
            this.companies = companies;
        }
    }

    private static final HashMap<Integer, ExchangeData> exchanges = new HashMap<Integer, ExchangeData>(){{
        //Exchange 0
        put(0, new ExchangeData(1241, 1251,
                        new ArrayList<String>(){{
                            add("empA");
                        }}));
        //Exchange 1
        put(1, new ExchangeData(1242, 1252,
                new ArrayList<String>(){{
                    add("empA");
                }}));
        //Exchange 2
        put(2, new ExchangeData(1243, 1253,
                new ArrayList<String>(){{
                    add("empA");
                }}));
    }};

    //private HashMap<String, Auction> active_auctions;
    //private HashMap<String, Emission> active_emissions;

    private ZMQ.Context frontend_context;
    private ZMQ.Socket frontend_socket_pull;
    private ZMQ.Socket frontend_socket_push ;

    public Exchange(ExchangeData data){
        this.frontend_context = ZMQ.context(1);
        this.frontend_socket_pull = this.frontend_context.socket(ZMQ.PULL);
        this.frontend_socket_push = this.frontend_context.socket(ZMQ.PUSH);
        this.frontend_socket_pull.bind("tcp://localhost:" + data.socket_pull_port);
        this.frontend_socket_push.bind("tcp://localhost:" + data.socket_push_port);
    }

    private void start() {

    }

    public static void main(String[] args) {
        int exchange_id = Integer.parseInt(args[0]);
        Exchange exchange = new Exchange(exchanges.get(exchange_id));
        exchange.start();
    }

}

