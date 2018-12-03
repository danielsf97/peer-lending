
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Exchange {

    public static class ExchangeData {
        public int socket_push_port;
        public int socket_pull_port;
        public Map<String, Company> companies;

        public ExchangeData(int socket_pull_port, int socket_push_port, Map<String, Company> companies) {
            this.socket_push_port = socket_push_port;
            this.socket_pull_port = socket_pull_port;
            this.companies = companies;
        }
    }

    private static final HashMap<Integer, ExchangeData> exchanges = new HashMap<>() {
        {
            // Exchange 0
            put(0, new ExchangeData(1241, 1251, new HashMap<>() {
                {
                    put("empA", new Company("empA"));
                }
            }));
            // Exchange 1
            put(1, new ExchangeData(1242, 1252, new HashMap<>() {
                {
                    put("empB", new Company("empB"));
                }
            }));
            // Exchange 2
            put(2, new ExchangeData(1243, 1253, new HashMap<>() {
                {
                    put("empC", new Company("empC"));
                }
            }));
        }
    };

    private ZMQ.Context context;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;
    public Map<String, Company> companies;
    public DirectoryManager directoryManager;

    public Exchange(ExchangeData data) {
        this.context = ZMQ.context(1);
        this.push = this.context.socket(ZMQ.PUSH);
        this.pull = this.context.socket(ZMQ.PULL);
        this.push.bind("tcp://*:" + data.socket_push_port);
        this.pull.bind("tcp://*:" + data.socket_pull_port);
        this.companies = data.companies;
        this.directoryManager = new DirectoryManager();
    }

    private void start() {
        try {
            while (true) {
                byte[] b = this.pull.recv();

                Protos.MessageWrapper msg = Protos.MessageWrapper.parseFrom(b);
                switch(msg.getInnerMessageCase()) {
                    case CREATEREQ:
                        Protos.CreateReq createReq = msg.getCreateReq();
                        long value = createReq.getValue();
                        float rate = createReq.getRate();
                        Company c = companies.get(createReq.getCompany());
                        try {
                            if (createReq.getType() == Protos.CreateReq.Type.AUCTION) {
                                Auction a = new Auction(value, rate);
                                c.setActiveAuction(a);
                            }
                            else {
                                Emission e = new Emission(value, rate);
                                c.setActiveEmission(e);
                            }
                        }
                        catch(Exception e) {
                            System.out.println(e.getMessage());
                            // TODO: mudar isto, deverá informar-se a empresa de que
                            // TODO: não é possível fazer o leilão/emissão
                        }
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