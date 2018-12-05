
import org.zeromq.ZMQ;
import protos.Protos;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;

public class Exchange {

    public static class ExchangeData {
        public int socket_push_port;
        public int socket_pull_port;
        public Map<String, Company> companies;

        public ExchangeData(int socket_pull_port, Map<String, Company> companies) {
            this.socket_pull_port = socket_pull_port;
            this.companies = companies;
        }
    }

    private static final HashMap<Integer, ExchangeData> exchanges = new HashMap<>() {
        {
            // Exchange 0
            put(0, new ExchangeData(1234, new HashMap<>() {
                {
                    put("empA", new Company("empA"));
                }
            }));
            // Exchange 1
            put(1, new ExchangeData(1235, new HashMap<>() {
                {
                    put("empB", new Company("empB"));
                }
            }));
            // Exchange 2
            put(2, new ExchangeData(1236, new HashMap<>() {
                {
                    put("empC", new Company("empC"));
                }
            }));
        }
    };

    private final int socket_push_port = 1222;
    private final int delayTime = 10;
    private ScheduledExecutorService scheduler;
    private ZMQ.Context context;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;
    public Map<String, Company> companies;
    public DirectoryManager directoryManager;

    public Exchange(ExchangeData data) {
        this.context = ZMQ.context(1);
        this.push = this.context.socket(ZMQ.PUSH);
        this.pull = this.context.socket(ZMQ.PULL);
        this.push.connect("tcp://*:" + socket_push_port);
        this.pull.bind("tcp://*:" + data.socket_pull_port);
        this.companies = data.companies;
        this.directoryManager = new DirectoryManager();
        this.scheduler = Executors.newScheduledThreadPool(1);
    }

    private void start() {
        try {
            while (true) {
                byte[] b = this.pull.recv();

                Protos.MessageWrapper msg = Protos.MessageWrapper.parseFrom(b);

                switch(msg.getInnerMessageCase()) {
                    case COMPANYACTIONREQ:
                        Protos.CompanyActionReq createReq = msg.getCompanyactionreq();
                        long value = createReq.getValue();
                        float rate;
                        Company c = companies.get(createReq.getClient()); //o client é a company
                        try {
                            if (createReq.getReqType() == Protos.CompanyActionReq.RequestType.AUCTION) {
                                rate = createReq.getMaxRate();
                                Auction a = new Auction(value, rate);
                                c.setActiveAuction(a);
                                directoryManager.postAuction(a, c.getName());
                                scheduler.schedule(new ScheduledExecutor(c), delayTime, TimeUnit.MINUTES);
                            }
                            else {
                                Emission e = new Emission(value);
                                c.setActiveEmission(e);
                                directoryManager.postEmission(e, c.getName());
                            }
                        }
                        catch(Exception e) {

                        }
                        break;
                }
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }finally {
            scheduler.shutdown();
        }
    }

    public static void main(String[] args) {
        int exchange_id = Integer.parseInt(args[0]);
        Exchange exchange = new Exchange(exchanges.get(exchange_id));
        exchange.start();
    }

}