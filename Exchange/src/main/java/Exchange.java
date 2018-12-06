
import org.zeromq.ZMQ;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;

public class Exchange {

    public static class ExchangeData {
        int socket_pull_port;
        Map<String, Company> companies;

        ExchangeData(int socket_pull_port, Map<String, Company> companies) {
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
                    put("empB", new Company("empB"));
                }
            }));
            // Exchange 1
            put(1, new ExchangeData(1235, new HashMap<>() {
                {
                    put("empC", new Company("empC"));
                }
            }));
            // Exchange 2
            put(2, new ExchangeData(1236, new HashMap<>() {
                {
                    put("empD", new Company("empD"));
                }
            }));
        }
    };

    private final int socket_push_port = 1222;
    private final int socket_pub_port = 12347;
    private final int delayTime = 10;
    private ScheduledExecutorService scheduler;
    private ZMQ.Context context;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;
    private ZMQ.Socket pub;
    public Map<String, Company> companies;
    public DirectoryManager directoryManager;

    public Exchange(ExchangeData data) {
        this.context = ZMQ.context(1);
        this.push = this.context.socket(ZMQ.PUSH);
        this.pull = this.context.socket(ZMQ.PULL);
        this.pub = this.context.socket(ZMQ.PUB);
        this.push.connect("tcp://localhost:" + socket_push_port);
        this.pull.bind("tcp://localhost:" + data.socket_pull_port);
        this.pub.connect("tcp://localhost:" + socket_pub_port);
        this.companies = data.companies;
        this.directoryManager = new DirectoryManager();
        this.scheduler = Executors.newScheduledThreadPool(1);
    }

    private void start() {
        try {
            while(true) {
                byte[] b = this.pull.recv();

                Protos.MessageWrapper msg = Protos.MessageWrapper.parseFrom(b);

                switch(msg.getInnerMessageCase()) {
                    case EMISSIONFIXEDRATEREQ:
                        processEmissionFixedRateReq(msg);
                        break;
                    case COMPANYACTIONREQ:
                        processCompanyActionReq(msg);
                        break;
                    case INVESTORACTIONREQ:
                        processInvestorActionReq(msg);
                        break;
                }
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }
        finally {
            scheduler.shutdown();
        }
    }

    private void processEmissionFixedRateReq(Protos.MessageWrapper msg) {

        Protos.EmissionFixedRateReq req = msg.getEmissionfixedratereq();

        Company c = companies.get(req.getClient()); // O cliente é a empresa

        float rate = c.getEmissionRate();

        if(c.hasActiveAction()) rate = -1;

        Protos.MessageWrapper resp = createEmissionFixedRateResp(req.getClient(), rate);

        this.push.send(resp.toByteArray());

    }

    private void processCompanyActionReq(Protos.MessageWrapper msg) {
        Protos.CompanyActionReq companyActionReq = msg.getCompanyactionreq();
        long value = companyActionReq.getValue();
        float rate;
        Company c = companies.get(companyActionReq.getClient()); // o client é a company
        try {
            if (companyActionReq.getReqType() == Protos.CompanyActionReq.RequestType.AUCTION) {
                rate = companyActionReq.getMaxRate();
                Auction a = new Auction(value, rate);
                c.setActiveAuction(a);

                directoryManager.postAuction(a, c.getName());
                scheduler.schedule(new ScheduledExecutor(c, push, pub, directoryManager), delayTime, TimeUnit.MINUTES);

            }
            else {
                Emission e = new Emission(value);
                c.setActiveEmission(e);

                directoryManager.postEmission(e, c.getName());
                scheduler.schedule(new ScheduledExecutor(c, push, pub, directoryManager), delayTime, TimeUnit.MINUTES);
            }

            Protos.MessageWrapper statusMsg = createCompanyActionResp(companyActionReq.getClient(), Protos.CompanyActionResp.Status.SUCCESS);
            push.send(statusMsg.toByteArray());
        }
        catch(Exception e) {

            Protos.MessageWrapper statusMsg = createCompanyActionResp(companyActionReq.getClient(), Protos.CompanyActionResp.Status.INVALID);
            push.send(statusMsg.toByteArray());
        }
    }

    private void processInvestorActionReq(Protos.MessageWrapper msg) {
        Protos.InvestorActionReq investorActionReq = msg.getInvestoractionreq();

        long value = investorActionReq.getValue();
        String cName = investorActionReq.getCompany();
        Company c = companies.get(cName);
        String client = investorActionReq.getClient();

        if(investorActionReq.getReqType() == Protos.InvestorActionReq.RequestType.AUCTION) {
            float rate = investorActionReq.getRate();
            Auction a = c.getActiveAuction();
            if(a == null) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.INVALID);
                push.send(resp.toByteArray());
                return;
            }
            int stat = a.addBid(client, value, rate);
            if(stat == -1) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.INVALID);
                push.send(resp.toByteArray());
                return;
            }
            else if(stat == 1) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.REPLACED);
                push.send(resp.toByteArray());
                return;
            }
        }
        else if(investorActionReq.getReqType() == Protos.InvestorActionReq.RequestType.EMISSION) {
            Emission e = c.getActiveEmission();
            if(e == null) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.INVALID);
                push.send(resp.toByteArray());
                return;
            }
            e.addSubscription(client, value);
        }

        Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.CONFIRMED);
        push.send(resp.toByteArray());
    }

    private Protos.MessageWrapper createEmissionFixedRateResp(String client, float rate) {
        Protos.EmissionFixedRateResp emissionRateMsg = Protos.EmissionFixedRateResp.newBuilder()
                .setClient(client)
                .setRate(rate)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setEmissionfixedrateresp(emissionRateMsg)
                .build();
    }

    private Protos.MessageWrapper createCompanyActionResp(String client, Protos.CompanyActionResp.Status status) {
        Protos.CompanyActionResp companyActionResp = Protos.CompanyActionResp.newBuilder()
                .setClient(client)
                .setStatus(status)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.ASYNC)
                .setCompanyactionresp(companyActionResp)
                .build();
    }

    private Protos.MessageWrapper createInvestorActionResp(String client, Protos.InvestorActionResp.Status status) {
        Protos.InvestorActionResp investorActionResp = Protos.InvestorActionResp.newBuilder()
                .setClient(client)
                .setStatus(status)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.ASYNC)
                .setInvestoractionresp(investorActionResp)
                .build();
    }

    public static void main(String[] args) {
        int exchange_id = Integer.parseInt(args[0]);
        Exchange exchange = new Exchange(exchanges.get(exchange_id));
        exchange.start();
    }
}