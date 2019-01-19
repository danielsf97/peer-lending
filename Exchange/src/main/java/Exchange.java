
import org.zeromq.ZMQ;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;


/**
 * Representa uma exchange.
 *
 */
public class Exchange {


    /**
     * Classe representativa os dados de uma exchange.
     *
     */
    public static class ExchangeData {
        int socketPullPort;
        int socketPushPort;
        Map<String, Company> companies;


        /**
         * Construtor parametrizado.
         *
         * @param socketPullPort  Porta do socket pull.
         * @param companies         Empresas pela qual a exchange está responsável.
         */
        ExchangeData(int socketPullPort, int socketPushPort, Map<String, Company> companies) {
            this.socketPullPort = socketPullPort;
            this.socketPushPort = socketPushPort;
            this.companies = companies;
        }
    }

    private static final HashMap<Integer, ExchangeData> exchanges = new HashMap<>() {
        {
            // Exchange 0
            put(0, new ExchangeData(1234, 1221, new HashMap<>() {
                {
                    put("empA", new Company("empA"));
                    put("empB", new Company("empB"));
                }
            }));
            // Exchange 1
            put(1, new ExchangeData(1235, 1222, new HashMap<>() {
                {
                    put("empC", new Company("empC"));
                }
            }));
            // Exchange 2
            put(2, new ExchangeData(1236, 1223, new HashMap<>() {
                {
                    put("empD", new Company("empD"));
                }
            }));
        }
    };

    private final int socketPubPort = 12347;
    private final int delayTime = 1;
    private ScheduledExecutorService scheduler;
    private ZMQ.Context context;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;
    private ZMQ.Socket pub;
    private Map<String, Company> companies;
    private DirectoryManager directoryManager;


    /**
     * Construtor parametrizado.
     *
     * @param data Dados da exchange como porta pull e empresas pelas quais
     *             está responsável.
     */
    public Exchange(ExchangeData data) {
        this.context = ZMQ.context(1);
        this.push = this.context.socket(ZMQ.PUSH);
        this.pull = this.context.socket(ZMQ.PULL);
        this.pub = this.context.socket(ZMQ.PUB);
        this.push.connect("tcp://localhost:" + data.socketPushPort);
        this.pull.bind("tcp://localhost:" + data.socketPullPort);
        this.pub.connect("tcp://localhost:" + socketPubPort);
        this.companies = data.companies;
        this.directoryManager = new DirectoryManager();
        this.scheduler = Executors.newScheduledThreadPool(1);
    }


    /**
     * Recebe os pedidos de clientes através do socket pull.
     *
     */
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


    /**
     * Trata de processar um pedido pela taxa fixa de uma próxima emissão
     * e de enviar a resposta ao cliente.
     *
     * @param msg   Mensagem com o pedido recebido.
     */
    private void processEmissionFixedRateReq(Protos.MessageWrapper msg) {

        Protos.EmissionFixedRateReq req = msg.getEmissionfixedratereq();

        String clientSession = msg.getClientSession();

        Company c = companies.get(req.getClient()); // O cliente é a empresa

        float rate = c.getEmissionRate();

        if(c.hasActiveAction()) rate = -1;

        Protos.MessageWrapper resp = createEmissionFixedRateResp(req.getClient(), rate, clientSession);

        this.push.send(resp.toByteArray());

    }


    /**
     * Trata de processar um pedido para uma ação de uma empresa e de enviar
     * a resposta ao cliente.
     *
     * @param msg   Mensagem com a resposta ao pedido.
     */
    private void processCompanyActionReq(Protos.MessageWrapper msg) {
        Protos.CompanyActionReq companyActionReq = msg.getCompanyactionreq();
        String clientSession = msg.getClientSession();
        long value = companyActionReq.getValue();
        float rate = companyActionReq.getMaxRate();
        String notification = null;
        Company c = companies.get(companyActionReq.getClient()); // o client é a company
        try {
            if (companyActionReq.getReqType() == Protos.CompanyActionReq.RequestType.AUCTION) {

                Auction a = new Auction(value, rate);
                c.setActiveAuction(a);

                directoryManager.postAuction(a, c.getName());
                scheduler.schedule(new ScheduledExecutor(c, push, pub, directoryManager), delayTime, TimeUnit.MINUTES);

                notification = "Leilao_" + c.getName() + ": Criação de Leilão, Montante: " + value + ", Taxa Max.: " + rate;

            }
            else {
                Emission e = new Emission(value,rate);
                c.setActiveEmission(e);

                directoryManager.postEmission(e, c.getName());
                scheduler.schedule(new ScheduledExecutor(c, push, pub, directoryManager), delayTime, TimeUnit.MINUTES);

                notification = "Emissao_" + c.getName() + ": Criação de Emissão, Montante: " + value + ", Taxa: " + rate;
            }

            Protos.MessageWrapper statusMsg = createCompanyActionResp(companyActionReq.getClient(), Protos.CompanyActionResp.Status.SUCCESS, clientSession);
            push.send(statusMsg.toByteArray());
            pub.send(notification);
        }
        catch(Exception e) {
            Protos.MessageWrapper statusMsg = createCompanyActionResp(companyActionReq.getClient(), Protos.CompanyActionResp.Status.INVALID, clientSession);
            push.send(statusMsg.toByteArray());
        }
    }


    /**
     * Trata de processar um pedido para uma ação de um investidor e de enviar
     * a resposta ao cliente.
     *
     * @param msg   Mensagem com a resposta ao pedido.
     */
    private void processInvestorActionReq(Protos.MessageWrapper msg) {
        Protos.InvestorActionReq investorActionReq = msg.getInvestoractionreq();
        String clientSession = msg.getClientSession();
        String notification = null;
        long value = investorActionReq.getValue();
        String cName = investorActionReq.getCompany();
        Company c = companies.get(cName);
        String client = investorActionReq.getClient();

        if(investorActionReq.getReqType() == Protos.InvestorActionReq.RequestType.AUCTION) {
            float rate = investorActionReq.getRate();
            Auction a = c.getActiveAuction();
            if(a == null) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.INVALID, clientSession);
                push.send(resp.toByteArray());
                return;
            }
            int stat = a.addBid(client, clientSession, value, rate);
            if(stat == -1) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.INVALID, clientSession);
                push.send(resp.toByteArray());
                return;
            }
            else if(stat == 1) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.REPLACED, clientSession);
                notification = "Leilao_" + c.getName() + ": Licitação Substituída, Montante: " + value + ", Taxa: " + rate;
                push.send(resp.toByteArray());
                return;
            }
            notification = "Leilao_" + c.getName() + ": Nova licitação em Leilão, Montante: " + value + ", Taxa: " + rate;
        }
        else if(investorActionReq.getReqType() == Protos.InvestorActionReq.RequestType.EMISSION) {
            Emission e = c.getActiveEmission();

            if(e == null || value > e.getValue()) {
                Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.INVALID, clientSession);
                push.send(resp.toByteArray());
                return;
            }
            e.addSubscription(client, clientSession,value);
            notification = "Emissao_" + c.getName() + ": Nova Subscrição em Emissão, Montante: " + value;

        }

        Protos.MessageWrapper resp = createInvestorActionResp(client, Protos.InvestorActionResp.Status.CONFIRMED, clientSession);
        push.send(resp.toByteArray());
        pub.send(notification);
    }


    /**
     * Cria uma resposta a um pedido acerca da taxa da próxima emissão.
     *
     * @param client    Cliente que fez o pedido.
     * @param rate      Taxa da próxima emissão.
     * @return          Resposta criada.
     */
    private Protos.MessageWrapper createEmissionFixedRateResp(String client, float rate, String clientSession) {
        Protos.EmissionFixedRateResp emissionRateMsg = Protos.EmissionFixedRateResp.newBuilder()
                .setClient(client)
                .setRate(rate)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setClientSession(clientSession)
                .setEmissionfixedrateresp(emissionRateMsg)
                .build();
    }


    /**
     * Cria uma resposta a uma ação de uma empresa (criação de leilão/emissão, etc).
     *
     * @param client    Cliente que fez o pedido.
     * @param status    Estado indicativo do sucesso/insucesso da ação.
     * @return          Resposta criada.
     */
    private Protos.MessageWrapper createCompanyActionResp(String client, Protos.CompanyActionResp.Status status, String clientSession) {
        Protos.CompanyActionResp companyActionResp = Protos.CompanyActionResp.newBuilder()
                .setClient(client)
                .setStatus(status)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setClientSession(clientSession)
                .setCompanyactionresp(companyActionResp)
                .build();
    }


    /**
     * Cria uma resposta a uma ação de um investidor (criação de subscrição/licitação, etc).
     *
     * @param client    Cliente que fez o pedido.
     * @param status    Estado indicativo do sucesso/insucesso da ação.
     * @return          Resposta criada.
     */
    private Protos.MessageWrapper createInvestorActionResp(String client, Protos.InvestorActionResp.Status status, String clientSession) {
        Protos.InvestorActionResp investorActionResp = Protos.InvestorActionResp.newBuilder()
                .setClient(client)
                .setStatus(status)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setClientSession(clientSession)
                .setInvestoractionresp(investorActionResp)
                .build();
    }


    /**
     * Cria e começa a execução de uma exchange.
     *
     * @param args
     */
    public static void main(String[] args) {
        int exchangeId = Integer.parseInt(args[0]);
        Exchange exchange = new Exchange(exchanges.get(exchangeId));
        exchange.start();
    }
}