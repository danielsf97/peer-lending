
import org.zeromq.ZMQ;
import utils.Menu;
import utils.NetClient;

import java.nio.channels.SocketChannel;


/**
 * Classe responsável por lidar com as ações tomadas por um investidor.
 *
 */
public class InvestorWorker extends Thread{
    private SocketChannel socket;
    private ZMQ.Socket sub;
    private Investor investor;
    private String name;
    private Menu menu;


    /**
     * Construtor parametrizado.
     *
     * @param socket        Socket.
     * @param investor      Investidor.
     * @param sub           Socket sub para notificações.
     */
    public InvestorWorker(SocketChannel socket, Investor investor, ZMQ.Socket sub) {
        this.socket = socket;
        this.sub = sub;
        this.investor = investor;
        this.name = investor.getName();
        this.menu = new Menu("Menu Investidor");
        this.menu.add("Licitar em leilão");
        this.menu.add("Subscrever empréstimo a taxa fixa");
        this.menu.add("Subscrever notificação");
        this.menu.add("Desativar notificação");
        this.menu.add("Ver lista de empresas");
        this.menu.add("Ver leilões ativos");
        this.menu.add("Ver emissões ativas");
        this.menu.add("Ver histórico de leilões de empresa");
        this.menu.add("Ver histórico de emissões de empresa");
    }


    /**
     * Mostra o menu do investidor enquanto este estiver autenticado,
     * esperando pelas opções escolhidas por esta.
     *
     */
    public void run() {
        int option;
        
        do {
            int nNotifications = investor.getNumNotifications();
            int nAsyncMessages = investor.getNumAsyncMessages();
            this.menu.add("Ver " + nNotifications + " notificações" );
            this.menu.add("Ver " + nAsyncMessages + " resultados" );
            this.menu.add("Atualizar menu");

            this.menu.execute();
            option = this.menu.getOption();
            processOption(option);

            this.menu.removeLast(3);
        } while(investor.isLoggedIn());

        System.out.println("Logged out!");
    }


    /**
     * Efetua uma ação dependendo de uma opção escolhida
     * pelo utilizador (investidor).
     *
     * @param option    Opção escolhida.
     */
    private void processOption(int option) {
        switch(option) {
            case 0: logout();
                break;
            case 1: bidOnAuction();
                break;
            case 2: subscribeEmission();
                break;
            case 3: subscribeCompany();
                break;
            case 4: unsubscribeCompany();
                break;
            case 5: showCompanies();
                break;
            case 6: showActiveAuctions();
                break;
            case 7: showActiveEmissions();
                break;
            case 8: showCompanyAuctionHistory();
                break;
            case 9: showCompanyEmissionHistory();
                break;
            case 10: readNotifications();
                break;
            case 11: readAsyncMessages();
                break;
            case 12:
                break;
        }
    }


    /**
     * Lê mensagens assíncronas destinadas ao investidor (notificações).
     *
     */
    private void readAsyncMessages() {
        String asyncMessages = investor.getAsyncMessages();
        System.out.println(asyncMessages);
    }


    /**
     * Possibilita a um investidor subscrever as notificações de uma empresa.
     *
     */
    private void subscribeCompany() {
        Menu m = new Menu("Subscrever Empresa");

        m.execute();

        String comp = menu.readString("Empresa: ");

        sub.subscribe(comp);

        System.out.println("Notificações da empresa " + comp + " subscritas.\n");
    }


    /**
     * Possibilita a um investidor cancelar a subscrição de notificações de uma empresa.
     *
     */
    private void unsubscribeCompany() {
        Menu m = new Menu("Remover Subscrição de Empresa");

        m.execute();

        String comp = menu.readString("Empresa: ");

        sub.subscribe(comp);
        System.out.println("Remoção das notificações da empresa " + comp + " completa.\n");
    }


    /**
     * Lê notificações destinadas ao investidor.
     *
     */
    private void readNotifications() {
        String notifications = investor.getNotifications();
        System.out.println(notifications);
    }


    /**
     * Possibilita ao investidor fazer subscrição a uma emissão.
     *
     */
    private void subscribeEmission() {
        Menu m = new Menu("Subscrever empréstimo a taxa fixa");
        m.execute();
        String comp = m.readString("Empresa: ");
        long value = m.readLong("Valor: ");

        Protos.MessageWrapper req = createEmissionReq(comp, value);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,investor);

        Protos.InvestorActionResp investorResp = null;

        if(resp.hasInvestoractionresp())
            investorResp = resp.getInvestoractionresp();
        else if(resp.hasErrormsg())
            System.out.println(resp.getErrormsg().getError());

        if(investorResp != null) {
            switch (investorResp.getStatus()){
                case CONFIRMED:
                    System.out.println("Empréstimo Confirmado!!");
                    break;
                case ENDED:
                    System.out.println("Não se aceitam mais empréstimos!!");
                    break;
                case INVALID:
                    System.out.println("Empréstimo Inválido!!");
                    break;
            }
        }
    }


    /**
     * Possiblita ao investidor fazer uma licitação num leilão.
     *
     */
    private void bidOnAuction() {
        Menu m = new Menu("Licitar em Leilão");
        m.execute();
        String comp = m.readString("Empresa: ");
        long value = m.readLong("Valor: ");
        float rate = m.readFloat("Taxa: ");

        Protos.MessageWrapper req = createAuctionReq(comp, value, rate);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,investor);

        Protos.InvestorActionResp investorResp = null;

        if(resp.hasInvestoractionresp())
            investorResp = resp.getInvestoractionresp();
        else if(resp.hasErrormsg())
            System.out.println(resp.getErrormsg().getError());

        if(investorResp != null) {
            switch (investorResp.getStatus()){
                case CONFIRMED:
                    System.out.println("Licitação Confirmada!!");
                    break;
                case REPLACED:
                    System.out.println("A sua licitação anterior foi substituida!!");
                    break;
                case ENDED:
                    System.out.println("Licitação Inválida. Leilão Terminado!!");
                    break;
                case INVALID:
                    System.out.println("Licitação Inválida!!");
                    break;
            }
        }
    }


    /**
     * Possibilita a um investidor ver as empresas existentes.
     *
     */
    private void showCompanies() {
        try {
            System.out.println(NetClient.getCompanies());
        }
        catch(RuntimeException e) {
            System.out.println(e.getCause());
        }
        catch(Exception e) {
            e.printStackTrace();
        }
        try {
            System.in.read();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Possibilita a um investidor ver a lista de leilões ativos.
     *
     */
    private void showActiveAuctions() {
        try {
            System.out.println(NetClient.getActiveAuctions());
        }
        catch(RuntimeException e) {
            System.out.println(e.getMessage());
        }
        catch(Exception e) {
            e.printStackTrace();
        }
        try {
            System.in.read();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Possibilita a um investidor ver a lista de emissões ativas.
     *
     */
    private void showActiveEmissions() {
        try {
            System.out.println(NetClient.getActiveEmissions());
        }
        catch(RuntimeException e ) {
            System.out.println(e.getMessage());
        }
        catch(Exception e) {
            e.printStackTrace();
        }
        try {
            System.in.read();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Possibilita a um investidor ver o histórico de leilões para uma
     * determinada empresa.
     *
     */
    private void showCompanyAuctionHistory() {
        try {
            String emp = menu.readString("Nome da empresa: ");
            System.out.println(NetClient.getCompanyAuctionHistory(emp));
        }
        catch(RuntimeException e ) {
            System.out.println(e.getMessage());
        }
        catch(Exception e) {
            e.printStackTrace();
        }
        try {
            System.in.read();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }


    /**
     * Possibilita a um investidor ver o histórico de emissões para uma
     * determinada empresa.
     *
     */
    private void showCompanyEmissionHistory() {
        try {
            String emp = menu.readString("Nome da empresa: ");
            System.out.println(NetClient.getCompanyEmissionHistory(emp));
        }
        catch(RuntimeException e ) {
            System.out.println(e.getMessage());
        }
        catch(Exception e) {
            e.printStackTrace();
        }
        try {
            System.in.read();
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Possibilita a um investidor efetuar logout.
     *
     */
    private void logout() {
        Protos.MessageWrapper req = createLogoutReq();
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,investor);

        Protos.LogoutResp logoutResp = null;
        if(resp.hasLogoutresp())
            logoutResp = resp.getLogoutresp();

        if(logoutResp != null){
            if (logoutResp.getStatus() == Protos.LogoutResp.Status.SUCCESS)
                investor.logout();
            else{
                System.out.println("Erro ao fazer logout!");
            }
        }
    }


    /**
     * Cria um pedido de licitação de um leilão.
     *
     * @param comp      Empresa da qual é o leilão.
     * @param value     Montante da licitação.
     * @param rate      Taxa da licitação.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createAuctionReq(String comp, long value, float rate) {
        Protos.InvestorActionReq req = Protos.InvestorActionReq.newBuilder()
                .setRate(rate)
                .setValue(value)
                .setReqType(Protos.InvestorActionReq.RequestType.AUCTION)
                .setCompany(comp)
                .setClient(name)
                .build();
        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setInvestoractionreq(req).build();
    }

    /**
     * Cria um pedido de subscrição de uma emissão.
     *
     * @param comp      Empresa da qual é a emissão.
     * @param value     Montante da subscrição.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createEmissionReq(String comp, long value) {
        Protos.InvestorActionReq req = Protos.InvestorActionReq.newBuilder()
                .setValue(value)
                .setReqType(Protos.InvestorActionReq.RequestType.EMISSION)
                .setCompany(comp)
                .setClient(name)
                .build();
        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setInvestoractionreq(req).build();
    }


    /**
     * Cria mensagem de pedido de logout.
     *
     * @return  Mensagem criada.
     */
    public Protos.MessageWrapper createLogoutReq() {
        Protos.LogoutReq logoutMsg = Protos.LogoutReq.newBuilder()
                .setName(this.name)
                .build();
        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setLogoutreq(logoutMsg).build();
    }
}
