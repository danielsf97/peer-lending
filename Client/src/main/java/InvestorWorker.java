package client;

import org.zeromq.ZMQ;
import utils.Menu;
import utils.NetClient;
import utils.Utils;

import java.nio.channels.SocketChannel;

public class InvestorWorker extends Thread{
    private SocketChannel socket;
    private ZMQ.Socket sub;
    private Investor investor;
    private String name;
    private Menu menu;

    public InvestorWorker(SocketChannel socket, Investor investor, ZMQ.Socket sub) {
        this.socket = socket;
        this.sub = sub;
        this.investor = investor;
        this.name = investor.getName();
        this.menu = new Menu("Menu Investidor");
        menu.add("Licitar em leilão");
        menu.add("Subscrever empréstimo a taxa fixa");
        menu.add("Subscrever notificação");
        menu.add("Desativar notificação");
        menu.add("Ver lista de empresas");
        menu.add("Ver leilões ativos");
        menu.add("Ver emissões ativas");
        menu.add("Ver histórico de leilões de empresa");
        menu.add("Ver histórico de emissões de empresa");
    }

    public void run() {
        int option;
        
        do {
            int nNotifications = investor.getNumNotifications();
            int nAsyncMessages = investor.getNumAsyncMessages();
            menu.add("Ver " + nNotifications + " notificações" );
            menu.add("Ver " + nAsyncMessages + " resultados" );

            menu.execute();
            option = menu.getOption();
            processOption(option);

            menu.removeLast(2);
        } while(investor.isLoggedIn());

        System.out.println("Logged out!!");
    }

    private void processOption(int option) {
        switch(option) {
            case 0: logout();
                break;
            case 1: bid_on_auction();
                break;
            case 2: subscribe_emission();
                break;
            case 3: subscribe_company();
                break;
            case 4: unsubscribe_company();
                break;
            case 5: show_companies();
                break;
            case 6: show_active_auctions();
                break;
            case 7: show_active_emissions();
                break;
            case 8: show_company_auction_history();
                break;
            case 9: show_company_emission_history();
                break;
            case 10: readNotifications();
                break;
            case 11: readAsyncMessages();
                break;
        }
    }

    private void readAsyncMessages() {
        String asyncMessages = investor.getAsyncMessages();
        System.out.println(asyncMessages);
    }

    private void subscribe_company() {
        Menu m = new Menu("Subscrever Empresa");
        String comp = menu.readString("Empresa: ");

        sub.subscribe(comp);
        System.out.println("Notificações da empresa " + comp + " subscritas.\n");
    }

    private void unsubscribe_company() {
        Menu m = new Menu("Remover Subscrição de Empresa");
        String comp = menu.readString("Empresa: ");

        sub.subscribe(comp);
        System.out.println("Remoção das notificações da empresa " + comp + " completa.\n");
    }

    private void readNotifications() {
        String notifications = investor.getNotifications();
        System.out.println(notifications);
    }

    private void subscribe_emission() {
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

    private void bid_on_auction() {
        Menu m = new Menu("Licitar em Leilão");
        m.execute();
        String comp = m.readString("Empresa: ");
        long value = m.readLong("Valor: ");
        float rate = m.readFloat("Taxa: ");

        Protos.MessageWrapper req = createAuctionReq(comp, value, rate);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,investor);

        System.out.println("mensagem recebida");
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

    private void show_companies() {
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

    private void show_active_auctions() {
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

    private void show_active_emissions() {
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

    private void show_company_auction_history() {
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

    private void show_company_emission_history() {
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

    private Protos.MessageWrapper createEmissionReq(String comp, long value) {
        Protos.InvestorActionReq req = Protos.InvestorActionReq.newBuilder()
                .setValue(value)
                .setReqType(Protos.InvestorActionReq.RequestType.AUCTION)
                .setCompany(comp)
                .setClient(name)
                .build();
        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setInvestoractionreq(req).build();
    }

    public Protos.MessageWrapper createLogoutReq() {
        Protos.LogoutReq logoutMsg = Protos.LogoutReq.newBuilder()
                .setName(this.name)
                .build();
        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setLogoutreq(logoutMsg).build();
    }
}
