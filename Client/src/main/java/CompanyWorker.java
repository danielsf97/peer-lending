import org.zeromq.ZMQ;
import utils.Menu;

import java.io.IOException;
import java.nio.channels.SocketChannel;


/**
 * Classe responsável por lidar com as ações tomadas por uma empresa.
 *
 */
public class CompanyWorker extends Thread {

    private SocketChannel socket;
    private ZMQ.Socket sub;
    private Company company;
    private String name;
    private Menu menu;


    /**
     * Construtor parametrizado.
     *
     * @param socket        Socket.
     * @param company       Empresa.
     */
    public CompanyWorker(SocketChannel socket, Company company) {
        this.socket = socket;
        this.company = company;
        this.name = company.getName();
        this.menu = new Menu("Menu Empresa");
        this.menu.add("Criar leilão");
        this.menu.add("Criar emissão");
    }


    /**
     * Mostra o menu da empresa enquanto esta estiver autenticada,
     * esperando pelas opções escolhidas por esta.
     *
     */
    public void run() {
        int option;

        do {

            int nAsyncMessages = company.getNumAsyncMessages();

            menu.add("Ver " + nAsyncMessages + " resultados" );
            menu.add("Atualizar menu");

            menu.execute();
            option = menu.getOption();
            processOption(option);

            menu.removeLast(2);

        } while(company.isLoggedIn());

        System.out.println("Logged out!");
    }


    /**
     * Efetua uma ação dependendo de uma opção escolhida
     * pelo utilizador (empresa).
     *
     * @param option    Opção escolhida.
     */
    private void processOption(int option) {
        switch(option) {
            case 0: logout();
                break;
            case 1: createAuction();
                break;
            case 2: createEmission();
                break;
            case 3: readAsyncMessages();
                break;
            case 4:
                break;
        }
    }


    /**
     * Lê mensagens assíncronas destinadas à empresa (notificações).
     *
     */
    private void readAsyncMessages() {
        String asyncMessages = company.getAsyncMessages();
        System.out.println(asyncMessages);
    }


    /**
     * Efetua o logout de uma empresa.
     *
     */
    private void logout(){
        Protos.MessageWrapper req = createLogoutReq();
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,company);

        Protos.LogoutResp logoutResp = null;
        if(resp.hasLogoutresp())
            logoutResp = resp.getLogoutresp();

        try {
            if (logoutResp != null) {
                if (logoutResp.getStatus() == Protos.LogoutResp.Status.SUCCESS) {
                    socket.close();
                    company.logout();
                } else {
                    System.out.println("Erro ao fazer logout!");
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    /**
     * Possibilita a uma empresa criar uma emissão, mediante a especificação
     * do seu valor. É necessário antes disto enviar um pedido de forma a
     * descobrir a taxa fixa desta emissão.
     *
     */
    private void createEmission() {

        float rate = getEmissionFixedRate();

        if(rate == -1) {
            System.out.println("Não é possível criar emissão!");
            return;
        }

        Menu m = new Menu("Criar Emissão a Taxa Fixa de " + String.format("%.2f", rate*100) + "%");
        m.add("Sim");
        m.execute();

        if(m.getOption() == 0) return;

        long value = m.readLong("Valor: ");

        Protos.MessageWrapper req = createEmissionReq(value, rate);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,company);

        Protos.CompanyActionResp companyResp = null;

        if(resp.hasCompanyactionresp())
            companyResp = resp.getCompanyactionresp();
        else if(resp.hasErrormsg())
            System.out.println(resp.getErrormsg().getError());

        if(companyResp != null) {
            switch (companyResp.getStatus()) {
                case SUCCESS:
                    System.out.println("Emissão criada com sucesso!");
                    break;
                case INVALID:
                    System.out.println("Criação de emissão inválida!");
                    break;
            }
        }
    }


    /**
     * Possibilita a uma empresa a criação de um leilão, mediante a especificação
     * de um valor e de uma taxa máxima admitida para este.
     *
     */
    private void createAuction() {
        Menu m = new Menu("Criar Leilão");
        m.execute();

        long value = m.readLong("Valor: ");
        float rate = m.readFloat("Taxa máxima: ");

        Protos.MessageWrapper req = createAuctionReq(value, rate);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req, socket, company);

        Protos.CompanyActionResp companyResp = null;

        if(resp.hasCompanyactionresp())
            companyResp = resp.getCompanyactionresp();
        else if(resp.hasErrormsg())
            System.out.println(resp.getErrormsg().getError());

        if(companyResp != null) {
            switch (companyResp.getStatus()) {
                case SUCCESS:
                    System.out.println("Leilão criado com sucesso!");
                    break;
                case INVALID:
                    System.out.println("Criação de leilão inválida!");
                    break;
            }
        }
    }


    /**
     * Envia um pedido pela taxa fixa de uma emissão futura ao servidor frontend.
     *
     * @return  valor da taxa fixa da emissão.
     */
    private float getEmissionFixedRate() {
        Protos.MessageWrapper req = createEmissionFixedRateReq();
        Protos.MessageWrapper resp = Utils.sendAndRecv(req, socket, company);
        return resp.getEmissionfixedrateresp().getRate();
    }


    /**
     * Cria o pedido pela taxa fixa de uma emissão futura.
     *
     * @return  Mensagem criada.
     */
    private Protos.MessageWrapper createEmissionFixedRateReq() {
        Protos.EmissionFixedRateReq emissionRateMsg = Protos.EmissionFixedRateReq.newBuilder()
                .setClient(this.name)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setClientSession(this.company.getSessionToken())
                .setEmissionfixedratereq(emissionRateMsg).build();
    }

    /**
     * Cria o pedido de criação de uma emissão.
     *
     * @param value     Valor da emissão.
     * @param rate      Taxa fixa da emissão.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createEmissionReq(long value, float rate) {
        Protos.CompanyActionReq req = Protos.CompanyActionReq.newBuilder()
                .setMaxRate(rate)
                .setValue(value)
                .setClient(this.name)
                .setReqType(Protos.CompanyActionReq.RequestType.EMISSION)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setClientSession(this.company.getSessionToken())
                .setCompanyactionreq(req).build();
    }


    /**
     * Cria o pedido de criação de um leilão.
     *
     * @param value     Valor do leilão.
     * @param rate      Taxa máxima admitida no leilão.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createAuctionReq(long value, float rate) {
        Protos.CompanyActionReq req = Protos.CompanyActionReq.newBuilder()
                .setMaxRate(rate)
                .setValue(value)
                .setClient(this.name)
                .setReqType(Protos.CompanyActionReq.RequestType.AUCTION)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setClientSession(this.company.getSessionToken())
                .setCompanyactionreq(req).build();
    }


    /**
     * Cria mensagem para pedido de logout.
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
