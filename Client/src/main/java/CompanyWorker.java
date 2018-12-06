import org.zeromq.ZMQ;
import utils.Menu;

import java.nio.channels.SocketChannel;

public class CompanyWorker extends Thread{

    private SocketChannel socket;
    private ZMQ.Socket sub;
    private Company company;
    private String name;
    private Menu menu;

    public CompanyWorker(SocketChannel socket, Company company) {
        this.socket = socket;
        this.company = company;
        this.name = company.getName();
        menu = new Menu("Menu Empresa");
        menu.add("Criar leilão");
        menu.add("Criar emissão");
    }

    public void run() {
        int option;

        do {

            int nAsyncMessages = company.getNumAsyncMessages();
            menu.add("Ver " + nAsyncMessages + " resultados" );

            menu.execute();
            option = menu.getOption();
            processOption(option);

            menu.removeLast(1);

        } while(company.isLoggedIn());

        System.out.println("Logged out!!");
    }

    private void processOption(int option) {
        switch(option) {
            case 0: logout();
                break;
            case 1: create_auction();
                break;
            case 2: create_emission();
                break;
            case 3: readAsyncMessages();
                break;
        }
    }

    private void readAsyncMessages() {
        String asyncMessages = company.getAsyncMessages();
        System.out.println(asyncMessages);
    }

    private void logout() {
        Protos.MessageWrapper req = createLogoutReq();
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,company);

        Protos.LogoutResp logoutResp = null;
        if(resp.hasLogoutresp())
            logoutResp = resp.getLogoutresp();

        if(logoutResp != null){
            if (logoutResp.getStatus() == Protos.LogoutResp.Status.SUCCESS)
                company.logout();
            else {
                System.out.println("Erro ao fazer logout!");
            }
        }
    }

    private void create_emission() {

        float rate = getEmissionFixedRate();

        if(rate == -1){
            System.out.println("Não é possível criar emissão!!");
            return;
        }

        Menu m = new Menu("Criar Emissão a Taxa Fixa de " + String.format("%.2f", rate*100) + "%");
        m.add("Sim");
        m.execute();

        if(menu.getOption() == 0) return;

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
                    System.out.println("Emissão criada com sucesso!!");
                    break;
                case INVALID:
                    System.out.println("Criação de Emissão inválida!!");
                    break;
            }
        }
    }

    private void create_auction() {
        Menu m = new Menu("Criar Leilão");
        m.execute();

        long value = m.readLong("Valor: ");
        float rate = m.readFloat("Taxa máxima: ");

        Protos.MessageWrapper req = createAuctionReq(value, rate);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,company);

        Protos.CompanyActionResp companyResp = null;

        if(resp.hasCompanyactionresp())
            companyResp = resp.getCompanyactionresp();
        else if(resp.hasErrormsg())
            System.out.println(resp.getErrormsg().getError());

        if(companyResp != null) {
            switch (companyResp.getStatus()) {
                case SUCCESS:
                    System.out.println("Leilão criado com sucesso!!");
                    break;
                case INVALID:
                    System.out.println("Criação de Leilão inválida!!");
                    break;
            }
        }
    }

    private float getEmissionFixedRate() {
        Protos.MessageWrapper req = createEmissionFixedRateReq();
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket,company);
        return resp.getEmissionfixedrateresp().getRate();
    }

    private Protos.MessageWrapper createEmissionFixedRateReq() {
        Protos.EmissionFixedRateReq emissionRateMsg = Protos.EmissionFixedRateReq.newBuilder()
                .setClient(this.name)
                .build();
        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setEmissionfixedratereq(emissionRateMsg).build();
    }

    private Protos.MessageWrapper createEmissionReq(long value, float rate) {
        Protos.CompanyActionReq req = Protos.CompanyActionReq.newBuilder()
                .setMaxRate(rate)
                .setValue(value)
                .setClient(this.name)
                .setReqType(Protos.CompanyActionReq.RequestType.EMISSION)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setCompanyactionreq(req).build();
    }

    private Protos.MessageWrapper createAuctionReq(long value, float rate) {
        Protos.CompanyActionReq req = Protos.CompanyActionReq.newBuilder()
                .setMaxRate(rate)
                .setValue(value)
                .setClient(this.name)
                .setReqType(Protos.CompanyActionReq.RequestType.AUCTION)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setCompanyactionreq(req).build();
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
