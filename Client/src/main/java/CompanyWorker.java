import org.zeromq.ZMQ;
import protos.Protos;
import utils.Menu;
import utils.Utils;

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

    public void run(){
        int option;

        do{
            menu.execute();
            option = menu.getOption();
            processOption(option);

        }while(company.isLoggedIn());

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
        }
    }

    private void logout() {
        Protos.MessageWrapper req = createLogoutReq();
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket);

        Protos.LogoutResp logoutResp = null;
        if(resp.hasLogoutresp())
            logoutResp = resp.getLogoutresp();

        if(logoutResp != null){
            if (logoutResp.getStatus() == Protos.LogoutResp.Status.SUCCESS)
                company.logout();
            else{
                System.out.println("Erro ao fazer logout!!");
            }
        }
    }

    private void create_emission() {
        Menu m = new Menu("Criar Emissão a Taxa Fixa");
        m.execute();

        long value = m.readLong("Valor: ");

        Protos.MessageWrapper req = createEmissionReq(value);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket);

        Protos.CompanyActionResp companyResp = null;

        if(resp.hasCompanyactionresp())
            companyResp = resp.getCompanyactionresp();
        else if(resp.hasErrormsg())
            System.out.println(resp.getErrormsg().getError());

        if(companyResp != null) {
            switch (companyResp.getStatus()){
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
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket);

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

    private Protos.MessageWrapper createEmissionReq(long value) {
        Protos.CompanyActionReq req = Protos.CompanyActionReq.newBuilder()
                .setValue(value)
                .setClient(name)
                .build();

        return Protos.MessageWrapper.newBuilder().setCompanyactionreq(req).build();
    }

    private Protos.MessageWrapper createAuctionReq(long value, float rate) {
        Protos.CompanyActionReq req = Protos.CompanyActionReq.newBuilder()
                .setMaxRate(rate)
                .setValue(value)
                .setClient(name)
                .build();

        return Protos.MessageWrapper.newBuilder().setCompanyactionreq(req).build();
    }

    public Protos.MessageWrapper createLogoutReq() {
        Protos.LogoutReq logoutMsg = Protos.LogoutReq.newBuilder()
                .setName(this.name)
                .build();
        return Protos.MessageWrapper.newBuilder().setLogoutreq(logoutMsg).build();
    }

}
