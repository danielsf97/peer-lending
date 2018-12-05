
import org.zeromq.ZMQ;
import protos.Protos;
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
            if(nNotifications > 0)
                menu.add("Ver " + nNotifications + " notificações" );
            menu.execute();
            option = menu.getOption();
            processOption(option);
            if(nNotifications > 0)
                menu.removeLast(1);
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
        }
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
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket);

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
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket);

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
                    System.out.println("Licitação Inváli    <name>PeerLending-Diretorio</name>\n" +
                            "\n" +
                            "    <properties>\n" +
                            "        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>\n" +
                            "        <dropwizard.version>0.7.1</dropwizard.version>\n" +
                            "        <maven.compiler.source>1.8</maven.compiler.source>\n" +
                            "        <maven.compiler.target>1.8</maven.compiler.target>\n" +
                            "    </properties>\n" +
                            "\n" +
                            "\n" +
                            "    <dependencies>\n" +
                            "        <dependency>\n" +
                            "            <groupId>javax.xml.bind</groupId>\n" +
                            "            <artifactId>jaxb-api</artifactId>\n" +
                            "            <version>2.3.0</version>\n" +
                            "        </dependency>\n" +
                            "        <dependency>\n" +
                            "            <groupId>io.dropwizard</groupId>\n" +
                            "            <artifactId>dropwizard-core</artifactId>\n" +
                            "            <version>${dropwizard.version}</version>\n" +
                            "        </dependency>\n" +
                            "    </dependencies>\n" +
                            "\n" +
                            "    <build>\n" +
                            "        <plugins>\n" +
                            "\n" +
                            "            <plugin>\n" +
                            "                <groupId>org.apache.maven.plugins</groupId>\n" +
                            "                <artifactId>maven-shade-plugin</artifactId>\n" +
                            "                <version>1.6</version>\n" +
                            "                <configuration>\n" +
                            "                    <createDependencyReducedPom>true</createDependencyReducedPom>\n" +
                            "                    <filters>\n" +
                            "                        <filter>\n" +
                            "                            <artifact>*:*</artifact>\n" +
                            "                            <excludes>\n" +
                            "                                <exclude>META-INF/*.SF</exclude>\n" +
                            "                                <exclude>META-INF/*.DSA</exclude>\n" +
                            "                                <exclude>META-INF/*.RSA</exclude>\n" +
                            "                            </excludes>\n" +
                            "                        </filter>\n" +
                            "                    </filters>\n" +
                            "                </configuration>\n" +
                            "                <executions>\n" +
                            "                    <execution>\n" +
                            "                        <phase>package</phase>\n" +
                            "                        <goals>\n" +
                            "                            <goal>shade</goal>\n" +
                            "                        </goals>\n" +
                            "                        <configuration>\n" +
                            "                            <transformers>\n" +
                            "                                <transformer implementation=\"org.apache.maven.plugins.shade.resource.ServicesResourceTransformer\"/>\n" +
                            "                                <transformer implementation=\"org.apache.maven.plugins.shade.resource.ManifestResourceTransformer\">\n" +
                            "                                    <mainClass>hello.HelloApplication</mainClass>\n" +
                            "                                </transformer>\n" +
                            "                            </transformers>\n" +
                            "                        </configuration>\n" +
                            "                    </execution>\n" +
                            "                </executions>\n" +
                            "            </plugin>\n" +
                            "\n" +
                            "            <plugin>\n" +
                            "                <groupId>org.apache.maven.plugins</groupId>\n" +
                            "                <artifactId>maven-jar-plugin</artifactId>\n" +
                            "                <version>2.4</version>\n" +
                            "                <configuration>\n" +
                            "                    <archive>\n" +
                            "                        <manifest>\n" +
                            "                            <addDefaultImplementationEntries>true</addDefaultImplementationEntries>\n" +
                            "                        </manifest>\n" +
                            "                    </archive>\n" +
                            "                </configuration>\n" +
                            "            </plugin>\n" +
                            "            <plugin>\n" +
                            "                <groupId>org.apache.maven.plugins</groupId>\n" +
                            "                <artifactId>maven-compiler-plugin</artifactId>\n" +
                            "                <configuration>\n" +
                            "                    <source>9</source>\n" +
                            "                    <target>9</target>\n" +
                            "                </configuration>\n" +
                            "            </plugin>\n" +
                            "\n" +
                            "        </plugins>\n" +
                            "    </build>da. Leilão Terminado!!");
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
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket);

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
        return Protos.MessageWrapper.newBuilder().setInvestoractionreq(req).build();
    }

    private Protos.MessageWrapper createEmissionReq(String comp, long value) {
        Protos.InvestorActionReq req = Protos.InvestorActionReq.newBuilder()
                .setValue(value)
                .setReqType(Protos.InvestorActionReq.RequestType.AUCTION)
                .setCompany(comp)
                .setClient(name)
                .build();
        return Protos.MessageWrapper.newBuilder().setInvestoractionreq(req).build();
    }

    public Protos.MessageWrapper createLogoutReq() {
        Protos.LogoutReq logoutMsg = Protos.LogoutReq.newBuilder()
                .setName(this.name)
                .build();
        return Protos.MessageWrapper.newBuilder().setLogoutreq(logoutMsg).build();
    }
}
