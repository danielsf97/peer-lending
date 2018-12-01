import org.apache.commons.lang3.StringUtils;
import utils.Menu;
import utils.NetClient;
import utils.Utils;

import java.io.*;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;

public class Client {
    private SocketChannel socket;
    private BufferedReader br;

    public Client(SocketChannel socket) {
        this.socket = socket;
        this.br = new BufferedReader(new InputStreamReader(System.in));
    }

    private void authenticate() throws IOException {
        int w = 79;
        System.out.println(StringUtils.rightPad("LOGIN ", w - 1, "-"));
        System.out.print("Username: ");
        String user = br.readLine();
        System.out.print("Password: ");
        String pass = br.readLine();

        Protos.LoginReq req = createLoginReq(user, pass);
        Utils.send_msg(req.toByteArray(), socket);

        byte[] resp = Utils.recv_msg(socket);
        Protos.LoginResp rep = Protos.LoginResp.parseFrom(resp);

        if(rep.getStatus() == Protos.LoginResp.Status.SUCCESS) {
            switch(rep.getCType()){
                case COMPANY:
                    company();
                    break;
                case INVESTOR:
                    investor();
                    break;
            }
        }
        else {
            System.out.println("Credenciais inválidas!");
            authenticate();
        }
    }

    private void investor() {
        Object[] res;
        Menu m = new Menu("utils.Menu Investidor");
        m.adiciona("Licitar em leilão");
        m.adiciona("Subscrever empréstimo a taxa fixa");
        m.adiciona("Subscrever notificação");
        m.adiciona("Ver lista de empresas");
        m.adiciona("Ver leilões ativos");
        m.adiciona("Ver emissões ativas");
        m.adiciona("Ver histórico de leilões de empresa");
        m.adiciona("Ver histórico de emissões de empresa");
        m.executa();
        int option = m.getOpcao();

        switch(option) {
            case 1:
                res = bid_on_auction_form();
                Investor.bid_on_auction(res, socket);
                break;
            case 2:
                break;
            case 3:
                break;
            case 4:
                // Ver lista de empresas
                try {
                    System.out.println(NetClient.getEmpresas());
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
                investor();
                break;
            case 5:
                // Ver leilões ativos
                try {
                    System.out.println(NetClient.getLeiloesAtivos());
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
                investor();
                break;
            case 6:
                // Ver emissões ativas
                try {
                    System.out.println(NetClient.getEmissoesAtivas());
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
                investor();
                break;
            case 7:
                // Ver histórico de leilões para empresa
                try {
                    System.out.print("ID da empresa: ");
                    long emp = Long.parseLong(br.readLine());
                    System.out.println(NetClient.getHistoricoLeiloes(emp));
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
                investor();
                break;
            case 8:
                // Ver histórico de emissões para empresa
                try {
                    System.out.print("Empresa: ");
                    long emp = Long.parseLong(br.readLine());
                    System.out.println(NetClient.getHistoricoEmissoes(emp));
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
                investor();
                break;
            default:
                investor();
                break;
        }
    }

    private Object[] bid_on_auction_form() {

        Object[] res = new Object[2];

        try {
            int w = 79;
            System.out.println(StringUtils.rightPad("LICITAR EM LEILÃO ", w - 1, "-"));
            System.out.print("Empresa:");
            res[0] = br.readLine();
            System.out.print("Valor:");
            res[1] = Float.parseFloat(br.readLine());
            System.out.print("Taxa:");
            res[2] = Float.parseFloat(br.readLine());
        }
        catch (IOException e) {
            e.printStackTrace();
            return bid_on_auction_form();
        }
        catch (NumberFormatException e){
            System.out.println("Valor Inválido!");
            return bid_on_auction_form();
        }

        return res;
    }

    private void company() {
        Menu m = new Menu("utils.Menu Empresa");
        m.adiciona("Criar leilão");
        m.adiciona("Criar emissão");
        m.executa();
        int option = m.getOpcao();
        float[] res;
        switch(option) {
            case 1:
                res = create_auction_form();
                Company.create_auction(res, socket);
                company();
                break;
            case 2:
                res = create_emission_form();
                Company.create_emission(res, socket);
                company();
                break;
            default:
                company();
                break;
        }
    }

    private float[] create_auction_form() {

        float [] res = new float [2];

        try {
            int w = 79;
            System.out.println(StringUtils.rightPad("CRIAR LEILÃO ", w - 1, "-"));
            System.out.print("Valor:");
            res[0] = Float.parseFloat(br.readLine());
            System.out.print("Taxa máxima:");
            res[1] = Float.parseFloat(br.readLine());
        }
        catch (IOException e) {
            e.printStackTrace();
            return create_auction_form();
        }
        catch (NumberFormatException e){
            System.out.println("Valor inválido!");
            return create_auction_form();
        }

        return res;
    }

    private float[] create_emission_form() {

        float[] res = new float[1];

        try {
            int w = 79;
            System.out.println(StringUtils.rightPad("CRIAR EMISSÃO", w - 1, "-"));
            System.out.print("Valor: ");
            res[0] = Float.parseFloat(br.readLine());
        }
        catch (IOException e) {
            e.printStackTrace();
            return create_emission_form();
        }
        catch (NumberFormatException e){
            System.out.println("Valor inválido!");
            return create_emission_form();
        }
        return res;
    }

    public static Protos.LoginReq createLoginReq(String user, String pass) {
        return Protos.LoginReq.newBuilder()
                              .setName(user)
                              .setPassword(pass)
                              .build();
    }

    public static void main(String[] args) throws Exception{
        SocketChannel socket = SocketChannel.open();
        socket.connect(new InetSocketAddress("127.0.0.1",12345));
        Client c = new Client(socket);
        c.authenticate();
    }
}
