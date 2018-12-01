import java.io.*;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;

public class Client {
    private SocketChannel socket;
    private BufferedReader br;

    public Client(SocketChannel socket) throws Exception{
        this.socket = socket;
        this.br = new BufferedReader(new InputStreamReader(System.in));
    }

    private void authenticate() throws IOException {
        System.out.println("=== LOGIN ===");
        System.out.println("Username:");
        String user = br.readLine();
        System.out.println("Password:");
        String pass = br.readLine();

        Protos.LoginReq req = createLoginReq(user, pass);
        Utils.send_msg(req.toByteArray(), socket);

        byte[] resp = Utils.recv_msg(socket);
        Protos.LoginResp rep = Protos.LoginResp.parseFrom(resp);

        if(rep.getStatus() == Protos.LoginResp.Status.SUCCESS){
            switch(rep.getCType()){
                case COMPANY:
                    company();
                    break;
                case INVESTOR:
                    investor();
                    break;
            }
        }else{
            System.out.println("Invalid Credentials!!");
            authenticate();
        }
    }

    private void investor() {
        Object [] res;
        print_investor_menu();
        int option = get_opt(3); // 3 options
        switch(option) {
            case 1:
                res = bid_on_auction_form();
                Investor.bid_on_auction(res, socket);
                break;
            case 2:
                break;
            case 3:
                break;
            default:
                investor();
                break;
        }
    }

    private Object[] bid_on_auction_form() {

        Object [] res = new Object [2];

        try {
            System.out.println("=========== Licitar em Leilão ===========");
            System.out.print("Empresa:");
            res[0] = br.readLine();
            System.out.print("Valor:");
            res[1] = Float.parseFloat(br.readLine());
            System.out.print("Taxa:");
            res[2] = Float.parseFloat(br.readLine());
        } catch (IOException e) {
            e.printStackTrace();
            return bid_on_auction_form();
        } catch (NumberFormatException e){
            System.out.println("Valor Inválido!");
            return bid_on_auction_form();
        }

        return res;
    }

    private int get_opt(int n) {
        int i = -1;

        try {
            i = br.read();

            while(i > n){
                System.out.println("Opção inválida!!\nOpção: ");
                i = br.read();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        return i;
    }



    private void company() {
        print_company_menu();
        int option = get_opt(2); // 2 options
        float [] res;
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
            System.out.println("============== Leilão ==============");
            System.out.print("Valor:");
            res[0] = Float.parseFloat(br.readLine());
            System.out.print("Taxa máxima:");
            res[1] = Float.parseFloat(br.readLine());
        } catch (IOException e) {
            e.printStackTrace();
            return create_auction_form();
        } catch (NumberFormatException e){
            System.out.println("Valor Inválido!");
            return create_auction_form();
        }

        return res;
    }

    private float[] create_emission_form() {

        float [] res = new float [1];

        try {
            System.out.println("========== Emissão a taxa fixa ==========");
            System.out.print("Valor:");
            res[0] = Float.parseFloat(br.readLine());
        } catch (IOException e) {
            e.printStackTrace();
            return create_emission_form();
        } catch (NumberFormatException e){
            System.out.println("Valor Inválido!");
            return create_emission_form();
        }
        return res;
    }

    private void print_investor_menu() {
        StringBuilder builder = new StringBuilder("================= Menu ==============\n")
                .append("1 - Licitar em leilão\n")
                .append("2 - Subscrever empréstimo a taxa fixa\n")
                .append("3 - Subscrever notificação\n")
                .append("======================================\n")
                .append("Opção: ");
        System.out.println(builder.toString());
    }

    public void print_company_menu() {
        StringBuilder builder = new StringBuilder("================= Menu ==============\n")
                .append("1 - Criar leilão\n")
                .append("2 - Criar emissão a taxa fixa\n")
                .append("======================================\n")
                .append("Opção: ");
        System.out.print(builder.toString());
    }

    public static Protos.LoginReq createLoginReq(String user, String pass) {
        return
            Protos.LoginReq.newBuilder()
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
