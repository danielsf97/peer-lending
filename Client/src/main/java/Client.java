
import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;
import utils.Menu;

import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;


/**
 * Classe principal, responsável por executar um cliente.
 *
 */
public class Client {


    /**
     * Mostra o menu de autenticação a um utilizador e efetua o seu login,
     * fazendo a distinção entre investidor ou empresa.
     *
     * @param socket        Socket.
     * @return              Cliente autenticado.
     * @throws Exception
     */
    private static ClientType login(SocketChannel socket) throws Exception {

        Menu m = new Menu("Autenticação");
        m.execute();
        String user = m.readString("Username: ");
        String pass = m.readString("Password: ");

        Protos.MessageWrapper req = createLoginReq(user, pass);
        Utils.sendMsg(req.toByteArray(), socket);
        byte[] respB = Utils.recvMsg(socket);
        Protos.MessageWrapper resp = Protos.MessageWrapper.parseFrom(respB);

        Protos.LoginResp loginResp = null;

        if(resp.hasLoginresp())
            loginResp = resp.getLoginresp();

        if(loginResp != null) {
            if (loginResp.getStatus() == Protos.LoginResp.Status.SUCCESS) {
                if (loginResp.getCType() == Protos.LoginResp.ClientType.COMPANY) {
                    return new Company(user);
                }
                else {
                    return new Investor(user);
                }
            }
            else {
                System.out.println("Credenciais inválidas!");
                return login(socket);
            }
        }
        return login(socket);
    }

    /**
     * Cria uma mensagem de resposta a um pedido de autenticação.
     *
     * @param resp     Bytes com mensagem de resposta.
     * @return         Mensagem criada.
     */
    public static Protos.LoginResp getLoginResp(byte[] resp) {
        try {
            Protos.MessageWrapper msg = Protos.MessageWrapper.parseFrom(resp);
            if(msg.hasLoginresp())
                return msg.getLoginresp();
        }
        catch (InvalidProtocolBufferException e) {
            System.out.println("Mensagem recebida inválida!");
            e.printStackTrace();
        }
        return null;
    }


    /**
     * Cria uma mensagem de pedido de autenticação.
     *
     * @param user     Utilizador que se deseja autenticar.
     * @param pass     Password do utilizador que se deseja autenticar.
     * @return         Mensagem criada.
     */
    public static Protos.MessageWrapper createLoginReq(String user, String pass) {
        Protos.LoginReq loginMsg = Protos.LoginReq.newBuilder()
                                        .setName(user)
                                        .setPassword(pass)
                                        .build();
        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.SYNC)
                .setLoginreq(loginMsg).build();
    }


    /**
     * Começa a execução de um cliente.
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception{
        String address = "127.0.0.1"; // args[0]
        int frontendPort = 12345; //Integer.parseInt(args[1]);
        int notificationsPort = 12346; //Integer.parseInt(args[2]);

        SocketChannel socket = SocketChannel.open();
        socket.connect(new InetSocketAddress(address,frontendPort));

        ClientType clientType = login(socket);

        if(clientType instanceof Company) {
            Company company = (Company) clientType;
            CompanyWorker compWorker = new CompanyWorker(socket, company);

            compWorker.start();
        }
        else {
            Investor investor = (Investor) clientType;

            ZMQ.Context context = ZMQ.context(1);
            ZMQ.Socket sub = context.socket(ZMQ.SUB);
            sub.connect("tcp://localhost:" + notificationsPort);


            Notifier notifier = new Notifier(investor, sub);
            InvestorWorker invWorker = new InvestorWorker(socket, investor, sub);

            notifier.start();
            invWorker.start();
        }

        MsgReader reader = new MsgReader(socket, clientType);
        reader.start();
    }

}
