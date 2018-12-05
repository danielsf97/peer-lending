import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;
import protos.Protos;
import utils.Menu;
import utils.Utils;

import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;

public class Client {

    private static ClientType login(SocketChannel socket) throws Exception {
        int w = 79;

        Menu m = new Menu("Autenticação");
        m.execute();
        String user = m.readString("Username: ");
        String pass = m.readString("Password: ");

        Protos.MessageWrapper req = createLoginReq(user, pass);
        Protos.MessageWrapper resp = Utils.sendAndRecv(req,socket);

        Protos.LoginResp loginResp = null;

        if(resp.hasLoginresp())
            loginResp = resp.getLoginresp();

        if(loginResp != null) {
            if (loginResp.getStatus() == Protos.LoginResp.Status.SUCCESS) {
                if (loginResp.getCType() == Protos.LoginResp.ClientType.COMPANY) {
                    return new Company(user);
                } else {
                    return new Investor(user);
                }
            } else {
                System.out.println("Credenciais inválidas!");
                return login(socket);
            }
        }
        return login(socket);
    }

    public static Protos.LoginResp getLoginResp(byte [] resp){
        try {
            Protos.MessageWrapper msg = Protos.MessageWrapper.parseFrom(resp);
            if(msg.hasLoginresp())
                return msg.getLoginresp();
        }
        catch (InvalidProtocolBufferException e) {
            System.out.println("Mensagem recebida inválida!!");
            e.printStackTrace();
        }
        return null;
    }

    public static Protos.MessageWrapper createLoginReq(String user, String pass) {
        Protos.LoginReq loginMsg = Protos.LoginReq.newBuilder()
                                        .setName(user)
                                        .setPassword(pass)
                                        .build();
        return Protos.MessageWrapper.newBuilder().setLoginreq(loginMsg).build();
    }

    public static void main(String[] args) throws Exception{
        String address = "127.0.0.1"; // args[0]
        int frontend_port = 12345; //Integer.parseInt(args[1]);
        int frontend_async_port = 12346; //Integer.parseInt(args[2]);
        int notifications_port = 12347; //Integer.parseInt(args[3]);

        //socket utilizado para enviar pedidos e receber respostas a leilões
        SocketChannel socket = SocketChannel.open();
        socket.connect(new InetSocketAddress(address,frontend_port));

        //socket utilizado para receber informação sobre se fomos vencedores ou não leilões
        //em que licitamos licitamos e para as empresas saberem como correram os leilões e emissões
        SocketChannel socketAsync = SocketChannel.open();
        socket.connect(new InetSocketAddress(address,frontend_async_port));

        ClientType clientType = login(socket);

        if(clientType instanceof Company){
            Company company = (Company) clientType;
            CompanyWorker comp_worker = new CompanyWorker(socket, company);

            comp_worker.start();
        }else {
            Investor investor = (Investor) clientType;

            ZMQ.Context context = ZMQ.context(1);
            ZMQ.Socket sub = context.socket(ZMQ.SUB);
            sub.connect("tcp://localhost:" + notifications_port);


            Notifier notifier = new Notifier(investor, sub);
            InvestorWorker inv_worker = new InvestorWorker(socket, investor, sub);

            notifier.start();
            inv_worker.start();
        }

        MsgReader reader = new MsgReader(socketAsync);
        reader.start();
    }

}
