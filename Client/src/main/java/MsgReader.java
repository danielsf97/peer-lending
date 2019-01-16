import java.io.IOException;
import java.nio.channels.SocketChannel;


/**
 * Responsável por ler mensagens recebidas por um cliente.
 *
 */
public class MsgReader extends Thread {
    private SocketChannel sc;
    private ClientType client;


    /**
     * Construtor parametrizado.
     *
     * @param socket    Socket.
     * @param client    Cliente a quem são destinadas as mensagens.
     */
    public MsgReader(SocketChannel socket, ClientType client) {
        this.sc = socket;
        this.client = client;
    }


    /**
     * Lê mensagens recebidas, síncronas e assíncronas. Adiciona as mensagens
     * ao respetivo cliente.
     *
     */
    public void run() {
        byte[] msgB;
        Protos.MessageWrapper msg;
        try {

            while((msgB = Utils.recvMsg(sc)) != null) {

                msg = Protos.MessageWrapper.parseFrom(msgB);

                switch (msg.getMsgType()){
                    case SYNC:
                        client.setSyncMessage(msg);
                        break;
                    case ASYNC:
                        client.addAsyncMessage(msg.getAuctionemissionresult().getMsg());
                        break;
                }
            }

        }
        catch(IOException e) {
            e.printStackTrace();
        }
    }
}