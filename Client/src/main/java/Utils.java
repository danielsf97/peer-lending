import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;

public class Utils {

    /**
     * Envia e recebe a respetiva resposta de uma mensagem.
     *
     * @param req           Mensagem (pedido) a enviar.
     * @param socket        SocketChannel.
     * @param client        Cliente a quem enviar.
     * @return
     */
    public static Protos.MessageWrapper sendAndRecv(Protos.MessageWrapper req, SocketChannel socket, ClientType client) {
        Protos.MessageWrapper resp = null;

        try {
            byte[] reqB = req.toByteArray();

            sendMsg(reqB, socket);

            resp = getMsg(client);
        }
        catch(IOException e) {
            e.printStackTrace();
        }

        return resp;
    }

    /**
     * Recebe mensagem de um cliente.
     *
     * @param client    Cliente de quem receber mensagem.
     * @return          Mensagem recebida.
     */
    private static Protos.MessageWrapper getMsg(ClientType client) {
        return client.getSyncMessage();
    }


    /**
     * Envia uma mensagem.
     *
     * @param ba            Bytes da mensagem.
     * @param socket        SocketChannel.
     * @throws IOException
     */
    public static void sendMsg(byte[] ba, SocketChannel socket) throws IOException {

        ByteBuffer buffer = ByteBuffer.allocate(ba.length);
        buffer.order(ByteOrder.BIG_ENDIAN);
        buffer.put(ba);
        buffer.flip();

        socket.write(buffer);

    }


    /**
     * Recebe uma mensagem.
     *
     * @param socket        SocketChannel.
     * @return              Mensagem recebida.
     * @throws IOException
     */
    public static byte[] recvMsg(SocketChannel socket) throws IOException {
        ByteBuffer buffer = ByteBuffer.allocate(512);

        int len = socket.read(buffer);
        byte[] resp = null;

        if(len > 0) {
            buffer.flip();
            resp = new byte[len];
            buffer.get(resp, 0, len);
        }

        return resp;
    }
}