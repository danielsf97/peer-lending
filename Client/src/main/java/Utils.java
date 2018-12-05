package utils;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;

public class Utils {

    public static Protos.MessageWrapper sendAndRecv(Protos.MessageWrapper req, SocketChannel socket, ClientType client){
        Protos.MessageWrapper resp = null;

        try {
            byte [] req_b = req.toByteArray();

            send_msg(req_b, socket);

            resp = get_msg(client);
        }
        catch(IOException e) {
            e.printStackTrace();
        }

        return resp;
    }

    private static Protos.MessageWrapper get_msg(ClientType client) {
        return client.getSyncMessage();
    }

    public static void send_msg(byte[] ba, SocketChannel socket) throws IOException {

        ByteBuffer buffer = ByteBuffer.allocate(ba.length);
        buffer.order(ByteOrder.BIG_ENDIAN);
        buffer.put(ba);
        buffer.flip();

        socket.write(buffer);

    }

    public static byte[] recv_msg(SocketChannel socket) throws IOException {
        ByteBuffer buffer = ByteBuffer.allocate(512);

        int len = socket.read(buffer);
        buffer.flip();
        byte[] resp = new byte[len];
        buffer.get(resp, 0, len);

        return resp;
    }
}