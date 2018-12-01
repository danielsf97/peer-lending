package utils;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;

public class Utils {

    public static void send_msg(byte[] ba, SocketChannel socket) throws IOException {

        ByteBuffer buffer = ByteBuffer.allocate(4 + ba.length);
        buffer.order(ByteOrder.BIG_ENDIAN);
        buffer.putInt(ba.length);
        buffer.put(ba);
        buffer.flip();

        socket.write(buffer);
    }

    public static byte[] recv_msg(SocketChannel socket) throws  IOException{
        ByteBuffer buffer = ByteBuffer.allocate(512);
        socket.read(buffer);
        buffer.flip();
        int len = buffer.getInt();
        byte[] resp = new byte[len];
        buffer.get(resp, 0, len);
        return resp;
    }
}