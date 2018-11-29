import com.google.protobuf.CodedOutputStream;

import java.io.*;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;
import java.util.Scanner;

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
        send_msg(req.toByteArray());

        byte[] resp = recv_msg();
        Protos.LoginRep rep = Protos.LoginRep.parseFrom(resp);

        if(rep.getStatus() == Protos.LoginRep.Status.SUCCESS){
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

    private void send_msg(byte[] ba) throws IOException{

        ByteBuffer buffer = ByteBuffer.allocate(4 + ba.length);
        buffer.order(ByteOrder.BIG_ENDIAN);
        buffer.putInt(ba.length);
        buffer.put(ba);
        buffer.flip();

        this.socket.write(buffer);
    }

    private byte[] recv_msg() throws  IOException{
        ByteBuffer buffer = ByteBuffer.allocate(512);
        this.socket.read(buffer);
        buffer.flip();
        int len = buffer.getInt();
        byte[] resp = new byte[len];
        buffer.get(resp, 0, len);
        return resp;
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
