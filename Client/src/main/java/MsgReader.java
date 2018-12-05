package client;

import utils.Utils;

import java.io.IOException;
import java.nio.channels.SocketChannel;

public class MsgReader extends Thread{
    private SocketChannel sc;
    private ClientType client;

    public MsgReader(SocketChannel socket, ClientType client){
        this.sc = socket;
        this.client = client;
    }

    public void run(){
        byte [] msg_b;
        Protos.MessageWrapper msg;
        try {

            while ((msg_b = Utils.recv_msg(sc)) != null){

                msg = Protos.MessageWrapper.parseFrom(msg_b);
                switch (msg.getMsgType()){
                    case SYNC:
                        client.setSyncMessage(msg);
                        break;
                    case ASYNC:
                        client.addAsyncMessage(msg.getAuctionemissionresult().getMsg());
                        break;
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
