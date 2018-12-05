import java.nio.channels.SocketChannel;

public class MsgReader extends Thread{
    private SocketChannel sc;

    public MsgReader(SocketChannel socket){
        this.sc = socket;
    }

    public void run(){

    }
}
