import org.zeromq.ZMQ;

public class Notifier extends Thread{

    private Investor investor;
    private ZMQ.Socket sub;

    public Notifier(Investor investor, ZMQ.Socket sub) {
        this.investor = investor;
        this.sub = sub;
    }

    public void run() {
        String msg;

        while(true) {

            byte[] msg_b = sub.recv();
            msg = new String(msg_b);
            investor.addNotification(msg);
        }
    }

}
