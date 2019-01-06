import org.zeromq.ZMQ;

public class Notifier extends Thread {

    private Investor investor;
    private ZMQ.Socket sub;

    public Notifier(Investor investor, ZMQ.Socket sub) {
        this.investor = investor;
        this.sub = sub;
    }

    public void run() {
        String msg;

        while(true) {

            byte[] msgB = sub.recv();
            msg = new String(msgB);
            investor.addNotification(msg);
        }
    }
}
