import java.util.ArrayDeque;

public class Investor extends ClientType {

    private ArrayDeque<String> notifications;
    private int numNotifications;

    public Investor(String name) {
        super(name);
        this.notifications = new ArrayDeque<>();
        this.numNotifications = 0;
    }

    public synchronized void addNotification(String notification) {
        this.notifications.addLast(notification);
        this.numNotifications++;
    }

    public synchronized int getNumNotifications() {
        return numNotifications;
    }

    public synchronized String getNotifications() {
        String msg;
        StringBuilder sb = new StringBuilder();

        while((msg = notifications.pollFirst()) != null)
            sb.append(msg).append("\n");

        return sb.toString();
    }
}
