import java.util.ArrayDeque;

/**
 * Representa um investidor.
 *
 */
public class Investor extends ClientType {

    private ArrayDeque<String> notifications;
    private int numNotifications;

    /**
     * Construtor parametrizado.
     *
     * @param name  Nome do investidor.
     */
    public Investor(String name, String token) {
        super(name, token);
        this.notifications = new ArrayDeque<>();
        this.numNotifications = 0;
    }


    /**
     * Adiciona uma notificação para o investidor.
     *
     * @param notification  Notificação a adicionar.
     */
    public synchronized void addNotification(String notification) {
        this.notifications.addLast(notification);
        this.numNotifications++;
    }


    /**
     * Retorna número de notificações para o investidor.
     *
     * @return número de notificações para o investidor.
     */
    public synchronized int getNumNotifications() {
        return numNotifications;
    }


    /**
     * Retorna as notificações para um investidor.
     *
     * @return as notificações para um investidor.
     */
    public synchronized String getNotifications() {
        String msg;
        StringBuilder sb = new StringBuilder();

        while((msg = notifications.pollFirst()) != null){
            sb.append(msg).append("\n");
            numNotifications--;
        }

        return sb.toString();
    }
}
