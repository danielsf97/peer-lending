import java.util.ArrayDeque;


/**
 * Representa um cliente.
 *
 */
public class ClientType {
    private String name;
    private boolean loggedIn;
    private ArrayDeque<String> asyncMessages;
    private int numAsyncMessages;
    private Protos.MessageWrapper syncMessage;


    /**
     * Representa um cliente.
     *
     * @param name
     */
    public ClientType(String name) {
        this.name = name;
        this.loggedIn = true;
        this.asyncMessages = new ArrayDeque<>();
        this.numAsyncMessages = 0;
        this.syncMessage = null;
    }


    /**
     * Devolve o nome do cliente.
     *
     * @return  Nome do cliente.
     */
    public String getName() {
        return name;
    }


    /**
     * Devolve se o cliente está autenticado ou não.
     *
     * @return true caso o cliente esteja autenticado
     *         falso caso contrário.
     */
    public boolean isLoggedIn() {
        return this.loggedIn;
    }


    /**
     * Faz logout a um cliente.
     *
     */
    public void logout() {
        this.loggedIn = false;
    }


    /**
     * Retorna o número de mensagens assíncronas.
     *
     * @return o número de mensagens assíncronas.
     */
    public synchronized int getNumAsyncMessages() {
        return numAsyncMessages;
    }

    /**
     * Adiciona uma mensagem assíncrona (notificação).
     *
     * @param asyncMsg mensagem assíncrona a adicionar.
     */
    public synchronized void addAsyncMessage(String asyncMsg) {
        this.asyncMessages.addLast(asyncMsg);
        this.numAsyncMessages++;
    }


    /**
     * Estabelece uma mensagem síncrona recebida.
     *
     * @param syncMessage Mensagem recebida.
     */
    public synchronized void setSyncMessage(Protos.MessageWrapper syncMessage) {
        this.syncMessage = syncMessage;
        this.notify();
    }


    /**
     * Espera por uma mensagem síncrona, retornando-a.
     *
     * @return   mensagem síncrona recebida.
     */
    public synchronized Protos.MessageWrapper getSyncMessage() {
        Protos.MessageWrapper msg;
        try {

            if (this.syncMessage == null)
                this.wait();

        }
        catch (InterruptedException e) {
            e.printStackTrace();
            return getSyncMessage();
        }

        msg = syncMessage;
        setSyncMessage(null);
        return msg;
    }


    /**
     * Lista de mensagens assíncronas.
     *
     * @return  lista de mensagens assíncronas.
     */
    public synchronized String getAsyncMessages() {

        String msg;
        StringBuilder sb = new StringBuilder();

        while((msg = asyncMessages.pollFirst()) != null) {
            sb.append(msg).append("\n");
            numAsyncMessages--;
        }

        return sb.toString();
    }
}
