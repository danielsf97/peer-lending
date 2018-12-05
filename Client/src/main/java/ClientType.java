import java.util.ArrayDeque;

public class ClientType {
    private String name;
    private boolean loggedIn;
    private ArrayDeque<String> asyncMessages;
    private int numAsyncMessages;
    private Protos.MessageWrapper syncMessage;

    public ClientType(String name) {
        this.name = name;
        this.loggedIn = true;
        this.asyncMessages = new ArrayDeque<>();
        this.numAsyncMessages = 0;
        this.syncMessage = null;
    }

    public String getName() {
        return name;
    }

    public boolean isLoggedIn(){
        return this.loggedIn;
    }

    public void logout(){
        this.loggedIn = false;
    }

    public synchronized int getNumAsyncMessages() {
        return numAsyncMessages;
    }

    public synchronized void addAsyncMessage(String asyncMsg) {
        this.asyncMessages.addLast(asyncMsg);
        this.numAsyncMessages++;
    }

    public synchronized void setSyncMessage(Protos.MessageWrapper syncMessage){
        this.syncMessage = syncMessage;
        this.notify();
    }

    public synchronized Protos.MessageWrapper getSyncMessage(){
        Protos.MessageWrapper msg = null;
        try {

            if (this.syncMessage == null)
                this.wait();

        } catch (InterruptedException e) {
            e.printStackTrace();
            return getSyncMessage();
        }

        msg = syncMessage;
        setSyncMessage(null);
        return msg;
    }

    public synchronized String getAsyncMessages() {

        String msg;
        StringBuilder sb = new StringBuilder();

        while((msg = asyncMessages.pollFirst()) != null)
            sb.append(msg).append("\n");

        return sb.toString();
    }
}
