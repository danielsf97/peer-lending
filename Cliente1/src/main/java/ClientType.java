public class ClientType {
    private String name;
    private boolean loggedIn;

    public ClientType(String name) {
        this.name = name;
        this.loggedIn = true;
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
}
