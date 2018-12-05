public class Company {
    private String name;
    private Auction activeAuction;
    private Emission activeEmission;

    public Company(String name) {
        this.activeAuction = null;
        this.activeEmission = null;
        this.name = name;
    }

    public synchronized void setActiveAuction(Auction a) throws Exception {
        if((activeAuction == null && activeEmission == null) || a == null)
            this.activeAuction = a;
        else throw new Exception();
    }

    public synchronized void setActiveEmission(Emission e) throws Exception {
        if((activeAuction == null && activeEmission == null) || e == null)
            this.activeEmission = e;
        else throw new Exception();
    }

    public synchronized Auction getActiveAuction() {
        return activeAuction;
    }

    public synchronized Emission getActiveEmission() {
        return activeEmission;
    }

    public synchronized String getName() {
        return this.name;
    }
}
