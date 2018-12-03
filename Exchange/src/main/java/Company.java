public class Company {
    private String company;
    private Auction activeAuction;
    private Emission activeEmission;

    public Company(String name) {
        this.company = name;
    }

    public void setActiveAuction(Auction a) throws Exception {
        if(activeAuction != null || activeEmission != null)
            this.activeAuction = a;
        else throw new Exception();
    }

    public void setActiveEmission(Emission e) throws Exception {
        if(activeAuction != null || activeEmission != null)
            this.activeEmission = e;
        else throw new Exception();
    }
}
