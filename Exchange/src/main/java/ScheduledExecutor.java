public class ScheduledExecutor implements Runnable {

    private Company company;

    public ScheduledExecutor(Company company){
        this.company = company;
    }

    @Override
    public void run() {
        Auction auction = null;
        Emission emission = null;
        try{
            if((auction = company.getActiveAuction()) != null){
                finishAuction(auction);
                company.setActiveAuction(null);
            }else{
                emission = company.getActiveEmission();
                finishEmission(emission);
                company.setActiveEmission(null);
            }
        } catch (Exception e) {
            e.printStackTrace(); //never going to happen
        }

    }

    private void finishAuction(Auction auction) {

    }
}
