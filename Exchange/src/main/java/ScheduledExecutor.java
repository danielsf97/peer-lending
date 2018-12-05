
import org.zeromq.ZMQ;

import java.util.ArrayList;

public class ScheduledExecutor implements Runnable {

    private Company company;
    private ZMQ.Socket socket;

    public ScheduledExecutor(Company company, ZMQ.Socket socket){
        this.company = company;
        this.socket = socket;
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
        Pair<ArrayList<Pair<String,Long>>,ArrayList<String>> winnersAndLosers = auction.getWinnersLosers();

        ArrayList<Pair<String,Long>> winners = winnersAndLosers.getFirst();
        ArrayList<String> losers = winnersAndLosers.getSecond();

        if(winners != null){
            for(Pair winnerVal : winners){
                createInvestorAuctionResultMsg(winnerVal);
            }
        }

        if(losers != null){
            for(String investor : losers){

            }
        }
    }

    private void createInvestorAuctionResultMsg(Pair winnerVal) {

    }
}
