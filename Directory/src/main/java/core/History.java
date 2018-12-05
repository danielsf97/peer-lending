package core;

import representations.ActiveAuction;
import representations.ActiveEmission;
import representations.Auction;
import representations.Emission;

import java.util.ArrayList;
import java.util.List;

public class History {
    private List<Auction> auctions;
    private List<Emission> emissions;

    public History() {
        this.auctions = new ArrayList<>();
        this.emissions = new ArrayList<>();
    }

    public List<Auction> getAuctions() {
        return auctions;
    }

    public List<Emission> getEmissions() {
        return emissions;
    }

    public void addAuction(ActiveAuction aa, float highestRate, float lowestRate, boolean wasSucessful) {
        Auction a = new Auction(aa.getValue(), aa.getMaxRate(), aa.getStartingDateTime(), aa.getDuration(), highestRate, lowestRate, wasSucessful);
        auctions.add(a);
    }

    public void addEmission(ActiveEmission ee) {
        Emission e = new Emission(ee.getValue(), ee.getFixedRate(), ee.getStartingDateTime(), ee.getDuration());
        emissions.add(e);
    }
}
