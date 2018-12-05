import io.dropwizard.Application;
import io.dropwizard.Configuration;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import representations.*;
import resources.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DiretorioApp extends Application<Configuration> {
    private Map<Integer, Company> companies;
    private List<ActiveAuction> activeAuctions;
    private List<ActiveEmission> activeEmissions;
    private Map<String, List<Auction>> auctionHistory;
    private Map<String, List<Emission>> emissionHistory;


    public DiretorioApp() {
        this.companies = new HashMap<>();
        this.companies.put(0,new Company(0,"empA"));
        this.companies.put(1,"empB");
        this.companies.put(2,"empC");
        this.activeAuctions = new ArrayList<>();
        this.activeEmissions = new ArrayList<>();
        this.auctionHistory = new HashMap<>();
        this.emissionHistory = new HashMap<>();
        for(String c : this.companies) {
            auctionHistory.put(c, new ArrayList<>());
            emissionHistory.put(c, new ArrayList<>());
        }
    }

    @Override
    public String getName() {
        return "Directory";
    }

    @Override
    public void initialize(Bootstrap<Configuration> bootstrap) {
    }

    @Override
    public void run(Configuration config, Environment environment) {
        environment.jersey().register(new CompaniesResource(companies));
        environment.jersey().register(new AuctionsResource(activeAuctions));
        environment.jersey().register(new EmissionsResource(activeEmissions));
        environment.jersey().register(new AuctionHistoryResource(companies, auctionHistory));
        environment.jersey().register(new EmissionHistoryResource(companies, emissionHistory));
    }

    public static void main(String[] args) throws Exception {
        new DiretorioApp().run(args);
    }
}
