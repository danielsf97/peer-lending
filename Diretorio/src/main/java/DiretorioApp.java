import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import representations.*;
import resources.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DiretorioApp extends Application<DiretorioConfiguration> {
    private Map<Long, Company> companies;
    private List<ActiveAuction> activeAuctions;
    private List<ActiveEmission> activeEmissions;
    private Map<Long, List<Auction>> auctionHistory;
    private Map<Long, List<Emission>> emissionHistory;


    @Override
    public String getName() {
        return "Diretorio";
    }

    @Override
    public void initialize(Bootstrap<DiretorioConfiguration> bootstrap) {
        this.companies = new HashMap<>();
        this.activeAuctions = new ArrayList<>();
        this.activeEmissions = new ArrayList<>();
        this.auctionHistory = new HashMap<>();
        this.emissionHistory = new HashMap<>();
        for(Company e : this.companies.values()) {
            auctionHistory.put(e.getId(), new ArrayList<>());
            emissionHistory.put(e.getId(), new ArrayList<>());
        }
    }

    @Override
    public void run(DiretorioConfiguration sampleConfiguration, Environment environment) {
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
