import core.History;
import health.AppHealthCheck;
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

public class DirectoryApp extends Application<Configuration> {
    private Map<String, History> companies;
    private List<ActiveAuction> activeAuctions;
    private List<ActiveEmission> activeEmissions;

    /**
     *
     *
     */
    public DirectoryApp() {
        this.companies = new HashMap<>();
        this.companies.put("empA", new History());
        this.companies.put("empB", new History());
        this.companies.put("empC", new History());
        this.activeAuctions = new ArrayList<>();
        this.activeEmissions = new ArrayList<>();
    }

    /**
     *
     * @return
     */
    @Override
    public String getName() {
        return "Directory";
    }

    /**
     *
     * @param bootstrap
     */
    @Override
    public void initialize(Bootstrap<Configuration> bootstrap) {
    }

    /**
     *
     * @param config
     * @param environment
     */
    @Override
    public void run(Configuration config, Environment environment) {
        environment.jersey().register(new CompaniesResource(companies));
        environment.jersey().register(new AuctionsResource(companies, activeAuctions, activeEmissions));
        environment.jersey().register(new EmissionsResource(companies, activeAuctions, activeEmissions));
        environment.healthChecks().register("isRunning", new AppHealthCheck());
    }


    /**
     * Corre o serviço REST
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        new DirectoryApp().run(args);
    }
}
