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

/**
 * Serviço RESTful para obter informações acerca de leilões/emissões
 * a decorrer.
 *
 */
public class DirectoryApp extends Application<Configuration> {
    private Map<String, History> companies;
    private List<ActiveAuction> activeAuctions;
    private List<ActiveEmission> activeEmissions;
    private Map<String, List<String>> subscriptions;


    /**
     * Construtor vazio
     *
     */
    public DirectoryApp() {
        this.companies = new HashMap<>();
        this.companies.put("empA", new History());
        this.companies.put("empB", new History());
        this.companies.put("empC", new History());
        this.activeAuctions = new ArrayList<>();
        this.activeEmissions = new ArrayList<>();
        this.subscriptions = new HashMap<>();
    }


    /**
     * Retorna o nome da aplicação.
     *
     * @return nome da aplicação.
     */
    @Override
    public String getName() {
        return "Directory";
    }

    /**
     * Inicializa o bootstrap da aplicação.
     *
     * @param bootstrap
     */
    @Override
    public void initialize(Bootstrap<Configuration> bootstrap) {
    }

    /**
     * Faz parse aos argumentos da linha de comandos e corre a aplicação.
     *
     * @param config
     * @param environment
     */
    @Override
    public void run(Configuration config, Environment environment) {
        environment.jersey().register(new CompaniesResource(companies));
        environment.jersey().register(new AuctionsResource(companies, activeAuctions, activeEmissions));
        environment.jersey().register(new EmissionsResource(companies, activeAuctions, activeEmissions));
        environment.jersey().register(new SubscriptionsResource(subscriptions));
        environment.healthChecks().register("isRunning", new AppHealthCheck());
    }


    /**
     * Corre o serviço REST.
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        new DirectoryApp().run(args);
    }
}
