import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import resources.EmpresasResource;
import resources.LeiloesResource;

public class DiretorioApp extends Application<DiretorioConfiguration> {

    @Override
    public String getName() {
        return "Diretorio";
    }

    @Override
    public void initialize(Bootstrap<DiretorioConfiguration> bootstrap) {

    }

    @Override
    public void run(DiretorioConfiguration sampleConfiguration, Environment environment) throws Exception {
        environment.jersey().register(new EmpresasResource(sampleConfiguration.template, sampleConfiguration.defaultName));
        environment.jersey().register(new LeiloesResource(sampleConfiguration.template, sampleConfiguration.defaultName));
    }

    public static void main(String[] args) throws Exception {
        new DiretorioApp().run(args);
    }
}
