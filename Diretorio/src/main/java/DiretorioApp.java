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
    private Map<Long, Empresa> empresas;
    private Map<Long, LeilaoAtivo> leiloesAtivos;
    private Map<Long, EmissaoAtiva> emissoesAtivas;
    private Map<Long, List<Leilao>> historico_leiloes;
    private Map<Long, List<Emissao>> historico_emissoes;


    @Override
    public String getName() {
        return "Diretorio";
    }

    @Override
    public void initialize(Bootstrap<DiretorioConfiguration> bootstrap) {
        this.empresas = new HashMap<>();
        this.leiloesAtivos = new HashMap<>();
        this.emissoesAtivas = new HashMap<>();
        this.historico_leiloes = new HashMap<>();
        this.historico_emissoes = new HashMap<>();
        for(Empresa e : this.empresas.values()) {
            historico_leiloes.put(e.getId(), new ArrayList<>());
            historico_emissoes.put(e.getId(), new ArrayList<>());
        }
    }

    @Override
    public void run(DiretorioConfiguration sampleConfiguration, Environment environment) {
        environment.jersey().register(new EmpresasResource(empresas));
        environment.jersey().register(new LeiloesResource(leiloesAtivos));
        environment.jersey().register(new EmissoesResource(emissoesAtivas));
        environment.jersey().register(new HistoricoLeiloesResource(empresas, historico_leiloes));
        environment.jersey().register(new HistoricoEmissoesResource(empresas, historico_emissoes));
    }

    public static void main(String[] args) throws Exception {
        new DiretorioApp().run(args);
    }
}
