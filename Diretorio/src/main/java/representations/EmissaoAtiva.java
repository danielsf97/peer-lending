package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class EmissaoAtiva extends Emissao {
    public String empresa;

    @JsonCreator
    public EmissaoAtiva(@JsonProperty("id") long id, @JsonProperty("montanteTotalMax") long montanteTotalMax, @JsonProperty("taxaJuroFixa") float taxaJuroFixa, @JsonProperty("dataComeco") String dataComeco, @JsonProperty("duracao") long duracao, @JsonProperty("nomeEmpresa") String empresa) {
        super(id, montanteTotalMax, taxaJuroFixa, dataComeco, duracao);
        this.empresa = empresa;
    }

    public long getId() {
        return id;
    }

}
