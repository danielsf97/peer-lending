package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class LeilaoAtivo extends Leilao {

    public String empresa;

    @JsonCreator
    public LeilaoAtivo(@JsonProperty("id") long id, @JsonProperty("montanteTotalMax") long montanteTotalMax, @JsonProperty("taxaJuroMax") float taxaJuroMax, @JsonProperty("dataComeco") String dataComeco, @JsonProperty("duracao") long duracao, @JsonProperty("nomeEmpresa") String empresa) {
        super(id, montanteTotalMax, taxaJuroMax, dataComeco, duracao);
        this.empresa = empresa;
    }

    public long getId() {
        return id;
    }
}
