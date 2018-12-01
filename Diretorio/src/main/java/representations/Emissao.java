package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Emissao {
    public final long id;
    public final long montanteTotalMax;
    public final float taxaJuroFixa;
    public String dataComeco;
    public final long duracao;

    @JsonCreator
    public Emissao(@JsonProperty("id") long id, @JsonProperty("montanteTotalMax") long montanteTotalMax, @JsonProperty("taxaJuroFixa") float taxaJuroFixa, @JsonProperty("dataComeco") String dataComeco, @JsonProperty("duracao") long duracao) {
        this.id = id;
        this.montanteTotalMax = montanteTotalMax;
        this.taxaJuroFixa = taxaJuroFixa;
        this.dataComeco = dataComeco;
        this.duracao = duracao;
    }

    public long getId() {
        return id;
    }
}
