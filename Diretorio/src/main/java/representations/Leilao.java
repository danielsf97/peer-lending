package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Leilao {
    public final long id;
    public final long montanteTotalMax;
    public final float taxaJuroMax;
    public String dataComeco;
    public final long duracao;

    @JsonCreator
    public Leilao(@JsonProperty("id") long id, @JsonProperty("montanteTotalMax") long montanteTotalMax, @JsonProperty("taxaJuroMax") float taxaJuroMax, @JsonProperty("dataComeco") String dataComeco, @JsonProperty("duracao") long duracao) {
        this.id = id;
        this.montanteTotalMax = montanteTotalMax;
        this.taxaJuroMax = taxaJuroMax;
        this.dataComeco = dataComeco;
        this.duracao = duracao;
    }
}
