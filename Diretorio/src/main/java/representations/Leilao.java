package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDateTime;
import java.util.List;

public class Leilao {
    public final long montanteTotalMax;
    public final long taxaDeJuroMax;
    public final LocalDateTime dataComeco;
    public final long intervaloTempo;
    public final String empresa;
    // public final List<Emissao> emissoes;

    @JsonCreator
    public Leilao(@JsonProperty("montanteTotalMax") long montanteTotalMax, @JsonProperty("taxaDeJuroMax") long taxaDeJuroMax, @JsonProperty("dataComeco") LocalDateTime dataComeco, @JsonProperty("intervaloTempo") long intervaloTempo, @JsonProperty("nomeEmpresa") String empresa) {
        this.montanteTotalMax = montanteTotalMax;
        this.taxaDeJuroMax = taxaDeJuroMax;
        this.dataComeco = dataComeco;
        this.intervaloTempo = intervaloTempo;
        this.empresa = empresa;
    }
}
