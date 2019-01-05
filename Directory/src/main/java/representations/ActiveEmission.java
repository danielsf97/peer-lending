package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ActiveEmission extends Emission {
    public String company;


    /**
     * Construtor parametrizado.
     *
     * @param value                 Valor da emissão.
     * @param fixedRate             Taxa fixa da emissão.
     * @param startingDateTime      Data e hora de começo da emissão.
     * @param duration              Duração da emissão.
     * @param company               Empresa que realizou a emissão.
     */
    @JsonCreator
    public ActiveEmission(@JsonProperty("value") long value, @JsonProperty("fixedRate") float fixedRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration, @JsonProperty("companyName") String company) {
        super(value, fixedRate, startingDateTime, duration);
        this.company = company;
    }


    /**
     * Obtém a empresa associada a uma emissão ativa.
     *
     * @return a empresa associada a uma emissão ativa.
     *
     */
    public String getCompany() {
        return this.company;
    }
}
