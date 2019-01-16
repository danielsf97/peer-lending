package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ActiveAuction extends Auction {
    public String company;


    /**
     * Construtor parametrizado.
     *
     * @param value                 Valor do leilão.
     * @param maxRate               Taxa máxima admitida no leilão.
     * @param startingDateTime      Data e hora de começo do leilão.
     * @param duration              Duração do leilão.
     * @param company               Empresa associada ao leilão.
     */
    @JsonCreator
    public ActiveAuction(@JsonProperty("value") long value, @JsonProperty("maxRate") float maxRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration, @JsonProperty("companyName") String company, @JsonProperty("bool") String bool) {
        super(value, maxRate, startingDateTime, duration, bool);
        this.company = company;
    }


    /**
     * Obtém a empresa associada ao leilão.
     *
     * @return a empresa associada ao leilão.
     */
    public String getCompany() {
        return company;
    }

}
