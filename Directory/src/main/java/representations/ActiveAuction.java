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
    public ActiveAuction(@JsonProperty("value") long value, @JsonProperty("maxRate") float maxRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration, @JsonProperty("companyName") String company) {
        super(value, maxRate, startingDateTime, duration);
        this.company = company;
    }


    /**
     * Obtém o valor do leilão.
     *
     * @return o valor do leilão.
     */
    public long getValue() {
        return value;
    }


    /**
     * Obtém a taxa máxima admitida no leilão.
     *
     * @return taxa máxima admitida no leilão.
     */
    public float getMaxRate() {
        return maxRate;
    }


    /**
     * Obtém a hora e data de começo do leilão.
     *
     * @return hora e data de começo do leilão.
     */
    public String getStartingDateTime() {
        return startingDateTime;
    }


    /**
     * Obtém a duração do leilão.
     *
     * @return a duração do leilão.
     */
    public long getDuration() {
        return duration;
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
