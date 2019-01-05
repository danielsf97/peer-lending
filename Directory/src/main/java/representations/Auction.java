package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Auction {
    public final long value;
    public final float maxRate;
    public final long duration;
    public String startingDateTime;


    /**
     * Construtor parametrizado.
     *
     * @param value                 Valor de um leilão.
     * @param maxRate               Taxa máxima do leilão.
     * @param startingDateTime      Data e hora de começo do leilão.
     * @param duration              Duração do leilão.
     */
    @JsonCreator
    public Auction(@JsonProperty("value") long value, @JsonProperty("maxRate") float maxRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration) {
        this.value = value;
        this.maxRate = maxRate;
        this.startingDateTime = startingDateTime;
        this.duration = duration;
    }


    /**
     * Obtém o valor de um leilão.
     *
     * @return o valor de um leilão.
     */
    public long getValue() {
        return value;
    }


    /**
     * Obtém a taxa máxima admitida num leilão.
     *
     * @return Taxa máxima admitida num leilão.
     */
    public float getMaxRate() {
        return maxRate;
    }


    /**
     * Obtém a hora e data de começo de um leilão.
     *
     * @return hora e data de começo de um leilão.
     */
    public String getStartingDateTime() {
        return startingDateTime;
    }


    /**
     * Obtém a duração de um leilão.
     *
     * @return duração de um leilão.
     */
    public long getDuration() {
        return duration;
    }
}
