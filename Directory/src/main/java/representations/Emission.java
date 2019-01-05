package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Emission {
    public final long value;
    public final float fixedRate;
    public String startingDateTime;
    public final long duration;


    /**
     * Construtor parametrizado.
     * @param value                 Valor da emissão.
     * @param fixedRate             Taxa fixa da emissão.
     * @param startingDateTime      Data e hora de começo da emissão.
     * @param duration              Duração da emissão.
     */
    @JsonCreator
    public Emission(@JsonProperty("value") long value, @JsonProperty("fixedRate") float fixedRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration) {
        this.value = value;
        this.fixedRate = fixedRate;
        this.startingDateTime = startingDateTime;
        this.duration = duration;
    }


    /**
     * Obtém o valor da emissão.
     *
     * @return o valor da emissão.
     */
    public long getValue() {
        return value;
    }


    /**
     * Obtém a taxa fixa da emissão.
     *
     * @return a taxa fixa da emissão.
     */
    public float getFixedRate() {
        return fixedRate;
    }


    /**
     * Obtém a data e hora de começo da emissão.
     *
     * @return data e hora de começo da emissão.
     */
    public String getStartingDateTime() {
        return startingDateTime;
    }


    /**
     * Obtém a duração da emissão.
     *
     * @return a duração da emissão.
     */
    public long getDuration() {
        return duration;
    }
}
