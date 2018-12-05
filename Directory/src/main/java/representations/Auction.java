package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Auction {

    public final long value;
    public final float maxRate;
    public String startingDateTime;
    public final long duration;
    public final float lowestRate;
    public final float highestRate;
    public final boolean wasSucessful;

    @JsonCreator
    public Auction(@JsonProperty("value") long value, @JsonProperty("maxRate") float maxRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration, @JsonProperty("lowestRate") float lowestRate, @JsonProperty("highestRate") float highestRate, @JsonProperty("wasSuccessful") boolean wasSucessful) {
        this.value = value;
        this.maxRate = maxRate;
        this.startingDateTime = startingDateTime;
        this.duration = duration;
        this.lowestRate = lowestRate;
        this.highestRate = highestRate;
        this.wasSucessful = wasSucessful;
    }

    public long getValue() {
        return value;
    }

    public float getMaxRate() {
        return maxRate;
    }

    public String getStartingDateTime() {
        return startingDateTime;
    }

    public long getDuration() {
        return duration;
    }

    public float getLowestRate() {
        return lowestRate;
    }

    public float getHighestRate() {
        return highestRate;
    }

    public boolean wasSucessful() {
        return wasSucessful;
    }
}
