package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Emission {
    public final long value;
    public final float fixedRate;
    public String startingDateTime;
    public final long duration;

    @JsonCreator
    public Emission(@JsonProperty("value") long value, @JsonProperty("fixedRate") float fixedRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration) {
        this.value = value;
        this.fixedRate = fixedRate;
        this.startingDateTime = startingDateTime;
        this.duration = duration;
    }

    public long getValue() {
        return value;
    }

    public float getFixedRate() {
        return fixedRate;
    }

    public String getStartingDateTime() {
        return startingDateTime;
    }

    public long getDuration() {
        return duration;
    }
}
