package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Auction {
    public final long value;
    public final float maxRate;
    public String startingDateTime;
    public final long duration;

    @JsonCreator
    public Auction(@JsonProperty("value") long value, @JsonProperty("maxRate") float maxRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration) {
        this.value = value;
        this.maxRate = maxRate;
        this.startingDateTime = startingDateTime;
        this.duration = duration;
    }
}
