package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ActiveAuction {

    public final long value;
    public final float maxRate;
    public String startingDateTime;
    public final long duration;
    public String company;

    @JsonCreator
    public ActiveAuction(@JsonProperty("value") long value, @JsonProperty("maxRate") float maxRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration, @JsonProperty("companyName") String company) {
        this.value = value;
        this.maxRate = maxRate;
        this.startingDateTime = startingDateTime;
        this.duration = duration;
        this.company = company;
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

    public String getCompany() {
        return company;
    }
}
