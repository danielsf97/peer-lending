package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ActiveEmission extends Emission {
    public String company;

    @JsonCreator
    public ActiveEmission(@JsonProperty("value") long value, @JsonProperty("fixedRate") float fixedRate, @JsonProperty("startingDateTime") String startingDateTime, @JsonProperty("duration") long duration, @JsonProperty("companyName") String company) {
        super(value, fixedRate, startingDateTime, duration);
        this.company = company;
    }
    public String getCompany() {
        return this.company;
    }
}
