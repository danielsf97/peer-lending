package representations;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Company {
    public String name;

    @JsonCreator
    public Company(@JsonProperty("name") String name) {
        this.name = name;
    }
}
