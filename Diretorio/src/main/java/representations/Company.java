package representations;

import com.fasterxml.jackson.annotation.*;

public class Company {
    public final long id;
    public final String name;

    @JsonCreator
    public Company(@JsonProperty("id") long id, @JsonProperty("name") String name) {
        this.id = id;
        this.name = name;
    }

    public long getId() {
        return this.id;
    }

}

