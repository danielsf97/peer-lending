package representations;

import com.fasterxml.jackson.annotation.*;

public class Empresa {
    public final long id;
    public final String nome;

    @JsonCreator
    public Empresa(@JsonProperty("id") long id, @JsonProperty("nome") String nome) {
        this.id = id;
        this.nome = nome;
    }

    public long getId() {
        return this.id;
    }

}

