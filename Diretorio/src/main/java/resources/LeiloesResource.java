package resources;

import representations.Emissao;
import representations.Leilao;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Path("/leiloes")
@Produces(MediaType.APPLICATION_JSON)
public class LeiloesResource {
    private final String template;
    private volatile String defaultName;

    public LeiloesResource(String template, String defaultName) {
        this.template = template;
        this.defaultName = defaultName;
    }

    @GET
    public Response getLeiloes() {
        List<Leilao> leiloes = new ArrayList<>();
        leiloes.add(new Leilao(10000, (long) 0.2, LocalDateTime.now(), 10, "empresa1"));
        leiloes.add(new Leilao(10000, (long) 0.2, LocalDateTime.now(), 10, "empresa2"));
        leiloes.add(new Leilao(10000, (long) 0.2, LocalDateTime.now(), 10, "empresa3"));

        return Response.ok(leiloes).build();
    }
}