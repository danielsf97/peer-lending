package resources;

import exceptions.RestException;
import representations.LeilaoAtivo;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Map;

@Path("/leiloes")
@Produces(MediaType.APPLICATION_JSON)
public class LeiloesResource {
    private Map<Long, LeilaoAtivo> leiloesAtivos;

    public LeiloesResource(Map<Long, LeilaoAtivo> leiloesAtivos) {
        this.leiloesAtivos = leiloesAtivos;
    }

    @GET
    public Response get() {
        return Response.ok(leiloesAtivos.values()).build();
    }

    @POST
    public Response post(LeilaoAtivo l) {
        if(leiloesAtivos.containsKey(l.getId())) {
            final String msg = String.format("ID already exists!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        leiloesAtivos.put(l.getId(), l);
        return Response.ok().build();
    }

    @PUT
    public Response put(LeilaoAtivo l) {
        leiloesAtivos.put(l.getId(), l);
        return Response.ok().build();
    }
}