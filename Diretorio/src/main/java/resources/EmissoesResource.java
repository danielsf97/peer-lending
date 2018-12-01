package resources;

import exceptions.RestException;
import representations.EmissaoAtiva;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Map;

@Path("/emissoes")
@Produces(MediaType.APPLICATION_JSON)
public class EmissoesResource {
    private Map<Long, EmissaoAtiva> emissoesAtivas;

    public EmissoesResource(Map<Long, EmissaoAtiva> emissoesAtivas) {
        this.emissoesAtivas = emissoesAtivas;
    }

    @GET
    public Response get() {
        return Response.ok(emissoesAtivas.values()).build();
    }

    @POST
    public Response post(EmissaoAtiva e) {
        if(emissoesAtivas.containsKey(e.getId())) {
            final String msg = String.format("ID already exists!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        emissoesAtivas.put(e.getId(), e);
        return Response.ok().build();
    }

    @PUT
    public Response put(EmissaoAtiva e) {
        emissoesAtivas.put(e.getId(), e);
        return Response.ok().build();
    }
}