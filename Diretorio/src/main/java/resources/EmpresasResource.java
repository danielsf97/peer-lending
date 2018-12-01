package resources;

import exceptions.RestException;
import representations.Empresa;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;


@Path("/empresas")
@Produces(MediaType.APPLICATION_JSON)
public class EmpresasResource {
    Map<Long, Empresa> empresas;

    public EmpresasResource(Map<Long, Empresa> empresas) {
        this.empresas = empresas;
    }

    @GET
    public Response get() {
        return Response.ok(empresas.values()).build();
    }

    @POST
    public Response post(Empresa e) {
        if(empresas.containsKey(e.getId())) {
            final String msg = String.format("ID already exists!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        empresas.put(e.getId(), e);
        return Response.ok().build();
    }

    @PUT
    public Response put(Empresa e) {
        empresas.put(e.getId(), e);
        return Response.ok().build();
    }
}
