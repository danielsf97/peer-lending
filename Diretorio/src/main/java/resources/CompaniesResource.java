package resources;

import exceptions.RestException;
import representations.Company;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Map;


@Path("/companies")
@Produces(MediaType.APPLICATION_JSON)
public class CompaniesResource {
    Map<Long, Company> empresas;

    public CompaniesResource(Map<Long, Company> empresas) {
        this.empresas = empresas;
    }

    @GET
    public Response get() {
        return Response.ok(empresas.values()).build();
    }

    @PUT
    public Response put(Company e) {
        empresas.put(e.getId(), e);
        return Response.ok().build();
    }
}
