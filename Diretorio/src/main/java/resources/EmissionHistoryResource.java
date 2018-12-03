package resources;

import exceptions.RestException;
import representations.Company;
import representations.Emission;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

@Path("/emissionHistory")
@Produces(MediaType.APPLICATION_JSON)
public class EmissionHistoryResource {
    Map<Long, Company> companies;
    Map<Long, List<Emission>> emissionHistory;

    public EmissionHistoryResource(Map<Long, Company> companies, Map<Long, List<Emission>> emissionHistory) {
        this.companies = companies;
        this.emissionHistory = emissionHistory;
    }

    @GET
    @Path("/{id}")
    public Response get(@PathParam("id") long id) {
        if(!companies.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(companies.containsKey(id) && !emissionHistory.containsKey(id)) {
            emissionHistory.put(id, new ArrayList<>());
        }

        Collection<Emission> es = emissionHistory.get(id);
        return Response.ok(es).build();
    }

    @PUT
    @Path("/{id}")
    public Response put(@PathParam("id") long id, Emission e) {

        if(!companies.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(companies.containsKey(id) && !emissionHistory.containsKey(id)) {
            emissionHistory.put(id, new ArrayList<>());
        }
        List<Emission> emissions = emissionHistory.get(id);
        emissions.add(e);
        emissionHistory.put(id, emissions);

        return Response.ok().build();
    }
}
