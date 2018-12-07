package resources;

import com.fasterxml.jackson.annotation.JsonProperty;
import core.History;
import exceptions.RestException;
import representations.ActiveAuction;
import representations.ActiveEmission;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;

@Path("/activeAuctions")
@Produces(MediaType.APPLICATION_JSON)
public class AuctionsResource {
    private Map<String, History> companies;
    private List<ActiveAuction> activeAuctions;
    private List<ActiveEmission> activeEmissions;

    public AuctionsResource(Map<String, History> companies, List<ActiveAuction> activeAuctions, List<ActiveEmission> activeEmissions) {
        this.companies = companies;
        this.activeAuctions = activeAuctions;
        this.activeEmissions = activeEmissions;
    }

    @GET
    public Response get() {
        return Response.ok(activeAuctions).build();
    }

    @POST
    public Response post(ActiveAuction a) {
        if(!companies.containsKey(a.getCompany()))
            throw new RestException("A empresa não existe!", Response.Status.NOT_FOUND);

        boolean exists = false;
        for(ActiveAuction aa : activeAuctions) {
            if (aa.getCompany().equals(a.getCompany()))
                exists = true;
        }
        for(ActiveEmission ae: activeEmissions) {
            if (ae.getCompany().equals(a.getCompany()))
                exists = true;
        }
        if(exists)
            throw new RestException("Já existe um(a) leilão/emissão para essa empresa", Response.Status.CONFLICT);

        activeAuctions.add(a);

        return Response.ok().build();
    }

    @DELETE
    @Path("/{company}")
    public Response delete(@PathParam("company") String company) {
        if(!companies.containsKey(company))
            throw new RestException("A empresa não existe!", Response.Status.NOT_FOUND);

        ActiveAuction save = null;

        for(ActiveAuction a: activeAuctions) {
            if (a.getCompany().equals(company))
                save = a;
        }

        if(save == null) {
            throw new RestException("Não existe leilão ativo para essa empresa", Response.Status.NOT_FOUND);
        }

        activeAuctions.remove(save);

        History h = companies.get(company);
        h.addAuction(save);

        return Response.ok().build();
    }
}