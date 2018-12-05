package resources;

import core.History;
import exceptions.RestException;
import representations.Company;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;


@Path("/companies")
@Produces(MediaType.APPLICATION_JSON)
public class CompaniesResource {
    Map<String, History> companies;

    public CompaniesResource(Map<String, History> companies) {
        this.companies = companies;
    }

    @GET
    public Response getCompanies() {
        return Response.ok(companies.keySet()).build();
    }

    @GET
    @Path("{name}/auctionHistory")
    public Response getCompanyAuctionHistory(@PathParam("name") String name) {
        if(!companies.containsKey(name)) {
            final String msg = String.format("Empresa não existe!");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        return Response.ok(companies.get(name).getAuctions()).build();
    }

    @GET
    @Path("{name}/emissionHistory")
    public Response getCompanyEmissionHistory(@PathParam("name") String name) {
        if(!companies.containsKey(name))
            throw new RestException("A empresa não existe!", Response.Status.NOT_FOUND);

        return Response.ok(companies.get(name).getEmissions()).build();
    }
}
