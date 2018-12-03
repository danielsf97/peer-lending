package resources;

import exceptions.RestException;
import representations.Auction;
import representations.Company;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Path("/auctionHistory")
@Produces(MediaType.APPLICATION_JSON)
public class AuctionHistoryResource {
    Map<Long, Company> companies;
    Map<Long, List<Auction>> auctionHistory;

    public AuctionHistoryResource(Map<Long, Company> companies, Map<Long, List<Auction>> auctionHistory) {
        this.companies = companies;
        this.auctionHistory = auctionHistory;
    }

    @GET
    @Path("/{id}")
    public Response get(@PathParam("id") long id) {
        if(!companies.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(companies.containsKey(id) && !auctionHistory.containsKey(id)) {
            auctionHistory.put(id, new ArrayList<>());
        }

        return Response.ok(auctionHistory.get(id)).build();
    }

    @PUT
    @Path("/{id}")
    public Response put(@PathParam("id") long id, Auction l) {

        if(!companies.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(companies.containsKey(id) && !auctionHistory.containsKey(id)) {
            auctionHistory.put(id, new ArrayList<>());
        }

        List<Auction> auctions = auctionHistory.get(id);
        auctions.add(l);
        auctionHistory.put(id, auctions);

        return Response.ok().build();
    }
}
