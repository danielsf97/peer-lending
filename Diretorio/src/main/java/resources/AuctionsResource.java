package resources;

import representations.ActiveAuction;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;

@Path("/auctions")
@Produces(MediaType.APPLICATION_JSON)
public class AuctionsResource {
    private List<ActiveAuction> activeAuctions;

    public AuctionsResource(List<ActiveAuction> leiloesAtivos) {
        this.activeAuctions = leiloesAtivos;
    }

    @GET
    public Response get() {
        return Response.ok(activeAuctions).build();
    }

    @PUT
    public Response put(ActiveAuction l) {
        activeAuctions.add(l);
        return Response.ok().build();
    }

    @DELETE
    @Path("/delete/{company}")
    public Response delete(@PathParam("company") String company) {
        for(ActiveAuction l : activeAuctions) {
            if (l.getCompany().equals("company")) {
                activeAuctions.remove(l);
            }
        }
        return Response.ok().build();
    }
}