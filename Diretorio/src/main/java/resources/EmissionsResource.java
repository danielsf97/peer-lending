package resources;

import representations.ActiveEmission;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;

@Path("/emissions")
@Produces(MediaType.APPLICATION_JSON)
public class EmissionsResource {
    private List<ActiveEmission> activeEmissions;

    public EmissionsResource(List<ActiveEmission> activeEmissions) {
        this.activeEmissions = activeEmissions;
    }

    @GET
    public Response get() {
        return Response.ok(activeEmissions).build();
    }

    @PUT
    public Response put(ActiveEmission e) {
        activeEmissions.add(e);
        return Response.ok().build();
    }

    @DELETE
    @Path("/delete/{company}")
    public Response delete(@PathParam("company") String company) {
        for(ActiveEmission e : activeEmissions) {
            if (e.getCompany().equals("company")) {
                activeEmissions.remove(e);
            }
        }
        return Response.ok().build();
    }
}