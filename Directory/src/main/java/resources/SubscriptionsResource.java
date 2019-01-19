package resources;

import exceptions.RestException;
import representations.ActiveAuction;
import representations.ActiveEmission;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Representa as emissões ativas.
 *
 */
@Path("/subscriptions")
@Produces(MediaType.APPLICATION_JSON)
public class SubscriptionsResource {
    Map<String, List<String>> subscriptions;


    /**
     * Construtor parametrizado.
     *
     * @param subscriptions     Subscrições dos investidores.
     */
    public SubscriptionsResource(Map<String, List<String>> subscriptions) {
        this.subscriptions = subscriptions;
    }


    /**
     * Retorna a lista de subscrições de um investidor.
     *
     * @return      Lista de subscrições de um investidor.
     */
    @GET
    @Path("/{investor}")
    public Response get(@PathParam("investor") String investor) {
        if(!subscriptions.containsKey(investor))
            throw new RestException("O investidor não existe!", Response.Status.NOT_FOUND);

        return Response.ok(subscriptions.get(investor)).build();
    }


    /**
     * Adiciona uma subscrição a um investidor. Caso este não exista, cria-o.
     *
     */
    @POST
    @Path("/{investor}/{subscription}")
    public Response post(@PathParam("investor") String investor, @PathParam("subscription") String subscription) {
        if(!subscriptions.containsKey(investor)) {
            subscriptions.put(investor, new ArrayList<>());
        }

        List<String> tmp = subscriptions.get(investor);
        tmp.add(subscription);
        subscriptions.put(investor, tmp);

        return Response.ok().build();
    }
}