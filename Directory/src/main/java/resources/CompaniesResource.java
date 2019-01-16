package resources;

import core.History;
import exceptions.RestException;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Map;


/**
 *  Representa as empresas existentes e os seus históricos.
 *
 */
@Path("/companies")
@Produces(MediaType.APPLICATION_JSON)
public class CompaniesResource {
    Map<String, History> companies;

    /**
     * Construtor parametrizado.
     *
     * @param companies     Map com empresas existentes e o seu histórico.
     */
    public CompaniesResource(Map<String, History> companies) {
        this.companies = companies;
    }


    /**
     * Lista as empresas existentes.
     *
     * @return resposta REST ao pedido.
     */
    @GET
    public Response getCompanies() {
        return Response.ok(companies.keySet()).build();
    }


    /**
     * Lista o histórico de leilões de uma determinada empresa.
     * Falha caso a empresa dada não exista.
     *
     * @param name      Nome da empresa.
     * @return          Resposta REST ao pedido.
     */
    @GET
    @Path("/{name}/auctionHistory")
    public Response getCompanyAuctionHistory(@PathParam("name") String name) {
        if(!companies.containsKey(name)) {
            final String msg = String.format("Empresa não existe!");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        return Response.ok(companies.get(name).getAuctions()).build();
    }


    /**
     * Lista o histórico de emissões de uma determinada empresa.
     * Falha caso a empresa dada não exista.
     *
     * @param name      Nome da empresa.
     * @return          Resposta REST ao pedido.
     */
    @GET
    @Path("/{name}/emissionHistory")
    public Response getCompanyEmissionHistory(@PathParam("name") String name) {
        if(!companies.containsKey(name))
            throw new RestException("A empresa não existe!", Response.Status.NOT_FOUND);

        return Response.ok(companies.get(name).getEmissions()).build();
    }
}
