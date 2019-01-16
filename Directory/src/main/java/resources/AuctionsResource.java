package resources;

import core.History;
import exceptions.RestException;
import representations.ActiveAuction;
import representations.ActiveEmission;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;


/**
 * Representa os leilões ativos.
 *
 */
@Path("/activeAuctions")
@Produces(MediaType.APPLICATION_JSON)
public class AuctionsResource {
    private Map<String, History> companies;
    private List<ActiveAuction> activeAuctions;
    private List<ActiveEmission> activeEmissions;


    /**
     * Construtor parametrizado do resource de leilões ativos.
     *
     * @param companies         Map com empresas e seu histórico.
     * @param activeAuctions    Lista de leilões ativos.
     * @param activeEmissions   Lista de emissões ativas.
     */
    public AuctionsResource(Map<String, History> companies, List<ActiveAuction> activeAuctions, List<ActiveEmission> activeEmissions) {
        this.companies = companies;
        this.activeAuctions = activeAuctions;
        this.activeEmissions = activeEmissions;
    }


    /**
     * Constrói uma resposta REST com a lista de leilões ativos.
     *
     * @return  uma resposta REST com a lista de leilões ativos.
     */
    @GET
    public Response get() {
        return Response.ok(activeAuctions).build();
    }

    /**
     * Coloca no diretório um novo leilão ativo.
     * Esta operação é impossível caso a empresa associada não exista
     * ou caso já exista um leilão/emissão a decorrer para essa mesma
     * empresa.
     *
     * @param a     Leilão ativo a adicionar.
     * @return      Resposta REST do pedido.
     */
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


    /**
     * Apaga do diretório um leilão ativo de uma determinada empresa. Esta
     * operação implica guardar este mesmo leilão no histórico desta.
     * Falha caso a empresa associada ao leilão ativo não exista ou não
     * exista um leilão ativo para essa empresa.
     *
     * @param company   Empresa para a qual se quer apagar o leilão ativo.
     * @return          Resposta REST ao pedido.
     */
    @DELETE
    @Path("/{company}?success={success}")
    public Response delete(@PathParam("company") String company, @PathParam("success") String success) {
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

        save.setSuccess(success);

        History h = companies.get(company);
        h.addAuction(save);

        return Response.ok().build();
    }
}