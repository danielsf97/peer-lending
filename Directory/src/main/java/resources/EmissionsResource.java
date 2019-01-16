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
 * Representa as emissões ativas.
 *
 */
@Path("/activeEmissions")
@Produces(MediaType.APPLICATION_JSON)
public class EmissionsResource {
    private Map<String, History> companies;
    private List<ActiveAuction> activeAuctions;
    private List<ActiveEmission> activeEmissions;


    /**
     * Construtor parametrizado.
     *
     * @param companies             Empresas existentes.
     * @param activeAuctions        Leilões ativos.
     * @param activeEmissions       Emissões ativas.
     */
    public EmissionsResource(Map<String, History> companies, List<ActiveAuction> activeAuctions, List<ActiveEmission> activeEmissions) {
        this.companies = companies;
        this.activeAuctions = activeAuctions;
        this.activeEmissions = activeEmissions;
    }


    /**
     * Retorna a lista de emissões ativas.
     *
     * @return      Resposta REST com emissões ativas.
     */
    @GET
    public Response get() {
        return Response.ok(activeEmissions).build();
    }


    /**
     * Coloca uma emissão ativa no diretório.
     * Falha caso a empresa da emissão não exista ou já exista um
     * leilão/emissão ativos para essa empresa.
     *
     * @param e     Emissão a adicionar.
     * @return      Resposta REST ao pedido.
     */
    @POST
    public Response post(ActiveEmission e) {
        if(!companies.containsKey(e.getCompany()))
            throw new RestException("A empresa não existe!", Response.Status.NOT_FOUND);

        boolean exists = false;
        for(ActiveAuction aa : activeAuctions) {
            if (aa.getCompany().equals(e.getCompany()))
                exists = true;
        }
        for(ActiveEmission ae: activeEmissions) {
            if (ae.getCompany().equals(e.getCompany()))
                exists = true;
        }
        if(exists)
            throw new RestException("Já existe um(a) leilão/emissão para essa empresa", Response.Status.CONFLICT);

        activeEmissions.add(e);
        return Response.ok().build();
    }


    /**
     * Apaga uma emissão ativa de uma determinada empresa. Esta operação
     * implica passar esta emissão para o histórico dessa empresa.
     * Falha caso a empresa não exista ou caso não exista leilão/emissão
     * para essa empresa.
     *
     * @param company       Empresa para a qual se pretende eliminar emissão.
     * @return              Resposta REST ao pedido.
     */
    @DELETE
    @Path("/{company}?success={success}")
    public Response delete(@PathParam("company") String company, @PathParam("success") String success) {
        if(!companies.containsKey(company))
            throw new RestException("A empresa não existe!", Response.Status.NOT_FOUND);

        ActiveEmission save = null;

        for(ActiveEmission e: activeEmissions) {
            if (e.getCompany().equals(company))
                save = e;
        }

        if(save == null) {
            throw new RestException("Não existe leilão ativo para essa empresa", Response.Status.NOT_FOUND);
        }

        activeEmissions.remove(save);

        save.setSuccess(success);

        History h = companies.get(company);
        h.addEmission(save);

        return Response.ok().build();
    }
}