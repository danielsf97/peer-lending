package resources;

import exceptions.RestException;
import representations.Leilao;
import representations.Empresa;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Path("/historico_leiloes")
@Produces(MediaType.APPLICATION_JSON)
public class HistoricoLeiloesResource {
    Map<Long, Empresa> empresas;
    Map<Long, List<Leilao>> historico_leiloes;

    public HistoricoLeiloesResource(Map<Long, Empresa> empresas, Map<Long, List<Leilao>> historico_leiloes) {
        this.empresas = empresas;
        this.historico_leiloes = historico_leiloes;
    }

    @GET
    @Path("/{id}")
    public Response get(@PathParam("id") long id) {
        if(!empresas.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(empresas.containsKey(id) && !historico_leiloes.containsKey(id)) {
            historico_leiloes.put(id, new ArrayList<>());
        }

        return Response.ok(historico_leiloes.get(id)).build();
    }

    @PUT
    @Path("/{id}")
    public Response put(@PathParam("id") long id, Leilao l) {

        if(!empresas.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(empresas.containsKey(id) && !historico_leiloes.containsKey(id)) {
            historico_leiloes.put(id, new ArrayList<>());
        }

        List<Leilao> leiloes = historico_leiloes.get(id);
        leiloes.add(l);
        historico_leiloes.put(id, leiloes);

        return Response.ok().build();
    }
}
