package resources;

import exceptions.RestException;
import representations.Emissao;
import representations.Empresa;
import representations.Leilao;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Path("/historico_emissoes")
@Produces(MediaType.APPLICATION_JSON)
public class HistoricoEmissoesResource {
    Map<Long, Empresa> empresas;
    Map<Long, List<Emissao>> historico_emissoes;

    public HistoricoEmissoesResource(Map<Long, Empresa> empresas, Map<Long, List<Emissao>> historico_emissoes) {
        this.empresas = empresas;
        this.historico_emissoes = historico_emissoes;
    }

    @GET
    @Path("/{id}")
    public Response get(@PathParam("id") long id) {
        if(!empresas.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(empresas.containsKey(id) && !historico_emissoes.containsKey(id)) {
            historico_emissoes.put(id, new ArrayList<>());
        }

        Collection<Emissao> es = historico_emissoes.get(id);
        return Response.ok(es).build();
    }

    @PUT
    @Path("/{id}")
    public Response put(@PathParam("id") long id, Emissao e) {

        if(!empresas.containsKey(id)) {
            final String msg = String.format("Empresa não existe!\n");
            throw new RestException(msg, Response.Status.NOT_FOUND);
        }
        else if(empresas.containsKey(id) && !historico_emissoes.containsKey(id)) {
            historico_emissoes.put(id, new ArrayList<>());
        }
        List<Emissao> emissoes = historico_emissoes.get(id);
        emissoes.add(e);
        historico_emissoes.put(id, emissoes);

        return Response.ok().build();
    }
}
