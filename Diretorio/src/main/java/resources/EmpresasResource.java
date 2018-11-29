package resources;

import representations.Empresa;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;


@Path("/empresas")
@Produces(MediaType.APPLICATION_JSON)
public class EmpresasResource {
    private final String template;
    private volatile String defaultName;

    public EmpresasResource(String template, String defaultName) {
        this.template = template;
        this.defaultName = defaultName;
    }

    @GET
    public Response getEmpresas() {
        List<Empresa> empresas = new ArrayList<>();
        empresas.add(new Empresa(1, "empresa1"));
        empresas.add(new Empresa(2, "empresa2"));
        empresas.add(new Empresa(3, "empresa3"));
        return Response.ok(empresas).build();
    }

}
