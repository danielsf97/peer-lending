package exceptions;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;


/**
 * Representa uma exceção REST, possibilitando associar à
 * mensagem o código HTTP status.
 *
 */
public class RestException extends WebApplicationException {


    /**
     * Construtor parametrizado.
     *
     * @param message   Mensagem da exceção.
     * @param status    Código HTTP de erro.
     */
    public RestException(String message, Response.Status status) {
        super(Response.status(status).entity(message).type(MediaType.TEXT_PLAIN).build());
    }
}