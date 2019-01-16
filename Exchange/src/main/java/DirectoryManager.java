import com.google.gson.Gson;

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;


/**
 * Responsável por enviar pedidos ao diretório através da interface
 * RESTful oferecida por este.
 *
 */
public class DirectoryManager {
    private Gson gson;


    /**
     * Construtor vazio.
     *
     */
    DirectoryManager() {
        this.gson = new Gson();
    }


    /**
     * Publica para um determinado URI um body passado como parâmetro.
     *
     * @param uri           URI para o qual se pretende publicar.
     * @param json          Body a publicar.
     * @throws Exception
     */
    public void postHttp(String uri, String json) throws Exception {
        byte[] postData = json.getBytes(StandardCharsets.UTF_8);
        int postLength = postData.length;
        URL url = new URL(uri);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setFixedLengthStreamingMode(postLength);
        conn.setRequestMethod("POST");
        conn.setDoOutput(true);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setInstanceFollowRedirects(false);
        conn.setUseCaches( false );

        conn.connect();

        OutputStream os = conn.getOutputStream();
        os.write(json.getBytes());
        os.flush();

        if(conn.getResponseCode() != 200) {
            throw new RuntimeException();
        }
    }

    /**
     * Envia pedido de DELETE ao serviço RESTful.
     *
     * @param partialUri       URI parcial para o qual se pretende enviar DELETE.
     * @param company          Nome da empresa.
     * @throws Exception
     */
    public void deleteHttp(String partialUri, String company, String success) throws Exception {
        URL url = new URL("http://localhost:8080/" + partialUri + "/" + company + "/" + success);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("DELETE");
        conn.setDoOutput(true);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.connect();

        if(conn.getResponseCode() != 200)
            throw new RuntimeException();
    }


    /**
     * Publica um determinado leilão ativo no diretório.
     *
     * @param a             Leilão a publicar.
     * @param company       Empresa para a qual publicar leilão.
     * @throws Exception
     */
    public void postAuction(Auction a, String company) throws Exception {
        AuctionRep auctionRep = new AuctionRep(a.getValue(), a.getMaxRate(), a.getStartingDateTime(), a.getDuration(), company, "N/A");
        String auctionJson = gson.toJson(auctionRep);
        postHttp("http://localhost:8080/activeAuctions", auctionJson);
    }

    /**
     * Publica uma emissão ativa no diretório.
     *
     * @param e             Emissão a publicar.
     * @param company       Empresa para a qual publicar emissão.
     * @throws Exception
     */
    public void postEmission(Emission e, String company) throws Exception {
        EmissionRep emissionRep = new EmissionRep(e.getValue(), e.getFixedRate(), e.getStartingDateTime(), e.getDuration(), company, "N/A");
        String emissionJson = gson.toJson(emissionRep);
        postHttp("http://localhost:8080/activeEmissions", emissionJson);
    }


    /**
     * Apaga um leilão ativo de uma determinada empresa do diretório.
     *
     * @param company       Empresa para a qual apagar leilão ativo.
     * @throws Exception
     */
    public void deleteAuction(String company, String success) throws Exception {
        deleteHttp("activeAuctions", company, success);
    }


    /**
     * Apaga uma emissão ativa de uma determinada empresa do diretório.
     *
     * @param company       Empresa para a qual apagar emissão ativa.
     * @throws Exception
     */
    public void deleteEmission(String company, String success) throws Exception {
        deleteHttp("activeEmissions", company, success);
    }


    /**
     * Representação de um leilão para conversão para JSON.
     *
     */
    class AuctionRep {
        public long value;
        public float maxRate;
        public String startingDateTime;
        public long duration;
        public String company;
        public String success;

        public AuctionRep(long value, float maxRate, LocalDateTime startingDateTime, long duration, String company, String success) {
            this.value = value;
            this.maxRate = maxRate;
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            this.startingDateTime = startingDateTime.format(formatter);
            this.duration = duration;
            this.company = company;
            this.success = success;
        }
    }

    /**
     * Representação de uma emissão para conversão para JSON.
     *
     */
    class EmissionRep {
        public long value;
        public float fixedRate;
        public String startingDateTime;
        public long duration;
        public String company;
        public String success;

        public EmissionRep(long value, float fixedRate, LocalDateTime startingDateTime, long duration, String company, String success) {
            this.value = value;
            this.fixedRate = fixedRate;
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            this.startingDateTime = startingDateTime.format(formatter);
            this.duration = duration;
            this.company = company;
            this.success = success;
        }
    }
}
