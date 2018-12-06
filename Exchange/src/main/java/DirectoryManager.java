import com.google.gson.Gson;

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DirectoryManager {
    private Gson gson;

    DirectoryManager() {
        this.gson = new Gson();
    }

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

    public void deleteHttp(String partialUri, String company) throws Exception {
        URL url = new URL("http://localhost:8080/companies/" + company + "/" + partialUri);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("DELETE");
        conn.setDoOutput(true);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.connect();

        if(conn.getResponseCode() != 200)
            throw new RuntimeException();
    }

    public void postAuction(Auction a, String company) throws Exception {
        AuctionRep auctionRep = new AuctionRep(a.getValue(), a.getMaxRate(), a.getStartingDateTime(), a.getDuration(), company);
        String auctionJson = gson.toJson(auctionRep);
        postHttp("http://localhost:8080/activeAuctions", auctionJson);
    }

    public void postEmission(Emission e, String company) throws Exception {
        EmissionRep emissionRep = new EmissionRep(e.getValue(), e.getFixedRate(), e.getStartingDateTime(), e.getDuration(), company);
        String emissionJson = gson.toJson(emissionRep);
        postHttp("http://localhost:8080/activeEmissions", emissionJson);
    }

    public void deleteAuction(String company) throws Exception {
        deleteHttp("auctionHistory", company);
    }

    public void deleteEmission(String company) throws Exception {
        deleteHttp("emissionHistory", company);
    }

    class AuctionRep {
        public long value;
        public float maxRate;
        public String startingDateTime;
        public long duration;
        public String company;

        public AuctionRep(long value, float maxRate, LocalDateTime startingDateTime, long duration, String company) {
            this.value = value;
            this.maxRate = maxRate;
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            this.startingDateTime = startingDateTime.format(formatter);
            this.duration = duration;
            this.company = company;
        }
    }

    class EmissionRep {
        public long value;
        public float fixedRate;
        public String startingDateTime;
        public long duration;
        public String company;

        public EmissionRep(long value, float fixedRate, LocalDateTime startingDateTime, long duration, String company) {
            this.value = value;
            this.fixedRate = fixedRate;
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            this.startingDateTime = startingDateTime.format(formatter);
            this.duration = duration;
            this.company = company;
        }
    }
}
