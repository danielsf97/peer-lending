/*
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Scanner;



public class Main {


    public static String empresasToString(JsonArray jarray) {
        StringBuilder sb = new StringBuilder();

        sb.append("LISTA DE EMPRESAS:").append("\n");
        sb.append("==================").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();
            sb.append("ID: ").append(o.get("id")).append("\n");
            sb.append("NOME: ").append(o.get("nome")).append("\n");
            sb.append("------------------").append("\n");
        }

        return sb.toString();
    }

    public static void main(String[] args) throws Exception {
        URL url = new URL("http://localhost:12345/empresas");

        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        conn.setRequestMethod("GET");

        conn.connect();

        int rc = conn.getResponseCode();

        String json_str = null;
        StringBuilder sb = new StringBuilder();


        if(rc != 200)
            throw new RuntimeException("HttpResponseCode: " + rc);
        else {
            Scanner sc = new Scanner(url.openStream());
            while(sc.hasNext())
            {
                sb.append(sc.nextLine());
            }

            json_str = sb.toString();
            sc.close();
        }

        JsonParser jp = new JsonParser();

        JsonElement tree = jp.parse(json_str);

        JsonArray jo = tree.getAsJsonArray();

        // -------------------------------------------------------




        // -------------------------------------------------------

        System.out.println(empresasToString(jo));

    }
}
*/
