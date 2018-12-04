package utils;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Scanner;

public class NetClient {

    public static final int w = 79;

    // -------------------------------------------------------------------------------------------

    public static JsonElement getJsonTree(String url_j) throws Exception {
        URL url = new URL(url_j);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        conn.connect();

        int rc = conn.getResponseCode();

        String json_str = null;
        StringBuilder sb = new StringBuilder();

        BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));

        if (rc != 200)
            throw new RuntimeException(br.readLine());
        else {
            Scanner sc = new Scanner(url.openStream());
            while (sc.hasNext()) {
                sb.append(sc.nextLine());
            }

            json_str = sb.toString();
            sc.close();
        }

        JsonParser jp = new JsonParser();

        return jp.parse(json_str);
    }

    // -------------------------------------------------------------------------------------------

    public static String empresasToString(JsonArray jarray) {
        StringBuilder sb = new StringBuilder();

        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        sb.append(StringUtils.center(StringUtils.center("Lista de Empresas", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();
            sb.append(StringUtils.rightPad("| ID: " + o.get("id"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| NOME: " + o.get("nome"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        }

        return sb.toString();
    }

    public static String leiloesToString(JsonArray jarray, boolean ativo) {
        StringBuilder sb = new StringBuilder();

        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        sb.append(StringUtils.center(StringUtils.center("Lista de Leilões Ativos", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();
            sb.append(StringUtils.rightPad("| ID: " + o.get("id"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| MONTANTE TOTAL MÁXIMO: " + o.get("montanteTotalMax"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| TAXA DE JURO MÁXIMA: " + o.get("taxaJuroMax"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DATA DE COMEÇO: " + o.get("dataComeco"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DURAÇÃO: " + o.get("duracao"), w-1) + "|").append("\n");
            if(ativo)
                sb.append(StringUtils.rightPad("| EMPRESA: " + o.get("empresa"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        }

        return sb.toString();
    }

    public static String emissoesToString(JsonArray jarray, boolean ativo) {
        StringBuilder sb = new StringBuilder();

        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        sb.append(StringUtils.center(StringUtils.center("Lista de Emissões Ativas", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();
            sb.append(StringUtils.rightPad("| ID: " + o.get("id"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| MONTANTE TOTAL MÁXIMO: " + o.get("montanteTotalMax"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| TAXA DE JURO FIXA: " + o.get("taxaJuroFixa"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DATA DE COMEÇO: " + o.get("dataComeco"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DURAÇÃO: " + o.get("duracao"), w-1) + "|").append("\n");
            if(ativo)
                sb.append(StringUtils.rightPad("| EMPRESA: " + o.get("empresa"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        }

        return sb.toString();
    }

    // -------------------------------------------------------------------------------------------

    public static String getEmpresas() throws Exception {
        JsonElement tree = getJsonTree("http://localhost:12346/empresas");
        JsonArray empresas_array = tree.getAsJsonArray();
        return empresasToString(empresas_array);

    }

    public static String getLeiloesAtivos() throws Exception {
        JsonElement tree = getJsonTree("http://localhost:12346/leiloes");
        JsonArray leat_array = tree.getAsJsonArray();
        return leiloesToString(leat_array, true);
    }

    public static String getEmissoesAtivas() throws Exception {
        JsonElement tree = getJsonTree("http://localhost:12346/emissoes");
        JsonArray emat_array = tree.getAsJsonArray();
        return emissoesToString(emat_array, true);
    }

    public static String getHistoricoLeiloes(long empresa) throws Exception {
        JsonElement tree = getJsonTree("http://localhost:12346/historico_leiloes/" + empresa);
        JsonArray emat_array = tree.getAsJsonArray();
        return leiloesToString(emat_array, false);
    }

    public static String getHistoricoEmissoes(long empresa) throws Exception {
        JsonElement tree = getJsonTree("http://localhost:12346/historico_emissoes/" + empresa);
        JsonArray emat_array = tree.getAsJsonArray();
        return emissoesToString(emat_array, false);
    }
}
