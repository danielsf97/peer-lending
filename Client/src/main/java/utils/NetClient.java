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
import java.util.ArrayList;
import java.util.Scanner;


/**
 * Cliente para o diretório, com o propósito de obter informação acerca de empresas
 * existentes, leilões/emissões ativos e históricos de empresas.
 */
public class NetClient {

    public static final int w = 79;

    // -------------------------------------------------------------------------------------------


    /**
     * Efetua o pedido REST e obtém a resposta em Json.
     *
     * @param uri       URI do pedido.
     * @return          Resposta em Json do pedido.
     */
    public static JsonElement getJsonTree(String uri) throws Exception {
        URL url = new URL(uri);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.connect();

        int rc = conn.getResponseCode();

        String json_str = null;
        StringBuilder sb = new StringBuilder();

        if(rc != 200) {
            BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
            throw new RuntimeException(br.readLine());
        }
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

    /**
     * Publica para um determinado URI.
     *
     * @param uri           URI para o qual se pretende publicar.
     * @throws Exception
     */
    public static void postHttp(String uri) throws Exception {
        URL url = new URL(uri);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setDoOutput(true);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setInstanceFollowRedirects(false);
        conn.setUseCaches(false);

        conn.connect();

        if(conn.getResponseCode() != 200) {
            BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
            throw new RuntimeException(br.readLine());
        }
    }


    /**
     * Apaga de determinado URI.
     *
     * @param uri           URI para do qual se pretende apagar.
     * @throws Exception
     */
    public static void deleteHttp(String uri) throws Exception {
        URL url = new URL(uri);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("DELETE");
        conn.setDoOutput(true);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.connect();

        if(conn.getResponseCode() != 200) {
            BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
            throw new RuntimeException(br.readLine());
        }
    }

    // -------------------------------------------------------------------------------------------


    /**
     * Constrói a representação textual de uma lista de empresas.
     *
     * @param jarray        JsonArray.
     * @return              Representação textual de uma lista de empresas.
     */
    public static String companiesToString(JsonArray jarray) {
        StringBuilder sb = new StringBuilder();

        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        sb.append(StringUtils.center(StringUtils.center("Lista de Empresas", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            sb.append(StringUtils.rightPad("| NOME: " + e, w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        }

        return sb.toString();
    }


    /**
     * Constrói a representação textual de uma lista de leilões.
     *
     * @param jarray        JsonArray.
     * @param active        Indicação de se tratar de uma lista de leilões ativos ou não.
     * @return              Representação textual de uma lista de leilões.
     */
    public static String auctionsToString(JsonArray jarray, boolean active) {
        StringBuilder sb = new StringBuilder();

        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        if(active)
            sb.append(StringUtils.center(StringUtils.center("Lista de Leilões Ativos", w - 2), w, "|")).append("\n");
        else
            sb.append(StringUtils.center(StringUtils.center("Histórico de leilões", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();
            sb.append(StringUtils.rightPad("| MONTANTE TOTAL MÁXIMO: " + o.get("value"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| TAXA DE JURO MÁXIMA: " + o.get("maxRate"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DATA DE COMEÇO: " + o.get("startingDateTime"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DURAÇÃO: " + o.get("duration"), w-1) + "|").append("\n");
            if(active)
                sb.append(StringUtils.rightPad("| EMPRESA: " + o.get("company"), w-1) + "|").append("\n");
            else
                sb.append(StringUtils.rightPad("| SUCESSO: " + o.get("success"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        }

        return sb.toString();
    }


    /**
     * Constrói a representação textual de uma lista de emissões.
     *
     * @param jarray        JsonArray.
     * @param active        Indicação de se tratar de uma lista de emissões ativas ou não.
     * @return              Representação textual de uma lista de emissões.
     */
    public static String emissionsToString(JsonArray jarray, boolean active) {
        StringBuilder sb = new StringBuilder();

        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        if(active)
            sb.append(StringUtils.center(StringUtils.center("Lista de Emissões Ativas", w - 2), w, "|")).append("\n");
        else
            sb.append(StringUtils.center(StringUtils.center("Histórico de emissões", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();

            sb.append(StringUtils.rightPad("| MONTANTE TOTAL MÁXIMO: " + o.get("value"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| TAXA DE JURO FIXA: " + o.get("fixedRate"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DATA DE COMEÇO: " + o.get("startingDateTime"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DURAÇÃO: " + o.get("duration"), w-1) + "|").append("\n");
            if(active)
                sb.append(StringUtils.rightPad("| EMPRESA: " + o.get("company"), w-1) + "|").append("\n");
            else
                sb.append(StringUtils.rightPad("| SUCESSO: " + o.get("success"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        }

        return sb.toString();
    }

    // -------------------------------------------------------------------------------------------


    /**
     * Envia pedido ao diretório para listagem de empresas e retorna a representação textual
     * destas.
     *
     * @return                  Representação textual das empresas existentes.
     * @throws Exception
     */
    public static String getCompanies() throws Exception {
        JsonElement tree = getJsonTree("http://localhost:8080/companies");
        JsonArray companiesArray = tree.getAsJsonArray();
        if (companiesArray.size() == 0)
            throw new RuntimeException("Não existem empresas");
        return companiesToString(companiesArray);

    }


    /**
     * Envia pedido ao diretório para listagem dos leilões ativos e retorna a representação
     * textual destes.
     *
     * @return                  Representação textual dos leilões ativos.
     * @throws Exception
     */
    public static String getActiveAuctions() throws Exception {
        JsonElement tree = getJsonTree("http://localhost:8080/activeAuctions");
        JsonArray aaArray = tree.getAsJsonArray();
        if (aaArray.size() == 0)
            throw new RuntimeException("Não existem leilões ativos!");
        return auctionsToString(aaArray, true);
    }


    /**
     * Envia pedido ao diretório para listagem das emissões ativas e retorna a representação
     * textual destas.
     *
     * @return                  Representação textual das emissões ativas.
     * @throws Exception
     */
    public static String getActiveEmissions() throws Exception {
        JsonElement tree = getJsonTree("http://localhost:8080/activeEmissions");
        JsonArray aeArray = tree.getAsJsonArray();
        if (aeArray.size() == 0)
            throw new RuntimeException("Não existem emissões ativas!");
        return emissionsToString(aeArray, true);
    }


    /**
     * Envia pedido ao diretório para o histórico de leilões para uma determinada empresa
     * e retorna a representação textual deste.
     *
     * @param company           Empresa para a qual se deseja saber o histórico.
     * @return                  Representação textual do histórico de leilões para uma
     *                          determinada empresa.
     * @throws Exception
     */
    public static String getCompanyAuctionHistory(String company) throws Exception {
        JsonElement tree = getJsonTree("http://localhost:8080/companies/" + company + "/auctionHistory");
        JsonArray cahArray = tree.getAsJsonArray();
        if (cahArray.size() == 0)
            throw new RuntimeException("O histórico de leilões da empresa encontra-se vazio!");
        return auctionsToString(cahArray,false);
    }


    /**
     * Envia pedido ao diretório para o histórico de emissões para uma determinada empresa
     * e retorna a representação textual deste.
     *
     * @param company           Empresa para a qual se deseja saber o histórico.
     * @return                  Representação textual do histórico de emissões para uma
     *                          determinada empresa.
     * @throws Exception
     */
    public static String getCompanyEmissionHistory(String company) throws Exception {
        JsonElement tree = getJsonTree("http://localhost:8080/companies/" + company + "/emissionHistory");
        JsonArray cehArray = tree.getAsJsonArray();
        if (cehArray.size() == 0)
            throw new RuntimeException("O histórico de emissões da empresa encontra-se vazio!");
        return emissionsToString(cehArray, false);
    }


    /**
     * Envia pedido ao diretório para a lista de subscrições de um determinado investidor e retorna
     * uma lista com essas subscrições.
     *
     * @param investor          Investidor para o qual se pretende conhecer lista de subscrições.
     * @return                  Lista de subscrições de um subscritor.
     * @throws Exception
     */
    public static ArrayList<String> getSubscriptions(String investor) throws Exception {
        JsonElement tree = getJsonTree("http://localhost:8080/subscriptions/" + investor);
        JsonArray subArray = tree.getAsJsonArray();
        ArrayList<String> subs = new ArrayList<>();
        for(JsonElement e : subArray) {
            subs.add(e.toString());
        }

        return subs;
    }


    /**
     * Publica uma subscrição de um investidor para o diretório.
     *
     * @param investor          Investidor para o qual publicar a subscrição.
     * @param subscription      Subscrição a publicar.
     * @return                  Se foi publicada com sucesso (não excede limite de subscrições).
     * @throws Exception
     */
    public static boolean postSubscription(String investor, String subscription) throws Exception {
        ArrayList<String> subs = getSubscriptions(investor);
        if(subs.size() == 5) return false;

        String uri = "http://localhost:8080/subscriptions/" + investor + "/" +  subscription;

        postHttp(uri);

        return true;
    }


    /**
     * Apaga uma subscrição de um investidor do diretório.
     *
     * @param investor          Investidor para o qual se pretende apagar uma subscrição.
     * @param subscription      Subscrição a apagar.
     * @throws Exception
     */
    public static void deleteSubscription(String investor, String subscription) throws Exception {

        String uri = "http://localhost:8080/subscriptions/" + investor + "/" +  subscription;

        deleteHttp(uri);
    }
}