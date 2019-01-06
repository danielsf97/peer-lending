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
        sb.append(StringUtils.center(StringUtils.center("Lista de Leilões Ativos", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();
            sb.append(StringUtils.rightPad("| MONTANTE TOTAL MÁXIMO: " + o.get("value"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| TAXA DE JURO MÁXIMA: " + o.get("maxRate"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DATA DE COMEÇO: " + o.get("startingDateTime"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DURAÇÃO: " + o.get("duration"), w-1) + "|").append("\n");
            if(active)
                sb.append(StringUtils.rightPad("| EMPRESA: " + o.get("companyName"), w-1) + "|").append("\n");
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
        sb.append(StringUtils.center(StringUtils.center("Lista de Emissões Ativas", w - 2), w, "|")).append("\n");
        sb.append(StringUtils.rightPad("+", w - 1, "-") + "+").append("\n");
        for(JsonElement e: jarray) {
            JsonObject o = e.getAsJsonObject();

            sb.append(StringUtils.rightPad("| MONTANTE TOTAL MÁXIMO: " + o.get("value"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| TAXA DE JURO FIXA: " + o.get("fixedRate"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DATA DE COMEÇO: " + o.get("startingDateTime"), w-1) + "|").append("\n");
            sb.append(StringUtils.rightPad("| DURAÇÃO: " + o.get("duration"), w-1) + "|").append("\n");
            if(active)
                sb.append(StringUtils.rightPad("| EMPRESA: " + o.get("companyName"), w-1) + "|").append("\n");
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
        JsonArray companies_array = tree.getAsJsonArray();
        if (companies_array.size() == 0)
            throw new RuntimeException("Não existem empresas");
        return companiesToString(companies_array);

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
        JsonArray aa_array = tree.getAsJsonArray();
        if (aa_array.size() == 0)
            throw new RuntimeException("Não existem leilões ativos!");
        return auctionsToString(aa_array, true);
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
        JsonArray ae_array = tree.getAsJsonArray();
        if (ae_array.size() == 0)
            throw new RuntimeException("Não existem emissões ativas!");
        return emissionsToString(ae_array, true);
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
        JsonArray cah_array = tree.getAsJsonArray();
        if (cah_array.size() == 0)
            throw new RuntimeException("O histórico de leilões da empresa encontra-se vazio!");
        return auctionsToString(cah_array,false);
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
        JsonArray ceh_array = tree.getAsJsonArray();
        if (ceh_array.size() == 0)
            throw new RuntimeException("O histórico de emissões da empresa encontra-se vazio!");
        return emissionsToString(ceh_array, false);
    }
}