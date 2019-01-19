import org.zeromq.ZMQ;

import java.util.ArrayList;


/**
 * Representa a tarefa a ser executada no fim de um leilão/emissão.
 *
 */
public class ScheduledExecutor implements Runnable {
    private Company company;
    private ZMQ.Socket push;
    private ZMQ.Socket pub;
    private DirectoryManager dir;


    /**
     * Construtor parametrizado.
     *
     * @param company   Empresa responsável pelo leilão/emissão.
     * @param push      Socket push.
     * @param pub       Socket pub.
     * @param dir       Gestor do diretório.
     */
    ScheduledExecutor(Company company, ZMQ.Socket push, ZMQ.Socket pub, DirectoryManager dir) {
        this.company = company;
        this.push = push;
        this.pub = pub;
        this.dir = dir;
    }


    /**
     * Corre a tarefa.
     *
     */
    @Override
    public void run() {
        Auction auction;
        Emission emission;

        try {
            if((auction = company.getActiveAuction()) != null) {
                finishAuction(auction);
                company.setActiveAuction(null);
            }
            else {
                emission = company.getActiveEmission();
                finishEmission(emission);
                company.setActiveEmission(null);
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }


    /**
     * Responsável por acabar um leilão. Calcula a lista de vencedores/perdedores
     * e envia mensagem do resultado a todos os envolvidos.
     *
     * @param auction           Leilão a acabar.
     * @throws Exception
     */
    private void finishAuction(Auction auction) throws Exception {
        Pair<ArrayList<Pair<String,Long>>, ArrayList<String>> winnersAndLosers = auction.getWinnersLosers();

        ArrayList<Pair<String,Long>> winners = winnersAndLosers.getFirst();
        ArrayList<String> losers = winnersAndLosers.getSecond();

        // Enviar mensagem aos vencedores
        if(winners != null && !winners.isEmpty()) {
            for(Pair winnerVal : winners) {
                Protos.MessageWrapper msg = createAuctionWinningResultMsg(winnerVal);
                push.send(msg.toByteArray());
            }
            float max_rate = auction.getMaxBidRate(winners.size());
            this.company.setEmissionRate(max_rate);
        }

        // Enviar mensagem aos perdedores
        if(losers != null && !losers.isEmpty()) {
            for(String investor : losers) {
                Protos.MessageWrapper msg = createAuctionLoserResultMsg(investor);
                push.send(msg.toByteArray());
            }
        }

        // Enviar mensagem à empresa
        Protos.MessageWrapper msgEmpresa;
        String notification;
        if(winners != null) {
            String status = "Sucesso";
            msgEmpresa = createAuctionCompanyResultMsg(status);

            notification = this.company.getName() + ": Leilão Terminado, status: Sucesso, montante: " + auction.getValue();
        }
        else {
            String status = "Insucesso";
            msgEmpresa = createAuctionCompanyResultMsg(status);

            notification = this.company.getName() + ": Leilão Terminado, status: Insucesso, montante: " + auction.getValue();
        }

        push.send(msgEmpresa.toByteArray());
        pub.send(notification);

        if(winners != null)
            dir.deleteAuction(this.company.getName(), "SUCCESSFUL");
        else
            dir.deleteAuction(this.company.getName(), "NON-SUCCESSFUL");
    }


    /**
     * Cria mensagem para os vencedores de um leilão.
     * @param winnerVal Par cujo primeiro elemento é o investidor vencedor e segundo elemento o montante do leilão.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createAuctionWinningResultMsg(Pair winnerVal) {
        String client = (String) winnerVal.getFirst();
        Long value = (Long) winnerVal.getSecond();
        String msg = "Leilão terminado! Empresa: " + company.getName() + ", Status: Vencedor, Montante: " + value;

        return createAuctionEmissionResult(client, msg);
    }

    /**
     * Cria mensagem para os perdedores de um leilão.
     *
     * @param investor  Investidor perdedor.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createAuctionLoserResultMsg(String investor) {
        String msg = "Leilão terminado! Empresa: " + company.getName() + ", Status: Perdedor";

        return createAuctionEmissionResult(investor, msg);
    }

    /**
     * Cria mensagem com informação acerca de sucesso/insucesso do leilão para a empresa.
     *
     * @param status    Resultado do leilão.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createAuctionCompanyResultMsg(String status) {
        String msg = "Leilão terminado! Status: " + status;
        String client = company.getName();

        return createAuctionEmissionResult(client, msg);

    }

    //-----------------------------------------------------------------------------------------------------------------------------


    /**
     * Responsável por acabar uma emissão. Calcula a lista de vencedores/perdedores
     * e envia mensagem do resultado a todos os envolvidos.
     *
     * @param emission           Emissão a acabar.
     * @throws Exception
     */
    private void finishEmission(Emission emission) throws Exception {
        Pair<ArrayList<Pair<String,Long>>,ArrayList<String>> winnersAndLosers = emission.getWinnersLosers();

        ArrayList<Pair<String,Long>> winners = winnersAndLosers.getFirst();
        ArrayList<String> losers = winnersAndLosers.getSecond();

        long sum = 0;

        // Enviar mensagem aos vencedores
        if(winners != null) {
            for(Pair winnerVal : winners) {
                sum += (long) winnerVal.getSecond();
                Protos.MessageWrapper msg = createEmissionWinningResultMsg(winnerVal);
                push.send(msg.toByteArray());
            }
        }

        // Enviar mensagem aos perdedores
        if(losers != null) {
            for(String investor : losers) {
                Protos.MessageWrapper msg = createEmissionLoserResultMsg(investor);
                push.send(msg.toByteArray());
            }
        }

        // Enviar mensagem à empresa
        Protos.MessageWrapper msgEmpresa;
        String notification;

        if(sum == emission.getValue()) {
            String status = "Total";
            msgEmpresa = createEmissionCompanyResultMsg(status, emission.getValue(), sum);

            notification = this.company.getName() + ": Emissão Terminada, subscrição: Total, requerido: " +
                    emission.getValue() + ", obtido: " + sum;

        }
        else {
            if(sum == 0) {
                String status = "Nula";
                msgEmpresa = createEmissionCompanyResultMsg(status, emission.getValue(), sum);

                notification = this.company.getName() + ": Emissão Terminada, subscrição: Nula, requerido: " +
                        emission.getValue() + ", obtido: " + sum;
            }
            else {
                String status = "Parcial";
                msgEmpresa = createEmissionCompanyResultMsg(status, emission.getValue(), sum);

                notification = this.company.getName() + ": Emissão Terminada, subscrição: Parcial, requerido: " +
                        emission.getValue() + ", obtido: " + sum;
            }
            float nextEmissionRate = ((int) (((this.company.getEmissionRate() * (float) 1.1) + 0.005f) * 100)) / 100f;
            this.company.setEmissionRate(nextEmissionRate);
        }
        push.send(msgEmpresa.toByteArray());
        pub.send(notification);

        if(winners != null)
            dir.deleteEmission(this.company.getName(), "SUCCESSFUL");
        else
            dir.deleteEmission(this.company.getName(), "NON-SUCCESSFUL");
    }


    /**
     * Cria mensagem para os vencedores de uma emissão.
     *
     * @param winnerVal Par cujo primeiro elemento é o investidor vencedor e segundo elemento o montante da emissão.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createEmissionWinningResultMsg(Pair winnerVal) {
        String client = (String) winnerVal.getFirst();
        Long value = (Long) winnerVal.getSecond();
        String msg = "Emissão terminada! Empresa: " + company.getName() + ", Status: Vencedor, Montante: " + value;

        return createAuctionEmissionResult(client, msg);
    }


    /**
     * Cria mensagem para os perdedores de uma emissão.
     *
     * @param investor  Investidor perdedor.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createEmissionLoserResultMsg(String investor) {
        String msg = "Emissão terminada! Empresa: " + company.getName() + ", Status: Perdedor";

        return createAuctionEmissionResult(investor, msg);
    }


    /**
     * Cria mensagem com informação acerca de sucesso/insucesso da emissão para a empresa.
     *
     * @param status    Resultado da emissão.
     * @return          Mensagem criada.
     */
    private Protos.MessageWrapper createEmissionCompanyResultMsg(String status, long value, long sum) {
        String msg = "Emissão terminada! Subscrição: " + status + ", Esperado: " + value + ", Obtido: " + sum;
        String client = company.getName();

        return createAuctionEmissionResult(client, msg);
    }

    //-----------------------------------------------------------------------------------------------------------------------------


    /**
     * Cria mensagem para o envio aos envolvidos numa emissão/leilão.
     *
     * @param client        Cliente envolvido.
     * @param msg           Mensagem a ser enviada.
     * @return              Mensagem criada.
     */
    private Protos.MessageWrapper createAuctionEmissionResult(String client, String msg) {
        Protos.AuctionEmissionResult result = Protos.AuctionEmissionResult.newBuilder()
                .setClient(client)
                .setMsg(msg)
                .build();

        return Protos.MessageWrapper.newBuilder()
                .setMsgType(Protos.MessageWrapper.MessageType.ASYNC)
                .setAuctionemissionresult(result)
                .build();
    }


}
