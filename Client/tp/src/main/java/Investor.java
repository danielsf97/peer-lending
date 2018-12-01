import java.io.IOException;
import java.nio.channels.SocketChannel;

public class Investor {

    public static void bid_on_auction(Object[] res, SocketChannel socket) {
        try{
            String company = (String) res[0];
            float value = Float.parseFloat(res[1].toString());
            float rate = Float.parseFloat(res[2].toString());

            Protos.InvestorActionReq msg = create_auction_req(company, value, rate);
            Utils.send_msg(msg.toByteArray(), socket);

            byte[] resp = Utils.recv_msg(socket);
            Protos.InvestorActionResp rep = Protos.InvestorActionResp.parseFrom(resp);

            switch(rep.getStatus()){
                case CONFIRMED:
                    System.out.println("Licitação Realizada com Sucesso!!");
                    break;
                case REPLACED:
                    System.out.println("Licitação anterior substituída!!");
                    break;
                case ENDED:
                    System.out.println("Leilão terminado!!");
                    break;
                case INVALID:
                    System.out.println("Valores inválidos!!");
                    break;
                default:
                    System.out.println("Erro!!");
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private static Protos.InvestorActionReq create_auction_req(String company, float value, float rate){
        return
                Protos.InvestorActionReq.newBuilder()
                        .setReqType(Protos.InvestorActionReq.RequestType.AUCTION)
                        .setCompany(company)
                        .setValue(value)
                        .setRate(rate)
                        .build();
    }
}
