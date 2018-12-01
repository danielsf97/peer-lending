import java.io.IOException;
import java.nio.channels.SocketChannel;

public class Company {

    public static void create_auction(float[] res, SocketChannel socket) {
        try{
            Protos.AuctionReq msg = create_auction_msg(res[0], res[1]);
            Utils.send_msg(msg.toByteArray(), socket);

            byte[] resp = Utils.recv_msg(socket);
            Protos.AuctionResp rep = Protos.AuctionResp.parseFrom(resp);

            if(rep.getStatus() == Protos.AuctionResp.Status.ONGOING_AUCTION){
                System.out.println("Operação não permitida!! Já possui um leilão a decorrer.");
            }else{
                System.out.println("Leilão Criado!!");
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public static void create_emission(float[] res, SocketChannel socket) {
    }

    private static Protos.AuctionReq create_auction_msg(float value, float max_rate) {
        return
                Protos.AuctionReq.newBuilder()
                        .setValue(value)
                        .setMaxRate(max_rate)
                        .build();
    }
}
