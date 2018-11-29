import org.zeromq.ZMQ;

public class Main {

    static EmpresasProtos.Empresa createEmpresa(int id, String nome) {
        return EmpresasProtos.Empresa.newBuilder()
                                     .setId(id)
                                     .setNome(nome)
                                     .build();
    }

    public static void main(String[] args) {
        EmpresasProtos.Empresa e1 = createEmpresa(1, "empresa1");
        EmpresasProtos.Empresa e2 = createEmpresa(2, "empresa2");
        EmpresasProtos.Empresa e3 = createEmpresa(3, "empresa3");
        EmpresasProtos.EmpresasRep ep = EmpresasProtos.EmpresasRep.newBuilder()
                                                      .addEmpresa(e1)
                                                      .addEmpresa(e2)
                                                      .addEmpresa(e3)
                                                      .build();

        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.SUB);

        socket.connect("tcp://localhost" + args[0]);

        socket.subscribe("".getBytes());

        while(true) {
            byte[] b = socket.recv();
            String req = new String(b);
            if(req.equals("EmpresasReq")) {
                System.out.println("Received request!");
            }

        }

    }
}
