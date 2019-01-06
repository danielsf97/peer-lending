/**
 * Representa uma empresa.
 *
 */
public class Company {
    private String name;
    private Auction activeAuction;
    private Emission activeEmission;
    private float emissionRate;

    /**
     * Construtor parametrizado.
     *
     * @param name Nome da empresa.
     */
    public Company(String name) {
        this.activeAuction = null;
        this.activeEmission = null;
        this.name = name;
        this.emissionRate = -1;
    }


    /**
     * Estabelece o leilão ativo de uma empresa.
     *
     * @param a             Novo leilão ativo da empresa.
     * @throws Exception    Caso já exista leilão/emissão ativo para a empresa.
     */
    public synchronized void setActiveAuction(Auction a) throws Exception {
        if((activeAuction == null && activeEmission == null) || a == null)
            this.activeAuction = a;
        else {
            throw new Exception();
        }
    }


    /**
     * Estabelece a emissão ativa de uma empresa.
     *
     * @param e             Nova emissão ativa da empresa.
     * @throws Exception    Caso já exista leilão/emissão ativo para a empresa.
     */
    public synchronized void setActiveEmission(Emission e) throws Exception {
        if((activeAuction == null && activeEmission == null) || e == null)
            this.activeEmission = e;
        else throw new Exception();
    }


    /**
     * Retorna o leilão ativo da empresa.
     *
     * @return o leilão ativo da empresa.
     */
    public synchronized Auction getActiveAuction() {
        return activeAuction;
    }


    /**
     * Retorna a emissão ativa da empresa.
     *
     * @return a emissão ativa da empresa.
     */
    public synchronized Emission getActiveEmission() {
        return activeEmission;
    }


    /**
     * Retorna o nome da empresa.
     *
     * @return o nome da empresa.
     */
    public String getName() {
        return this.name;
    }


    /**
     * Retorna a taxa de emissão da emissão ativa.
     *
     * @return a taxa de emissão da emissão ativa.
     */
    public float getEmissionRate() {
        return emissionRate;
    }


    /**
     * Estabelece a taxa de emissão da emissão ativa.
     *
     * @param emissionRate  Taxa de emissão da emissão ativa.
     */
    public void setEmissionRate(float emissionRate) {
        this.emissionRate = emissionRate;
    }


    /**
     * Verifica se uma empresa tem algum leilão ou emissão ativos.
     *
     * @return true se empresa tiver leilão/emissão ativos ou
     *         false caso contrário.
     */
    public synchronized boolean hasActiveAction() {
        return (activeAuction != null || activeEmission != null);
    }
}
