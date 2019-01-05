package core;

import representations.ActiveAuction;
import representations.ActiveEmission;
import representations.Auction;
import representations.Emission;

import java.util.ArrayList;
import java.util.List;

/**
 * Representa o histórico de leilões e emissões para uma determinada
 * empresa.
 *
 */
public class History {
    private List<Auction> auctions;
    private List<Emission> emissions;


    /**
     * Construtor vazio de um histórico.
     *
     */
    public History() {
        this.auctions = new ArrayList<>();
        this.emissions = new ArrayList<>();
    }

    /**
     * Obtém os leilões de um histórico.
     *
     * @return os leilões de um histórico.
     *
     */
    public List<Auction> getAuctions() {
        return auctions;
    }


    /**
     * Obtém as emissões de um histórico.
     *
     * @return as emissões de um histórico.
     *
     */
    public List<Emission> getEmissions() {
        return emissions;
    }

    /**
     * Adiciona um leilão previamente ativo ao histórico.
     *
     * @param aa Leilão a adicionar ao histórico.
     */
    public void addAuction(ActiveAuction aa) {
        Auction a = new Auction(aa.getValue(), aa.getMaxRate(), aa.getStartingDateTime(), aa.getDuration());
        auctions.add(a);
    }


    /**
     * Adiciona uma emissão previamente ativa ao histórico.
     *
     * @param ee Emissão a adicionar ao histórico.
     */
    public void addEmission(ActiveEmission ee) {
        Emission e = new Emission(ee.getValue(), ee.getFixedRate(), ee.getStartingDateTime(), ee.getDuration());
        emissions.add(e);
    }
}
