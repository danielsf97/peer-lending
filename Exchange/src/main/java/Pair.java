

/**
 * Representa um tipo constituído por dois elementos.
 *
 * @param <T>       Primeiro elemento.
 * @param <U>       Segundo elemento.
 */
public class Pair<T, U> {
    private T first;
    private U second;

    /**
     * Construtor parametrizado.
     *
     * @param first     Primeiro elemento
     * @param second    Segundo elemento.
     */
    public Pair(T first, U second) {
        this.first = first;
        this.second  = second;
    }


    /**
     * Retorna primeiro elemento do par.
     *
     * @return primeiro elemento do par.
     */
    public T getFirst() {
        return first;
    }


    /**
     * Retorna segundo elemento do par.
     *
     * @return segundo elemento do par.
     */
    public U getSecond() {
        return second;
    }


    /**
     * Estabelece primeiro elemento do par.
     *
     * @param first Segundo primeiro do par.
     */
    public void setFirst(T first) {
        this.first = first;
    }

    /**
     * Estabelece segundo elemento do par.
     *
     * @param second Segundo elemento do par.
     */
    public void setSecond(U second) {
        this.second = second;
    }
}

