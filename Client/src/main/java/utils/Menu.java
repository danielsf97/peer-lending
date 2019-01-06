package utils;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import org.apache.commons.lang3.StringUtils;


/**
 * Classe para criação de um menu para interface de texto.
 *
 */
public class Menu {
    private String title;
    private List<String> options;
    private int nOptions;
    private int op;
    private Scanner sc;

    /**
     * Construtor parametrizado.
     *
     * @param title  Título do menu.
     */
    public Menu(String title) {
        this.title = title;
        this.options = new ArrayList<>();
        this.nOptions = 0;
        this.op = 0;
        this.sc = new Scanner(System.in);
    }


    /**
     * Executa o menu, fazendo display deste e lendo a opção dada
     * pelo cliente.
     *
     */
    public void execute() {
        do {
            printMenu();
            if(!options.isEmpty())
                this.op = readOption();
        }
        while(this.op == -1);
    }


    /**
     * Adiciona uma opção ao menu.
     *
     * @param option opção a adicionar.
     */
    public void add(String option) {
        options.add(option);
        nOptions++;
    }


    /**
     * Remove as últimas n opções.
     *
     * @param optionsNumber Número de opções a eliminar.
     */
    public void removeLast(int optionsNumber) {
        while(optionsNumber > 0) {
            nOptions--;
            options.remove(nOptions);
            optionsNumber--;
        }
    }


    /**
     * Faz display de um menu.
     *
     */
    private void printMenu() {
        int w = 79;
        System.out.println(StringUtils.rightPad("+", w - 1, "-") + "+");
        System.out.println(StringUtils.center(StringUtils.center(this.title, w - 2), w, "|"));
        System.out.println(StringUtils.rightPad("+", w - 1, "-") + "+");

        for(int i = 0; i < this.options.size(); i++) {
            String option = i+1 + " - " + this.options.get(i);
            System.out.println(StringUtils.rightPad("| " +  option, w-1) + "|");
        }

        if(!options.isEmpty()) {
            System.out.println(StringUtils.rightPad("| 0 - Sair", w - 1) + "|");
            System.out.println(StringUtils.rightPad("+", w - 1, "-") + "+");
        }

    }


    /**
     * Lê a opção dada pelo utilizador.
     *
     * @return opção escolhida pelo utilizador.
     */
    private int readOption() {
        int op;
        Scanner is = new Scanner(System.in);

        System.out.print("Opção: ");

        try {
            op = is.nextInt();
        }
        catch(InputMismatchException e) {
            op = -1;
        }
        if(op < 0 || op > this.options.size()) {
            System.out.println("Opção inválida!");
            op = -1;
        }
        return op;
    }


    /**
     * Retorna opção escolhida pelo utilizador.
     *
     * @return Opção escolhida pelo utilizador.
     */
    public int getOption() {
        return this.op;
    }


    /**
     * Lê um inteiro dado pelo utilizador.
     *
     * @param msg   Mensagem de pedido.
     * @return      Inteiro lido.
     */
    public int readInt(String msg) {
        int num;

        System.out.print(msg);

        try {
            num = Integer.parseInt(sc.next());

        } catch (NumberFormatException e) {
            System.out.println("O valor não é válido!\n");
            num = readInt(msg);
        }

        return num;
    }


    /**
     * Lê um número floating point dado pelo utilizador.
     *
     * @param msg   Mensagem de pedido.
     * @return      Número lido.
     */
    public float readFloat(String msg) {
        float num;

        System.out.print(msg);

        try {

            num = Float.parseFloat(sc.next());

        } catch (NumberFormatException e) {
            System.out.println("O valor não é válido!!\n");
            num = readFloat(msg);
        }

        return num;
    }


    /**
     * Lê uma string dada pelo utilizador.
     *
     * @param msg   Mensagem de pedido.
     * @return      String lida.
     */
    public String readString(String msg) {
        System.out.print(msg);
        return sc.next();
    }


    /**
     * Lê um long dado pelo utilizador.
     *
     * @param msg   Mensagem de pedido.
     * @return      Long lido.
     */
    public long readLong(String msg) {
        long num;

        System.out.print(msg);

        try {

            num = Long.parseLong(sc.next());

        } catch (NumberFormatException e) {
            System.out.println("O valor não é válido!\n");
            num = readLong(msg);
        }

        return num;
    }
}
