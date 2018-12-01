package utils;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import org.apache.commons.lang3.StringUtils;

public class Menu {
    private String titulo;
    private List<String> opcoes;
    private int op;

    public Menu(String titulo) {
        this.titulo = titulo;
        this.opcoes = new ArrayList<String>();
        this.op = 0;
    }

    public void executa() {
        do {
            printMenu();
            this.op = lerOpcao();
        }
        while(this.op == -1);
    }

    public void adiciona(String opcao) {
        opcoes.add(opcao);
    }

    private void printMenu() {
        int w = 79;
        System.out.println(StringUtils.rightPad("+", w - 1, "-") + "+");
        System.out.println(StringUtils.center(StringUtils.center(this.titulo, w - 2), w, "|"));
        System.out.println(StringUtils.rightPad("+", w - 1, "-") + "+");

        for(int i = 0; i < this.opcoes.size(); i++) {
            String opcao = i+1 + " - " + this.opcoes.get(i);
            System.out.println(StringUtils.rightPad("| " +  opcao, w-1) + "|");
        }

        System.out.println(StringUtils.rightPad("| 0 - Sair",  w-1) + "|");
        System.out.println(StringUtils.rightPad("+", w - 1, "-") + "+");

    }

    private int lerOpcao() {
        int op;
        Scanner is = new Scanner(System.in);

        System.out.print("Opção: ");

        try {
            op = is.nextInt();
        }
        catch(InputMismatchException e) {
            op = -1;
        }
        if(op < 0 || op > this.opcoes.size()) {
            System.out.println("Opção inválida!");
            op = -1;
        }
        return op;
    }

    public int getOpcao() {
        return this.op;
    }
}
