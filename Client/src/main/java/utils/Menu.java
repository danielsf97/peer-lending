package utils;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import org.apache.commons.lang3.StringUtils;

public class Menu {
    private String title;
    private List<String> options;
    private int nOptions;
    private int op;
    private Scanner sc;

    public Menu(String title) {
        this.title = title;
        this.options = new ArrayList<String>();
        this.nOptions = 0;
        this.op = 0;
        sc = new Scanner(System.in);
    }

    public void execute() {
        do {
            printMenu();
            if(!options.isEmpty())
                this.op = readOption();
        }
        while(this.op == -1);
    }

    public void add(String option) {
        options.add(option);
        nOptions++;
    }
    public void removeLast(int optionsNumber){
        while(optionsNumber > 0){
            nOptions--;
            options.remove(nOptions);
            optionsNumber--;
        }
    }

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

    public int getOption() {
        return this.op;
    }

    public int readInt(String msg){
        int num;

        System.out.print(msg);

        try {

            num = Integer.parseInt(sc.next());

        } catch (NumberFormatException e) {
            System.out.println("O valor não é válido!!\n");
            num = readInt(msg);
        }

        return num;
    }

    public float readFloat(String msg){
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

    public String readString(String msg){
        System.out.print(msg);
        return sc.next();
    }

    public long readLong(String msg) {
        long num;

        System.out.print(msg);

        try {

            num = Long.parseLong(sc.next());

        } catch (NumberFormatException e) {
            System.out.println("O valor não é válido!!\n");
            num = readLong(msg);
        }

        return num;
    }
}
