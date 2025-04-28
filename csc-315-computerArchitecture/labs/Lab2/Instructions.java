// Names: Kassi Winter and Anna Makarewicz
// Section: CSC 315 - 01

import java.io.*;
import java.lang.*;
import java.util.*;
import java.util.HashMap;

public class Instructions {
    /* Getting Registers */
    public static Registers reg = new Registers();

    /* HashMap Objects for the R instruction type */
    static HashMap<String, String> R_Opcodes = new HashMap<String, String>();
    static{
        R_Opcodes.put("and", "000000");
        R_Opcodes.put("or", "001000");
        R_Opcodes.put("add", "000000");
        R_Opcodes.put("sll", "000000");
        R_Opcodes.put("sub", "000000");
        R_Opcodes.put("slt", "000000");
        R_Opcodes.put("jr", "000000");
    }

    static HashMap<String, String> R_Functions = new HashMap<String, String>();
    static{
        R_Functions.put("and", "100100");
        R_Functions.put("or", "100101");
        R_Functions.put("add", "100000");
        R_Functions.put("sll", "000000");
        R_Functions.put("sub", "100010");
        R_Functions.put("slt", "101010");
        R_Functions.put("jr", "001000");
    }

    public static String convert_R_Instructions(String name, String[] instructions){
        // NOTE: array should be sent in as followed: operation, rd, rs, rt
        HashMap<String, String> registers = reg.getRegisters();
        String opcode = R_Opcodes.get(name);
        String funct = R_Functions.get(name);
        String rd = registers.get(instructions[0]);
        String rs = registers.get(instructions[1]);
        String rt = registers.get(instructions[2]);
        String shamt = "00000";
        String formattedInstruction = opcode + " " + rs + " " + rt + " " + rd + " " + shamt + " " + funct;
        return formattedInstruction;
    }

    public static String[] get_R_Layout(String line){
        String[] instructions = new String[3];
        String substring = "";
        int index = 0;

        for (int i = 0; i < line.length(); i++) {
            if ((line.charAt(i) == ',')) {
                    instructions[index] = substring;
                    index ++;
                    substring = "";
                    
                }
            else {
                substring += line.charAt(i);
            }
        }
    
        instructions[index] =  substring;
        return instructions;
    }



     /* HashMap Objects for the I instruction type */
    static HashMap<String, String> I_Opcodes = new HashMap<String, String>();
    static{
        I_Opcodes.put("addi", "001000");
        I_Opcodes.put("beq", "000100");
        I_Opcodes.put("bne", "000101");
        I_Opcodes.put("lw", "100011");
        I_Opcodes.put("sw", "101011");
    }

    public static String convert_I_Instructions(String name, String[] instructions){
         // NOTE: array should be sent in as followed: operation, rt, rs, immediate
        HashMap<String, String> registers = reg.getRegisters();
        String opcode = I_Opcodes.get(name);
        if((name.equals("sw")) || (name.equals("lw"))) {
            String rt = registers.get(instructions[0]);
            String rs = registers.get(instructions[2]);
            int temp = Integer.parseInt(instructions[1]);
            String imm = Integer.toBinaryString(temp);
            char firstDigit = imm.charAt(0);
            if (firstDigit == '1'){
                imm = String.format("%16s", imm).replaceAll(" ", "1");
            }
            else {
                imm = String.format("%16s", imm).replaceAll(" ", "0");
            }
            String formattedInstruction = opcode + " " + rs + " " + rt + " " + imm;
            return formattedInstruction;
        }
        else {
            String rt = registers.get(instructions[0]);
            String rs = registers.get(instructions[1]);
	        int temp = Integer.parseInt(instructions[2]);
            String imm = Integer.toBinaryString(temp);
            char firstDigit = imm.charAt(0);
            if (firstDigit == '1'){
                imm = String.format("%16s", imm).replaceAll(" ", "1");
            }
            else {
                imm = String.format("%16s", imm).replaceAll(" ", "0");
            }
            String formattedInstruction = opcode + " " + rs + " " + rt + " " + imm;
            return formattedInstruction;
        }

    }

    public static String[] get_I_Layout(String line) {
        String[] instructions = new String[3];
        String sub = "";
        int index = 0;

        for (int i = 0; i < line.length(); i++) {
            if (line.charAt(i) == ',') {
                if(index != 0) {
                    sub = sub.substring(1);
                }
                instructions[index] = sub.trim();
                index ++;
                sub = "";
            }
            if (line.charAt(i) == '(') {
                sub = sub.substring(1);
                instructions[index] = sub.trim();
                index ++;
                sub = "";
            }
            if (line.charAt(i) == ')') {
                sub = sub.substring(1);
                instructions[index] = sub.trim();
                index ++;
                sub = "";
            }
            else {
                sub += line.charAt(i);
            }
        }
        if (instructions[2] == null) {
            sub = sub.substring(1);
            instructions[index] =  sub.trim();
        }
        return instructions;
    }



    /* HashMap Objects for the J instruction type */
    static HashMap<String, String> J_Opcodes = new HashMap<String, String>();
    static {
        J_Opcodes.put("j", "000010");
        J_Opcodes.put("jal", "000011");
    }

    public static String convert_J_Instructions(String name, String address){
        // NOTE: array should be sent in as followed: operation, immediate (as address)
        String opcode = J_Opcodes.get(name);
	    int temp = Integer.parseInt(address);
        String imm = Integer.toBinaryString(temp);
        imm = String.format("%26s", imm).replaceAll(" ", "0");
        String formattedInstruction = opcode + " " + imm;
        return formattedInstruction;
    }


/* #1 Checks to see if a name is valid; returns T if valid, F if not */
    public static boolean isValid(String name) {
        if (R_Opcodes.containsKey(name)) {
            return true; // if it is a R Instruction
        }
         if (I_Opcodes.containsKey(name)) {
            return true; // if it is a I Instruction
        }
         if (J_Opcodes.containsKey(name)) {
            return true; // if it is a J Instruction
        }
        return false; // if it is a random word (not an instruction)

    }

    /* #2 Takes in a line and gets the binary */
    public static String convertLine(String line) {
        // Step 1. Gets name of instruction
        String name = "";
        for (int i = 0; i < line.length(); i++) {
            if (line.charAt(i) == '$') {
                break;
            }
            
            if (Character.isDigit(line.charAt(i))) {
                break;
            }
            
            else {
                name += line.charAt(i);
            }
        }

        //Step 2. Create a substring that excludes name (just the instructions)
	    String inst = (line.substring(name.length())).trim();

	    // Step 3. Find the instruction type
        if (R_Opcodes.containsKey(name.trim())) {
            String[] R_instructions = new String[3];
            R_instructions = get_R_Layout(inst);
            String R_binary = convert_R_Instructions(name.trim(), R_instructions);
            return R_binary;
        }

        if (I_Opcodes.containsKey(name.trim())) {
            String[] I_instructions = new String[3];
            I_instructions = get_I_Layout(inst);
            String I_binary = convert_I_Instructions(name.trim(), I_instructions);
            return I_binary;
        }

        if (J_Opcodes.containsKey(name.trim())) {
            String J_binary = convert_J_Instructions(name.trim(), inst);
            return J_binary;
        }

        if (isValid(name.trim()) == false) {
            return ("invalid instruction: " + name.trim());
        }
        return ("invalid instruction: " + name.trim());

    }




}
