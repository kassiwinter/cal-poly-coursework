import java.io.*;
import java.lang.*;
import java.util.*;
import java.util.HashMap;

public class Instructions {
    /* Getting Registers */
    public static Registers reg = new Registers();

    /* HashMap Objects for the R instruction type */
    static HashMap<String, String> R_Opcodes = new HashMap<String, String>() {{
    put("and", "000000");
    put("or", "001000");
    put("add", "000000");
    put("sll", "000000");
    put("sub", "000000");
    put("slt", "000000");
    put("jr", "000000");
    }};

    static HashMap<String, String> R_Functions = new HashMap<String, String>() {{
    put("and", "100100");
    put("or", "100101");
    put("add", "100000");
    put("sll", "000000");
    put("sub", "100010");
    put("slt", "101010");
    put("jr", "001000");
    }};

    public static String convert_R_Instructions(String name, String[] instructions){
        // NOTE: array should be sent in as followed: operation, rd, rs, rt
        HashMap<String, String> registers = reg.getRegisters();
        String opcode = R_Opcodes.get(name);
        String funct = R_Functions.get(name);
        String rd = registers.get(instructions[0]);
        String rs = registers.get(instructions[1]);
        String rt = registers.get(instructions[2]);
        String shamt = "00000";
        String formattedInstruction = opcode + rs + rt + rd + shamt + funct;
        return formattedInstruction;
    }

    public static String[] get_R_Layout(String line){
        String[] instructions = new String[3];
        String substring = "";
        int index = 0;

        for (int i = 0; i < line.length(); i++) {
            if (line.charAt(i) == ',') {
                instructions[index] = substring;
                index ++;
                substring = "";
            }
            else {
                substring += line.charAt(i);
            }
        }
        instructions[index] = substring;
        return instructions;
    }



     /* HashMap Objects for the I instruction type */
    static HashMap<String, String> I_Opcodes = new HashMap<String, String>() {{
    put("addi", "001000");
    put("beq", "000100");
    put("bne", "000101");
    put("lw", "100011");
    put("sw", "101011");
    }};

    public static String convert_I_Instructions(String name, String[] instructions){
         // NOTE: array should be sent in as followed: operation, rt, rs, immediate
        HashMap<String, String> registers = reg.getRegisters();
        String opcode = I_Opcodes.get(name);
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
        String formattedInstruction = opcode + rs + rt + imm;
        return formattedInstruction;

    }

    public static String[] get_I_Layout(String line) {
        String[] instructions = new String[3];
        String substring = "";
        int index = 0;

        for (int i = 0; i < line.length(); i++) {
            if (line.charAt(i) == ',' | line.charAt(i) == '(' | line.charAt(i) == ')') {
                instructions[index] = substring;
                index ++;
                substring = "";
            }
            else {
                substring += line.charAt(i);
            }
        }
        instructions[index] = substring;
        return instructions;
    }



    /* HashMap Objects for the J instruction type */
    static HashMap<String, String> J_Opcodes = new HashMap<String, String>(){{
    put("j", "000010");
    put("jal", "000011");
    }};

    public static String convert_J_Instructions(String name, String address){
        // NOTE: array should be sent in as followed: operation, immediate (as address)
        HashMap<String, String> registers = reg.getRegisters();
        String opcode = J_Opcodes.get(name);
	int temp = Integer.parseInt(address);
        String imm = Integer.toBinaryString(temp);
        imm = String.format("%32s", imm).replaceAll(" ", "0");
        String formattedInstruction = opcode + imm;
        return formattedInstruction;
    }

    public static String get_J_Layout(String line) {
        String address = "";
        for (int i = 0; i < line.length(); i++) {
            if (line.charAt(i) == ',') {
                break;
            }
            else {
                address += line.charAt(i);
            }
        }
        return address;
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
            else {
                name += line.charAt(i);
            }
        }
        //Step 2. Create a substring that excludes name (just the instructions)
        int firstRegister = line.indexOf("$");
        String inst = line.substring(firstRegister);
	String[] instructions = inst.split("$");
        // Step 2. Find the instruction type
        if (R_Opcodes.containsKey(name)) {
            String[] R_instructions = new String[3];
            R_instructions = get_R_Layout(line);
            String R_binary = convert_R_Instructions(name, instructions);
            return R_binary;
        }

        if (I_Opcodes.containsKey(name)) {
            String[] I_instructions = new String[3];
            I_instructions = get_I_Layout(line);
            String I_binary = convert_I_Instructions(name, instructions);
            return I_binary;
        }

        if (J_Opcodes.containsKey(name)) {
            String[] J_instructions = new String[2];
            J_instructions = get_R_Layout(line);
            String J_binary = convert_J_Instructions(name, inst);
            return J_binary;
        }

        if (isValid(name) == false) {
            return "";
        }
        else return "";

    }




}
