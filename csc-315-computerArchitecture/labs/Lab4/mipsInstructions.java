// Names: Kassi Winter and Anna Makarewicz
// Section: CSC 315 - 01
/* Description:  Contains MIPS instructions as well as
functions that minipulate them. */

import java.io.*;
import java.lang.*;
import java.util.*;
import java.util.HashMap;

public class mipsInstructions {
    /*--- Getting Registers ---*/
    public static Registers reg = new Registers();

    /*--- HashMap Objects for the R instruction type ---*/
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


    /*--- R Layout ---*/
    public static String[] get_R_Layout(String name, String line){
        String[] instructions = new String[4];
        String sub = "";
        int index = 0;

        // append the name
        instructions[index] = name;
        index++;
        // append the rest of the instructions
        for (int i = 0; i < line.length(); i++) {
            if ((line.charAt(i) == ',')) {
                sub = sub.substring(1);
                instructions[index] = sub;
                index ++;
                sub = "";  
                }
            else {
                sub += line.charAt(i);
            }
        }
        sub = sub.substring(1);
        instructions[index] =  sub.trim();
        return instructions;
    }



     /*--- HashMap Objects for the I instruction type ---*/ 
    static HashMap<String, String> I_Opcodes = new HashMap<String, String>();
    static{
        I_Opcodes.put("addi", "001000");
        I_Opcodes.put("beq", "000100");
        I_Opcodes.put("bne", "000101");
        I_Opcodes.put("lw", "100011");
        I_Opcodes.put("sw", "101011");
    }

    /*--- I Layout ---*/
    public static String[] get_I_Layout(String name, String line) {
        String[] instructions = new String[4];
        String sub = "";
        int index = 0;

        // append the name
        instructions[index] = name;
        index++;

        // append the rest of the instructions
        for (int i = 0; i < line.length(); i++) {
            if ((line.charAt(i) == ',')) {
                sub = sub.substring(1);
                instructions[index] = sub;
                index ++;
                sub = "";   
            }
            else if (line.charAt(i) == '(') {
                instructions[index] = sub.trim();
                index ++;
                sub = "";
            }
            else {
                sub += line.charAt(i);
            }
        }

        // is it a negative number
        if(sub.contains("-")) {
            instructions[index] = sub.trim();
        }

        // is it a label
        else if((Character.isLetter(sub.charAt(0)) == true)) {
            instructions[index] = sub.trim();
        }

        else {
            // is it sw or lw
            if(sub.contains(")")) {
                sub = sub.substring(0, sub.indexOf(')'));
            }
            if((Character.isDigit(sub.charAt(0)) == false)) {
                sub = sub.substring(1);;
            }
            instructions[index] = sub.trim();
        }
        return instructions;
    }



    /* HashMap Objects for the J instruction type */
    static HashMap<String, String> J_Opcodes = new HashMap<String, String>();
    static {
        J_Opcodes.put("j", "000010");
        J_Opcodes.put("jal", "000011");
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
    

    /* #2 Takes in a line and gets its components in an array */
    public static String[] convertLine(String line) {
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
            String[] R_instructions = new String[4];
            R_instructions = get_R_Layout(name.trim(), inst);
            return R_instructions;
        }

        if (I_Opcodes.containsKey(name.trim())) {
            String[] I_instructions = new String[4];
            I_instructions = get_I_Layout(name.trim(), inst);
            return I_instructions;
        }

        if (J_Opcodes.containsKey(name.trim())) {
            String instruct[] = new String[2];
            instruct[0] = name.trim();
            instruct[1] =  inst;
            return instruct;
        }

        if (isValid(name.trim()) == false) {
            System.out.println("invalid instruction: " + name.trim());
            String empty[] = new String[0];
            return empty;
        }
        System.out.println("invalid instruction: " + name.trim());
        String empty[] = new String[0];
        return empty;

    }

    /* #3 Run instruction with CPU */
    public static void runInstruction(CPU cpu, String instruction) {
		cpu.regs.put("mem/wb reg", cpu.regs.get("exe/mem reg"));
		cpu.regs.put("exe/mem reg", cpu.regs.get("id/exe reg"));
		cpu.regs.put("id/exe reg", cpu.regs.get("if/id reg"));
		cpu.regs.put("if/id reg", instruction);
	}

    /* #4 Do instructions */
    public static int processInstruction(mipsEmulator emulator, ArrayList<String> lineList, HashMap<String, Integer> labels, int programCounter, CPU cpu) {
        cpu.cycles++;
        // if an after load indicated...
		if (cpu.load) {
			if (cpu.loadRate == 1) {
				cpu.stall();
				cpu.load = false;
				return programCounter;
			} 
			else {
				cpu.loadRate++;	
			}
		}		
        // if a jump was indicated...
		if (cpu.jump){
			cpu.jumpRate++;
			if (cpu.jumpRate == 1) {
				runInstruction(cpu, "squash");
				cpu.jumpRate = 0;
				cpu.jump = false;
				return emulator.jumpDest;	
			}
		} 
        		// if a branch was indicated...
		if (cpu.branch) {
			cpu.branchRate++;
			if (cpu.branchRate == 3) {
				cpu.squash();
				return emulator.branchDest;
			}
		}


        String line = lineList.get(programCounter);
        String[] instruction = convertLine(line);
	    runInstruction(cpu, instruction[0]);

        if (!cpu.jump && !cpu.branch) {
		switch (instruction[0]){
            // R Instructions
            case("add"):
				emulator.setRegister(instruction[1], emulator.getValue(instruction[2]) + emulator.getValue(instruction[3]));
				programCounter++;
				break;

			case("and"):
				emulator.setRegister(instruction[1], emulator.getValue(instruction[2]) & emulator.getValue(instruction[3]));
				programCounter++;
				break;

			case("or"):
                emulator.setRegister(instruction[1], emulator.getValue(instruction[2]) | emulator.getValue(instruction[3]));
				programCounter++;
				break; 

            case("slt"):
				if(emulator.getValue(instruction[2]) >= emulator.getValue(instruction[3])) {
					emulator.setRegister(instruction[1], 0);
				} else {
					emulator.setRegister(instruction[1], 1);
				}
				programCounter++;
				break;
            
            case("sll"):
				emulator.setRegister(instruction[1], emulator.getValue(instruction[2]) << Integer.parseInt(instruction[3]));
				programCounter++;
				break;

			case("sub"):
				emulator.setRegister(instruction[1], emulator.getValue(instruction[2]) - emulator.getValue(instruction[3]));
				programCounter++;
				break;

            // I Instructions
			case("addi"):
				emulator.setRegister(instruction[1], emulator.getValue(instruction[2]) + Integer.parseInt(instruction[3]));
				programCounter++;
				break;

			case("beq"):
				if(emulator.getValue(instruction[1]) == emulator.getValue(instruction[2])) {
                    cpu.branch = true;
					programCounter = labels.get(instruction[3]);
				} else {
					programCounter++;
				}
				break;

			case("bne"):
				if(emulator.getValue(instruction[1]) != emulator.getValue(instruction[2])) {
                    cpu.branch= true;
					programCounter = labels.get(instruction[3]);
				} else {
					programCounter++;
				}
				break;

			case("lw"):
				emulator.setRegister(instruction[3], emulator.getMemory(emulator.getValue(instruction[2])+emulator.getValue(instruction[1])));
				if(lineList.get(programCounter+1).contains(instruction[1]) && !lineList.get(programCounter+1).contains("lw") && !lineList.get(programCounter+1).contains("sw")) {
					cpu.load = true;					
				}
                programCounter++;
				break;

			case("sw"):
				emulator.setMemory(emulator.getValue(instruction[3])+Integer.parseInt(instruction[2]), emulator.getValue(instruction[1]));		
				programCounter++;
				break;

            // J Instructions
			case("j"):
				programCounter = labels.get(instruction[1]);
				break;

            case("jal"):
				emulator.setRegister("$ra", programCounter+1);
				programCounter = labels.get(instruction[1]);
				emulator.initializeRegs();
				break;

			case("jr"):
				programCounter = emulator.getValue(instruction[1]);
				break;

            // DEFAULT
			default:
				System.out.println("invalid instruction");
		}
        }
        // if there is a jump/branch flag activated
	    else {
		    programCounter = programCounter + 1;
	    }
	    cpu.instructions++;
		if (programCounter == lineList.size()) {
			for (int i=0; i<4; i++) {
				cpu.cycles++;
				runInstruction(cpu, "empty");
			}
		}
	return programCounter;
    }

}
