// Names: Kassi Winter
// Section: CSC 315 - 01
/* Description: Main program that emulates the MIPS registers, 
data memory (use an int array size 8192), and program counter.
The emulator will run into either of 2 modes:  interactive or script. */

import java.io.*;
import java.util.*;

/* --- MAIN FUNCTION --- */
public class lab5 {
	    // main function
		public static void main(String[] args) throws IOException{
			mipsEmulator emulator;
			File file = new File(args[0]);
			if (args.length == 3) {
				// supporting 2 bits
				if (Integer.parseInt(args[2]) == 2) {
					 emulator = new mipsEmulator(2);					
				} 
				// supporting 4 bits
				else if (Integer.parseInt(args[2]) == 4) {
					   emulator = new mipsEmulator(4);
					} 
					
				// supporting 8 bits
				else {
					emulator = new mipsEmulator(8);
				}	
			} else {
				// DEFAULT
				emulator = new mipsEmulator(2);	
			}
			try {

				Scanner scanner1 = new Scanner(file);
				HashMap<String,Integer> labels = getLabels(scanner1);	
				Scanner scanner2 = new Scanner(file);
				ArrayList<String> instructions = getLines(scanner2);
		
					// if the user puts a script file as an argument, go to script mode
				if (args.length == 2) {
					File script = new File(args[1]);
					scriptMode(script, emulator, instructions, labels, args[0], args[2]);
				}	 
				
					// otherwise go to interactive mode as default
					else {
					interactiveMode(emulator, instructions, labels, args[0], args[2]);
				}	
				scanner1.close();
				scanner2.close();
			}	
			catch(Exception e) {
				System.out.println(e.toString());
			}											
		}
		
    // SCRIPT MODE
	public static void scriptMode(File file, mipsEmulator emulator, ArrayList<String> instructions, HashMap<String,Integer> labels, String filename, String size) throws FileNotFoundException {
		Scanner scan = new Scanner(file);
        int programCounter = 0;
        String line;
        
        // if the file is a valid file (contains something) 
		if (scan.hasNext()) {
            // go through the script file given, while you aren't at the end of the instructions
            while ((programCounter != -1) && (scan.hasNext())) {
                line = scan.nextLine().trim();
				System.out.println("mips> " + line + "\n");
				programCounter = acceptCommand(emulator, instructions, labels, line, filename, size, programCounter);
            }

		}
		scan.close();
	}
	
    // INTERACTIVE MODE
	public static void interactiveMode(mipsEmulator emulator, ArrayList<String> instructions, HashMap<String,Integer> labels, String filename, String size) throws IOException{
        try{
			BufferedReader reader = new BufferedReader(new InputStreamReader(System.in)); 
			int programCounter = 0;
        	String line;

        	// while you are not at the end of the instructions
        	while(programCounter != -1) {
				System.out.print("mips> "); // wait for user input
	        	line = reader.readLine().trim();
	        	programCounter = acceptCommand(emulator, instructions, labels, line, filename, size, programCounter);
			}
		}

		catch(Exception e) {
			System.out.println(e.toString());
		}
	}

    // HELPER FUNCTIONS
	
    // #1. taking in and processing the input commands wanted
	public static int acceptCommand(mipsEmulator emulator, ArrayList<String> instructions, HashMap<String,Integer> labels, String line, String filename, String size, int programCounter) {
		switch(line.charAt(0)) {
			// a). case of help
			case ('h'):
				System.out.println("h = show help\n" + 
								   "d = dump register state\n" + 
								   "s = single step through the program (i.e. execute 1 instruction and stop)\n" + 
						           "s num = step through num instructions of the program\n" + 
						           "r = run until the program ends and display timing summary\n" + 
						           "m num1 num2 = display data memory from location num1 to num2\n" + 
								   "b = output the branch predictor accuracy\n" +
						           "c = clear all registers, memory, and the program counter to 0\n" + 
						           "q = exit the program\n");
				break;

            // case of dump
			case ('d'):
			System.out.println("\npc = " + emulator.getValue("pc"));

			System.out.println("\n$0 = " + emulator.getValue("0") + "\t\t$v0 = " + emulator.getValue("v0") + "\t\t$v1 = " +
						emulator.getValue("v1") + "\t\t$a0 = " + emulator.getValue("a0"));

			System.out.println("\n$a1 = " + emulator.getValue("a1") + "\t\t$a2 = " + emulator.getValue("a2") + "\t\t$a3 = " +
						emulator.getValue("a3") + "\t\t$t0 = " + emulator.getValue("t0"));

			System.out.println("\n$t1 = " + emulator.getValue("t1") + "\t\t$t2 = " + emulator.getValue("t2") + "\t\t$t3 = " +
						emulator.getValue("t3") + "\t\t$t4 = " + emulator.getValue("t4"));

			System.out.println("\n$t5 = " + emulator.getValue("t5") + "\t\t$t6 = " + emulator.getValue("t6") + "\t\t$t7 = " +
						emulator.getValue("t7") + "\t\t$s0 = " + emulator.getValue("s0"));

			System.out.println("\n$s1 = " + emulator.getValue("s1") + "\t\t$s2 = " + emulator.getValue("s2") + "\t\t$s3 = " +
						emulator.getValue("s3") + "\t\t$s4 = " + emulator.getValue("s4"));

			System.out.println("\n$s5 = " + emulator.getValue("s5") + "\t\t$s6 = " + emulator.getValue("s6") + "\t\t$s7 = " +
						emulator.getValue("s7") + "\t\t$t8 = " + emulator.getValue("t8"));

			System.out.println("\n$t9 = " + emulator.getValue("t9") + "\t\t$sp = " + emulator.getValue("sp") + "\t\t$ra = " +
						emulator.getValue("ra") + "\n");
			break;

            // case of single step / one instruction
		    case ('s'):
			if (line.length() > 1) {
				int step = Integer.parseInt(line.substring(1,line.length()).trim());
				int i = 0;
			
				// finds the step executedS
				while (i < step) {
					if (programCounter < instructions.size()) {
						programCounter = mipsInstructions.processInstruction(emulator, instructions, labels, programCounter);
					}
					else {
						System.out.println("No instructions left to execute.");
					}	
					i = i + 1;
				}
				System.out.println("\t" + step + " instruction(s) executed");
			}
			else {
				if (programCounter < instructions.size()) {
					programCounter = mipsInstructions.processInstruction(emulator, instructions, labels, programCounter);
					System.out.println("\t1 instruction(s) executed");
				}
				else {
					System.out.println("No instructions left to execute.");
				}
			}
			break;

            // case of running the entire program
			case ('r'):
				while(programCounter < instructions.size()) {
					programCounter = mipsInstructions.processInstruction(emulator, instructions, labels, programCounter); 
				}
				break;

            // case of desplaying memory 
			case ('m'):
			String[] location = line.substring(0,line.length()).split(" ");
			int num1 = Integer.parseInt(location[1]);
			int num2 = Integer.parseInt(location[2]);
			System.out.println("\n");

			while(num1 < num2) {
				System.out.println("[" + num1 + "] = " + emulator.getMemory(num1));
				num1 = num1 + 1;
			}
			System.out.println("\n");
			break;

			// case of branch predictor accuracy
			case ('b'):
				String str = emulator.getAccuracy(size, filename);
				System.out.println(str); 
				break;

            // case of clearing all of the registers
			case ('c'):
			System.out.println("\tSimulator reset\n");
			emulator.initializeRegs();
			break;

			// case of exiting program
			case ('q'):
				return -1;

			default:
		}		
		emulator.setRegister("pc", programCounter); 
		return programCounter;
	}
	

	public static ArrayList<String> getLines(Scanner scanner){
		ArrayList<String> lines = new ArrayList<String>();
		while (scanner.hasNextLine()) {
			String line = scanner.nextLine();
			if (!(line.contains("#")) && !(line.trim().isEmpty())){
				if(line.contains(":")){
					line = line.substring(line.indexOf(":") + 1);
				}
				line = line.replaceAll("\t", "");
	;			lines.add(line.replaceAll(" ", ""));
			}
			else if (!(line.trim().isEmpty()) && line.trim().charAt(0) != '#') {
				int index = line.indexOf('#');
				lines.add(line.substring(0,index).replaceAll(" ", "")); 
				}
		}
		return lines;
    }

    public static HashMap<String, Integer> getLabels(Scanner scanner){
		HashMap<String, Integer> labels = new HashMap<String, Integer>();
		int lineNum = 0;
		while (scanner.hasNextLine()) {
			String line = scanner.nextLine();
			if (!line.contains("#")) {
				if(line.contains(":")) {
					int index = line.indexOf(":");
					labels.put(line.substring(0,index),lineNum);
				}
				lineNum += 1;
				}
	
			}
		return labels;	
		}

}