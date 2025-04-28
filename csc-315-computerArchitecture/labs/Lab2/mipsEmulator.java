// Names: Kassi Winter and Anna Makarewicz
// Section: CSC 315 - 01

import java.util.Scanner;
import java.io.*;
import java.util.HashMap;
class mipsEmulator {
        public static void main(String args[]){
       		 // load in MIPS assembly file
        	Scanner in = new Scanner(System.in);
                System.out.println("What is the filename?");
                String input = in.nextLine();
				in.close();
				File pass1 = new File("pass1" + input);
		try {
			FileInputStream file = new FileInputStream(input);
			pass1.createNewFile();

			//Pass #1
        		Scanner scanner = new Scanner(file);
			FileWriter myWriter = new FileWriter(pass1.getName());
			while (scanner.hasNextLine()) {
				//remove all comments
				//remove all whitespace		
				String line = scanner.nextLine();	
				if (!(line.contains("#"))) {
					myWriter.write(line.replaceAll(" ", ""));
					myWriter.write("\n");
				}
			}
			scanner.close();
			myWriter.close();
		}
		catch (Exception ex) {
			ex.printStackTrace();
		}
		// Pass #2
		// Create a hashmap for labels and their line numbers
		HashMap<String, Integer> labels = new HashMap<String, Integer>();
		try {
			Scanner scr = new Scanner(new File(pass1.getName()));
			int lineNum = 0;
			while (scr.hasNextLine()) {
				String line = scr.nextLine();
				if(line.contains(":")){
					String arr[] = line.split(" ", 2);
					labels.put(arr[0].replace(":", ""), lineNum);
				}
				lineNum += 1;
			}
			scr.close();
		}
		catch (Exception ex) {
			ex.printStackTrace();
		}
		// Convert instructions
		try {
			Scanner sc = new Scanner(new File(pass1.getName()));
			while (sc.hasNextLine()) {
				String line = sc.nextLine();
				System.out.println(Instructions.convertLine(line));
			}
			sc.close();
		}
		catch(IOException e) {
			e.printStackTrace();
		} 
	}	
}
     
