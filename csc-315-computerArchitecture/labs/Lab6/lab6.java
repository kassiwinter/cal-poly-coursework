// Names: Kassi Winter
// Section: CSC 315 - 01
/* Description: For this lab, you will implement a cache simulator.  This simulator will model 7 different cache configurations and print out the number of hits and the hit rate.  Simulate the following cache configurations (using LRU when appropriate and the cache is byte addressable):
2KB, direct mapped, 1-word blocks
2KB, direct mapped, 2-word blocks
2KB, direct mapped, 4-word blocks
2KB, 2-way set associative, 1-word blocks
2KB, 4-way set associative, 1-word blocks
2KB, 4-way set associative, 4-word blocks
4KB, direct mapped, 1-word blocks */

import java.io.*;
import java.util.*;

public class lab6 {

	public static void main(String[] args) throws IOException{
		/* initialize the variables for later computing */
		int cacheTotal = 0;
		int direct_2KB_1word_cacheHits = 0;
		int direct_2KB_2word_cacheHits = 0;
		int direct_2KB_4word_cacheHits = 0;
		int associative_2KB_2way_1word_cacheHits = 0;
		int associative_2KB_4way_1word_cacheHits = 0;
		int associative_2KB_4way_4word_cacheHits = 0;
		int direct_4KB_1word_cacheHits = 0;
		
		/* initalize the seven cache configurations */
		directSimulator direct_2KB_1word = new directSimulator(2, 1);
		directSimulator direct_2KB_2word = new directSimulator(2, 2);
		directSimulator direct_2KB_4word = new directSimulator(2, 4);
		associativeSimulator associative_2KB_2way_1word = new associativeSimulator(2, 2, 1);
		associativeSimulator associative_2KB_4way_1word = new associativeSimulator(2, 4, 1);
		associativeSimulator associative_2KB_4way_4word = new associativeSimulator(2, 4, 4);
		directSimulator direct_4KB_1word = new directSimulator(4, 1);
		
        /* prints out the number of hits/hit rate of given cache configurations*/
		File file = new File(args[0]);
		Scanner scan = new Scanner(file);
        // a). get cache hits
		while(scan.hasNextLine()) {
			String line;
			int refrence_address;

			line = scan.nextLine();
			refrence_address =  Integer.parseInt(line.substring(1,line.length()).trim(), 16);
			refrence_address = intToBinary(refrence_address);

            // case: 2KB, direct mapped, 1-word blocks
			if (direct_2KB_1word.findCache(refrence_address)) {
				direct_2KB_1word_cacheHits++;
			}
            // case: 2KB, direct mapped, 2-word blocks
			if (direct_2KB_2word.findCache(refrence_address)) {
				direct_2KB_2word_cacheHits++;
			}

            // case: 2KB, direct mapped, 4-word blocks
			if (direct_2KB_4word.findCache(refrence_address)) {
				direct_2KB_4word_cacheHits++;
			}
            // case: 2KB, 2-way set associative, 1-word blocks
			if (associative_2KB_2way_1word.findCache(refrence_address, cacheTotal)) {
				associative_2KB_2way_1word_cacheHits++;
			}

            // case: 2KB, 4-way set associative, 1-word blocks
			if (associative_2KB_4way_1word.findCache(refrence_address, cacheTotal)) {
				associative_2KB_4way_1word_cacheHits++;
			}

            // case: 2KB, 4-way set associative, 4-word blocks
			if (associative_2KB_4way_4word.findCache(refrence_address, cacheTotal)) {
				associative_2KB_4way_4word_cacheHits++;
			}

            // case: 4KB, direct mapped, 1-word blocks
			if (direct_4KB_1word.findCache(refrence_address)) {
				direct_4KB_1word_cacheHits++;
			}
			cacheTotal++;
		}

        // b). get cache hit rate
		double direct_2KB_1word_cachePercentage = (double)(direct_2KB_1word_cacheHits * 100) / cacheTotal;
		double direct_2KB_2word_cachePercentage = (double)(direct_2KB_2word_cacheHits * 100) / cacheTotal;
		double direct_2KB_4word_cachePercentage = (double)(direct_2KB_4word_cacheHits * 100) / cacheTotal;
		double associative_2KB_2way_1word_cachePercentage = (double)(associative_2KB_2way_1word_cacheHits * 100) / cacheTotal;
		double associative_2KB_4way_1word_cachePercentage = (double)(associative_2KB_4way_1word_cacheHits * 100) / cacheTotal;
		double associative_2KB_4way_4word_cachePercentage = (double)(associative_2KB_4way_4word_cacheHits * 100) / cacheTotal;
		double direct_4KB_1word_cachePercentage = (double)(direct_4KB_1word_cacheHits * 100) / cacheTotal;

		System.out.println("Cache #1\n" + 
						   "Cache: 2KB\tSet Associative: 1\tBlocks: 1\n" +
						   "Hits: " + direct_2KB_1word_cacheHits + "\tHit Rate: " + String.format("%.2f", direct_2KB_1word_cachePercentage) + "%\n" +
						   "---------------------------");
		System.out.println("Cache #2\n" + 
				           "Cache: 2KB\tSet Associative: 1\tBlocks: 2\n" +
				           "Hits: " + direct_2KB_2word_cacheHits + "\tHit Rate: " + String.format("%.2f", direct_2KB_2word_cachePercentage) + "%\n" +
				   		   "---------------------------");
		System.out.println("Cache #3\n" + 
				   		   "Cache: 2KB\tSet Associative: 1\tBlocks: 4\n" +
				   		   "Hits: " + direct_2KB_4word_cacheHits + "\tHit Rate: " + String.format("%.2f", direct_2KB_4word_cachePercentage) + "%\n" +
				   		   "---------------------------");		
		System.out.println("Cache #4\n" + 
				   		   "Cache: 2KB\tSet Associative: 2\tBlocks: 1\n" +
				   		   "Hits: " + associative_2KB_2way_1word_cacheHits + "\tHit Rate: " + String.format("%.2f", associative_2KB_2way_1word_cachePercentage) + "%\n" +
				   	       "---------------------------");		
		System.out.println("Cache #5\n" + 
         				   "Cache: 2KB\tSet Associative: 4\tBlocks: 1\n" +
         				   "Hits: " + associative_2KB_4way_1word_cacheHits + "\tHit Rate: " + String.format("%.2f", associative_2KB_4way_1word_cachePercentage) + "%\n" +
				   		   "---------------------------");		
		System.out.println("Cache #6\n" + 
						   "Cache: 2KB\tSet Associative: 4\tBlocks: 4\n" +
						   "Hits: " + associative_2KB_4way_4word_cacheHits + "\tHit Rate: " + String.format("%.2f", associative_2KB_4way_4word_cachePercentage) + "%\n" +
				           "---------------------------");		
		System.out.println("Cache #7\n" + 
				           "Cache: 2KB\tSet Associative: 1\tBlocks: 1\n" +
				           "Hits: " + direct_4KB_1word_cacheHits + "\tHit Rate: " + String.format("%.2f", direct_4KB_1word_cachePercentage) + "%\n" +
				           "---------------------------");		
		scan.close();
	}	
	


    /* converts an integer number to its binary equivelent  */
	private static int intToBinary(int num) {
		if(num == 0){
			return 0;
		}
		
		int binary = 0;
		int place_holder = 1;

		while(num > 0){
			int remainder = num % 2;
			binary += remainder * place_holder;
			place_holder *= 10;
			num = num / 2;
		}
		return binary;
	}

}