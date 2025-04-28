public class associativeSimulator {
    int KBs;
    int blocks;
    int way;
	int cacheSize;
	int[][] cache;
    int [][] localityRefrences;
	
	public associativeSimulator(int KBs, int way, int blocks) {
		this.KBs = KBs;
        this.blocks = blocks;
        this.way = way;
		this.cacheSize = (KBs * 256)/(way * blocks);
        this.cache = new int[this.cacheSize][this.way];
		this.localityRefrences = new int[this.cacheSize][this.way];
	}

    /* converts a binary number to its decimal equivelent */
	private int binaryToDecimal(int binary) {
		int decimal = 0;
		int base = 1;

		while(binary != 0){
			int last_digit = binary % 10;
			decimal += last_digit * base;
			base *= 2;
			binary = binary / 10;
		}
		
		return decimal;
	}

	/* extracts bits needed */
	private int extractBits(int binary, int start, int stop){
		int numBits = stop - start;
		binary = binary >> start;
		int mask = (1 << numBits) - 1;
		int extractedBinary = binary & mask;
		return extractedBinary;

	}
	
    /* returns true/false based off if cache is found */
	public boolean findCache(int refrence_address, int line) {
        int index = 0;
		int offset = 2;

        // checking the KBs (2 vs 4)
        if (this.KBs == 2) {
			index = index + 9;
		} else if (this.KBs == 4) {
			index = index + 10;
		}

        // checking the number of blocks (1 vs 2 vs 4)
		if (this.blocks == 2) {
            index = index - 1;
			offset = offset + 1;				
		} else if (this.blocks == 4) {
            index = index - 2;
			offset = offset + 2;
		}

        // checking the way set assosiative (2 vs 4)
		if (this.way == 2) {
			index = index - 1;
		} else if (this.way == 4) {
			index = index - 2;
		}
		
		int tag = 32 - index - offset; 
		int decimal_tag = binaryToDecimal(extractBits(refrence_address, 0, tag));
		int decimal_index = binaryToDecimal(extractBits(refrence_address, tag, (tag + index)));

        // going through the ways...
		for (int i=0; i<this.way; i++) {
            // we found the cache...
			if (this.cache[decimal_index][i] == decimal_tag) {
				localityRefrences[decimal_index][i] = line;
				return true;
			}	
		}

		int totalMemoryRefrenceAddresses = 5000000; 
		int wayIndex = 0;
        // going through the ways...
		for (int i=0; i<this.way; i++) {
			if (this.localityRefrences[decimal_index][i] == 0) {
				localityRefrences[decimal_index][i] = line;
				this.cache[decimal_index][i] = decimal_tag;
				return false;
			}
            else {
				if (this.localityRefrences[decimal_index][i] < totalMemoryRefrenceAddresses) {
					totalMemoryRefrenceAddresses = this.localityRefrences[decimal_index][i];
					wayIndex = i;
				}
			}
		}

		this.cache[decimal_index][wayIndex] = decimal_tag;
		this.localityRefrences[decimal_index][wayIndex] = line;
		return false;
	}

}