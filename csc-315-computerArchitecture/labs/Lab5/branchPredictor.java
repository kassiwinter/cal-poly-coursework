public class branchPredictor {
	int[] predictions;
    int[] ghr;
	int numTotalPredictions;
    int numCorrectPredictions;
	
	public branchPredictor(int size) {		
		predictions = new int[(int)Math.pow(2, size)];
		ghr = new int[size];
        numTotalPredictions = 0;
		numCorrectPredictions = 0;
	}
	
	public boolean getPrediction() {
		int index = getIndex(this.ghr);
		if (this.predictions[index] <= 1) {
			return false;
		}
        return true;
	}
	
	public void updatePrediction(boolean realBranch) {		
		int index = getIndex(this.ghr);
		if (realBranch == true){
			if(this.predictions[index] < 3) {
				this.predictions[index]++;
			}
		} 

        else {
			if(this.predictions[index] > 0) {
				this.predictions[index]--;
			}
		}

        int i = this.ghr.length - 1;
		while (i > 0){
			this.ghr[i] = this.ghr[i-1];
            i = i - 1;
		}

		if (realBranch) {
			this.ghr[0] = 1;
		} 
        else {
			this.ghr[0] = 0;
		}
	}
	
	public static int getIndex(int[] ghr) {
		int index = 0;
        int i = 0;

        while( i < ghr.length) {
			if (ghr[i] == 1) {
				index = index + (int)Math.pow(2,i);
			}
            i = i + 1;
		}

		return index;
	}
}