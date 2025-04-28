import java.io.*;
import java.util.*;
import java.util.HashMap;
import java.util.Map;

public class mipsEmulator {
    // global variables
    Map<String, Integer> regs;
	int[] mem;
    branchPredictor branchPredict;
	
	public mipsEmulator(int size)
	{
        // initalizing variables 
		mem = new int[8192];
		regs = initializeRegs();
        branchPredict = new branchPredictor(size);
	}

	public Map<String, Integer> initializeRegs() {
		Map<String, Integer> regs = new HashMap<String, Integer>();
		regs.put("pc", 0);
		regs.put("0", 0);
		regs.put("zero", 0);
		regs.put("v0", 0); 
		regs.put("v1", 0); 
		regs.put("a0", 0); 
		regs.put("a1", 0); 
		regs.put("a2", 0); 
		regs.put("a3", 0); 
		regs.put("t0", 0); 
		regs.put("t1", 0); 
		regs.put("t2", 0); 
		regs.put("t3", 0); 
		regs.put("t4", 0); 
		regs.put("t5", 0); 
		regs.put("t6", 0); 
		regs.put("t7", 0);
		regs.put("s0", 0); 
		regs.put("s1", 0); 
		regs.put("s2", 0); 
		regs.put("s3", 0); 
		regs.put("s4", 0); 
		regs.put("s5", 0); 
		regs.put("s6", 0); 
		regs.put("s7", 0); 
		regs.put("t8", 0); 
		regs.put("t9", 0); 
		regs.put("sp", 0); 
		regs.put("ra", 0);
		return regs;
	}
	
	public Map<String, Integer> getRegister(){
		return this.regs;
	}

    public void setRegister(String register, int num) {
		this.regs.put(register, num);
	}
	
	public int getValue(String register){
		return this.regs.get(register);
	}

	public int getMemory(int idx) {
		return this.mem[idx];
	}
	
	public void setMemory(int idx, int num) {
		this.mem[idx] = num;
	}

    public String getAccuracy(String size, String filename) {
		if((branchPredict.numTotalPredictions == 0) || (branchPredict.numCorrectPredictions == 0 )){
			return size + "-bit GHR " + filename + ": no branches used, no predictions needed"; 
		}
		double percentage = 100.0*(branchPredict.numCorrectPredictions / branchPredict.numTotalPredictions);
		return size + "-bit GHR " + filename + ": "+ String.format("%.2f", percentage) + "% (" + branchPredict.numCorrectPredictions + 
			   " correct predictions, " + branchPredict.numTotalPredictions + " predictions)"; 
	}
}