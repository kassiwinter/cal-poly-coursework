// Names: Kassi Winter and Anna Makarewicz
// Section: CSC 315 - 01
/* Description:  Acts as our CPU simulation. */

import java.util.HashMap;

public class CPU {
	HashMap<String, String> regs;

    boolean load;
	boolean branch;
	boolean jump;

    int loadRate;
    int branchRate;
    int jumpRate;

    int instructions;
	int cycles;
	int stalls;
	
	public CPU() {
		regs = initializeRegs();

        load = false;
		branch = false;
		jump = false;

		branchRate = 0;
		jumpRate = 0;
		loadRate = 0;

        instructions = 0;
		cycles = 0;
		stalls = 0;
	}
	
	public HashMap<String, String> initializeRegs() {
		HashMap<String, String> regs = new HashMap<>();
		regs.put("if/id reg", "empty");
		regs.put("id/exe reg", "empty");
		regs.put("exe/mem reg", "empty");
		regs.put("mem/wb reg", "empty");
		return regs;
	}
	
	public String print() {
		double cpi = (double)this.cycles/this.instructions;
		String cpiStr = String.format("%.3f", cpi);
		return "CPI = " + cpiStr + "\tCycles = " + this.cycles + "\tInstructions = " + this.instructions;
	}
	
	public void squash() {
		this.regs.put("mem/wb reg", this.regs.get("exe/mem reg"));
		this.regs.put("if/id reg", "squash");
		this.regs.put("id/exe reg", "squash");
		this.regs.put("exe/mem reg", "squash");
		this.branchRate = 0;
		this.branch = false;
		this.instructions-=2;
	}

    public void stall() {
		this.regs.put("mem/wb reg", this.regs.get("exe/mem reg"));
		this.regs.put("exe/mem reg", this.regs.get("id/exe reg"));
		this.regs.put("id/exe reg", "stall");
		this.loadRate = 0;
	}
}

