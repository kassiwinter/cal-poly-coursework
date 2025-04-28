// Names: Kassi Winter and Anna Makarewicz
// Section: CSC 315 - 01

import java.io.*;
import java.lang.*;
import java.util.*;
import java.util.HashMap;

public class Registers {
    // Create a HashMap object for the registers
    HashMap<String, String> registers = new HashMap<String, String>() {{
    put("$zero", "0000");
    put("$0", "0000");
    put("$v0", "0000");
    put("$v1", "0000");
    put("$a0", "00100");
    put("$a1", "00101");
    put("$a2", "00110");
    put("$a3", "00111");
    put("$t0", "01000");
    put("$t1", "01001");
    put("$t2", "01010");
    put("$t3", "01011");
    put("$t4", "01100");
    put("$t5", "01101");
    put("$t6", "01110");
    put("$t7", "01111");
    put("$s0", "10000");
    put("$s1", "10001");
    put("$s2", "10010");
    put("$s3", "10011");
    put("$s4", "10100");
    put("$s5", "10101");
    put("$s6", "10110");
    put("$s7", "10111");
    put("$t8", "11000");
    put("$t9", "01110");
    put("$sp", "11101");
    put("$ra", "11111");
    }};

    public HashMap<String, String> getRegisters(){
        return registers;
    }
}