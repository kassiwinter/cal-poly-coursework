 # Name:  Kassi Winter and Anna Makarewicz
 # Section: 01
 # Description: Takes an integer as input and reverses it

 # Java Section
 # public static void main(String[] args)  
 # {  
 #   Scanner scan= new Scanner(System.in);
 # 	 System.out.println("This program reverses a number "); 
 #   System.out.println(" Enter an integer: ");
 #   int num = scan.nextInt();
 #   int rev = reverse(num);
 #   System.out.println(" Reversed = "); 
 #   System.out.print(rev); 
 # }

 # public static int reverse(int num) {
 # int result = 0;
 #
 # for(int i = 0; i < 31; i++) {
 #  result <<<= 1;
 #  if ((num && 1) == 1) {
 #      result = result ^ 1;
 #   }
 #		num >>>= 1;
 #  }
 #
 # return result;
 # }

# MIPS Section
# declare global so programmer can see actual addresses.
.globl welcome_message
.globl number_prompt
.globl result_message

# Data Area (this area contains strings to be displayed during the program)
.data
welcome_message:
	.asciiz " This program reverses a number \n"
number_prompt:
	.asciiz " \n Enter a number: "
result_message: 
	.asciiz " \n Reversed : "

# Text Area (i.e. instructions)
.text

main:
	# Display the welcome message (load 4 into $v0)
	addi     $v0, $0, 4	
    # This makes $a0 the welcome message (data section starts here)
	lui     $a0, 0x1001
	syscall

	# Display number prompt
	addi     $v0, $0, 4
    # This makes $a0 the enter number message
	lui     $a0, 0x1001
	addi     $a0, $a0,0x22
	syscall
    # Read integer from the user
	addi     $v0, $0, 5
	syscall
	# Clear $s0 for the integer number
	addi     $s0, $0, 0	
    # Store integer in $s0
	addu    $s0, $v0, $s0

	# Clear $s1 for the result variable
	addi     $s1, $0, 0	
	# Clear $s2 to hold 32
	addi 	$s2, $0, 32
	# Clear $t0 to hold 1
	addi 	$t0, $0, 1

loop:
	# Loop for all 32 bits (maybe from 31 to 0??)
    beq $s2, $0, exit
    # Bitmask of 1 & it with integer
    and $t1, $s0, $t0
    if:
        bne $t0, $t1, else
		# bit = 0;
		sll $s1, $s1, 1
        srl $s0, $s0, 1
		sub $s2, $s2, $t0
    	j loop
    else:
		# bit != 0;
		sll $s1, $s1, 1
        # add 1 to result (bitshift)
        addu $s1, $s1, $t0
        srl $s0, $s0, 1
		sub $s2, $s2, $t0
    	j loop

exit:
    # Display the result text
	addi     $v0, $0, 4		
	# This makes $a0 the reversed message	
	lui     $a0, 0x1001
	addi     $a0, $a0,0x36
	syscall
	# Display the mod
	addi     $v0, $0, 1			
	add 	$a0, $s1, $0
	syscall
	
	# Exit (load 10 into $v0)
	addi     $v0, $0, 10
	syscall


