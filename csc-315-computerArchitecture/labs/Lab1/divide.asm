 # Name:  Kassi Winter and Anna Makarewicz
 # Section: 01
 # Description:  Divides a 64-bit unsigned number with a 31-bit unsigned number

 # Java Section
 # public static void main(String[] args)  
 # {  
 #   Scanner scan= new Scanner(System.in);
 # 	 System.out.println("This program divs two numbers"); 
 #   System.out.println(" Enter upper 32 bits: ");
 #   int upper = scan.nextInt();
 #   System.out.println (" Enter lower 32 bits: ");
 #   int lower = scan.nextInt();
 #   System.out.println (" Enter lower 31 bit divisor: ");
 #   int div = scan.nextInt();
 #   int result = divide(upper, lower, div);
 #   System.out.println(" Div = "); 
 #   System.out.print(result); 
 # }
 # public static int pow(int upper, int lower, int div) {
 #    while ((div || 0) > 1) {
 #      if((upper && 1) == 1) { 
 #          lower >>>= 1;
 #          lower += x8000;
 #          upper >>>= 1;
#           div >>>= 1;
 #      }
 #      else {
 #          lower >>>= 1;
 #          upper >>>= 1;
#           div >>>= 1;
 #      }
 #    }
 #    return upper, lower;
 # }

# MIPS Section
# declare global so programmer can see actual addresses.
.globl welcome_message
.globl upper_prompt
.globl lower_prompt
.globl divisor_prompt
.globl result_prompt

# Data Area (this area contains strings to be displayed during the program)
.data
welcome_message:
	.asciiz " This program divs two numbers \n\n"
upper_prompt:
	.asciiz " Enter upper 32 bits: "
lower_prompt:
	.asciiz "\n Enter lower 32 bits: "
divisor_prompt:
	.asciiz "\n Enter lower 32 bit divisor: "
result_message: 
	.asciiz " \n Div = "

# Text Area (i.e. instructions)
.text

main:
	# Display the welcome message (load 4 into $v0 to display)
	addi     $v0, $0, 4	
    # This makes $a0 the welcome message (data section starts here)
	lui     $a0, 0x1001
	syscall

    # Display upper 32 bits prompt
	addi     $v0, $0, 4
    # This makes $a0 the enter upper 32 bits message
	lui     $a0, 0x1001
	addi     $a0, $a0,0x22
	syscall
    # Read upper 32 bits from the user
	addi     $v0, $0, 5
	syscall
	# Clear $s0 to store
	addi     $s0, $0, 0	
	# Store
	addu    $s0, $v0, $s0

    # Display lower 32 bits prompt 
	addi     $v0, $0, 4		
	# This makes $a0 the enter lower 32 bits message	
	lui     $a0, 0x1001
	addi     $a0, $a0,0x39
	syscall
	# Read lower 32 bits from the user
	addi	$v0, $0, 5			
	syscall
    # Clear $s1 to store
	addi $s1, $0, 0	
	# Store
	addu $s1, $v0, $s1

    # Display divisor 32 bits prompt
	addi $v0, $0, 4
    # This makes $a0 the enter 32 bit divisor message
	lui $a0, 0x1001
	addi $a0, $a0,0x51
	syscall
     # Read 32 bit divisor from the user
	addi $v0, $0, 5
	syscall
	# Clear $s2 to store
	addi $s2, $0, 0	
	# Store
	addu $s2, $v0, $s0

    # Clear $s3 to = 1
	addi $s3, $0, 1	
    # Clear $s4 to have mask
    lui $s4, 0x8000

while:
    # Is (div || 0) > 1? If so exit loop...
    or $t0, $s2, $0
    beq $t0, $s3, exit
    if:
		#  upper && 1, == 1?
        and $t0, $s0, $s3
        beq $t0, $s3, else
		# lower right shift 1
        srl $s1, $s1, 1
        # higher right shift 1
        srl $s0, $s0, 1
        # div right shift 1
        srl $s2, $s2, 1
    	j while

    else:
		# lower right shift 1
        srl $s1, $s1, 1
        # lower += mask
        add $s1, $s1, $s4
        # higher right shift 1
        srl $s0, $s0, 1
        # div right shift 1
        srl $s2, $s2, 1
        j while

exit:
	# Display the result text
	addi     $v0, $0, 4			
	lui     $a0, 0x1001
	addi     $a0, $a0,0x71
	syscall
	# Display the upper 32
	addi     $v0, $0, 1			
	add 	$a0, $s0, $0
	syscall
    # Display the lower 32
	addi     $v0, $0, 1			
	add 	$a0, $s1, $0
	syscall
	# Exit (load 10 into $v0)
	addi     $v0, $0, 10
	syscall
