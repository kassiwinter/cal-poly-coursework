 # Name:  Kassi Winter and Anna Makarewicz
 # Section: 01
 # Description: Takes two integers as inputs and modulates them

 # Java Section
 # public static void main(String[] args)  
 # {  
 #   Scanner scan= new Scanner(System.in);
 # 	 System.out.println("This program mods two numbers"); 
 #   System.out.println(" Enter a number: ");
 #   int num = scan.nextInt();
 #   System.out.println (" Enter a divisor: ");
 #   int div = scan.nextInt();
 #   int mod = num & (div - 1);
 #   System.out.println(" Mod = "); 
 #   System.out.print(mod); 
 # }


# MIPS Section
# declare global so programmer can see actual addresses.
.globl welcome_message
.globl number_prompt
.globl div_prompt
.globl result_message

#  Data Area (this area contains strings to be displayed during the program)
.data
welcome_message:
	.asciiz " This program mods two numbers \n"
number_prompt:
	.asciiz " \n Enter a number: "
div_prompt:
	.asciiz " \n Enter a divisor: "
result_message: 
	.asciiz " \n Mod = "

# Text Area (i.e. instructions)
.text

main:
	# Display the welcome message (loading 4 into $v0)
	addi     $v0, $0, 4	
    # This makes $a0 the welcome message (data section starts here)
	lui     $a0, 0x1001
	syscall


    # Display integer prompt
	addi     $v0, $0, 4
	# This makes $a0 the enter number message		
	lui     $a0, 0x1001
	addi     $a0, $a0,0x21
	syscall
    # Read 1st integer from the user (num)
	addi     $v0, $0, 5
	syscall
	# Clear $s0 for the result
	addi     $s0, $0, 0	
	# Store 1st integer to result (num)
	addu    $s0, $v0, $s0

    # Display divisor prompt 
	addi    $v0, $0, 4
	# This makes $a0 the enter divisor message			
	lui     $a0, 0x1001
	addi     $a0, $a0,0x35
	syscall
	# Read 2nd integer (divisor)
	addi	$v0, $0, 5			
	syscall
    # Subtract divisor by 1
    subu $v0, $v0, 1
    # And integer and divisor
	and    $s0, $s0, $v0 

    # Display the result text
	addi     $v0, $0, 4	
	# This makes $a0 the mod result message		
	lui     $a0, 0x1001
	addi     $a0, $a0,0x4A
	syscall
	# Display the mod
	addi     $v0, $0, 1			
	add 	$a0, $s0, $0
	syscall
	
	# Exit (load 10 into $v0)
	addi     $v0, $0, 10
	syscall
