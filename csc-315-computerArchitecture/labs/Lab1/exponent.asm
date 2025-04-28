 # Name:  Kassi Winter and Anna Makarewicz
 # Section: 01
 # Description: Takes two integers and raises one to the power of the other

 # Java Section
 # public static void main(String[] args)  
 # {  
 #   Scanner scan = new Scanner(System.in);
 # 	 System.out.println("This program pows two numbers"); 
 #   System.out.println(" Enter a x: ");
 #   int x = scan.nextInt();
 #   System.out.println(" Enter a y: ");
 #   int y = scan.nextInt();
 #   int power = Math.pow(x, y);
 #   System.out.println(" Pow = "); 
 #   System.out.print(power); 
 # }

# MIPS Section
# declare global so programmer can see actual addresses.
.globl welcome_message
.globl x_prompt
.globl y_prompt
.globl result_message

# Data Area (this area contains strings to be displayed during the program)
.data
welcome_message:
	.asciiz " This program pows two numbers \n"
x_prompt:
	.asciiz "\n Enter a x: "
y_prompt:
	.asciiz "\n Enter a y: "
result_message: 
	.asciiz " \n Pow = "

# Text Area (i.e. instructions)
.text

main:
	# Display the welcome message (load 4 into $v0 to display)
	addi     $v0, $0, 4	
    # This makes $a0 the welcome message (data section starts here)
	lui     $a0, 0x1001
	syscall

    # Display x prompt
	addi     $v0, $0, 4
	# This makes $a0 the enter x message		
	lui     $a0, 0x1001
	addi     $a0, $a0,0x21
	syscall
    # Read x from the user
	addi     $v0, $0, 5
	syscall
	# Clear $s0 to store
	addi     $s0, $0, 0	
	# Store
	addu    $s0, $v0, $s0

    # Display y prompt 
	addi     $v0, $0, 4		
	# This makes $a0 the enter y message		
	lui     $a0, 0x1001
	addi     $a0, $a0,0x30
	syscall
	# Read y from the user
	addi	$v0, $0, 5			
	syscall
    # Clear $s1 to store
	addi     $s1, $0, 0	
	# Store
	addu    $s1, $v0, $s1


	# Clear $s2 to store 1
	addi     $s2, $0, 0	
	addi    $s2, $s2, 1
	# Clear $s3 to store result
	addi     $s3, $0, 0	
	# Clear $s4 to store increment
	addi	$s4, $0, 0
	add	$s4, $s4, $s0
	# Set result to 1 and return 1 if y is 0
	addi 	$s3, $s3, 1
	beq	$s1, 0, exit
	# Set result to x
	add	$s3, $0, $s0
	# Set counter to x
	add	$s5, $0, 0
	add	$s5, $s5, $s0
	loop:
		# if(y<2), jump to exit
		blt	$s1, 2, exit
		aloop:
			# if x < 2, jump to done
			# result += increment
			blt	$s5, 2, done
			add 	$s3, $s3, $s4
			subu	$s5, $s5, $s2
			j aloop
		done:
		# increment = result
		add $s4, $0, $s3
		# set $s5 back to x
		add $s5, $0, $s0
		# y = y - 1
		subu	$s1, $s1, $s2
		j loop

	exit:
	# Display the result text
	addi     $v0, $0, 4			
	lui     $a0, 0x1001
	# This makes $a0 the result message	
	addi     $a0, $a0,0x3E
	syscall
	# Display the result
	addi     $v0, $0, 1			
	add 	$a0, $s3, $0
	syscall
	
	# Exit (load 10 into $v0)
	addi     $v0, $0, 10
	syscall


