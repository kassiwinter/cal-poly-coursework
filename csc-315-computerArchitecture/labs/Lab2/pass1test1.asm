	
	add$s0,$0,$zero
	addi$a0,$a0,100
	addi$a0,$a0,-1
	addi$a0,$a0,100
	
test1:



lw	$a0,8($a1)
	sw$a0,4($a1)
jtest1		
jr	$ra
	jaltest1
	slt$t0,$a0,$a1
	beq$t0,$t1,test
	sub$t3,$t1,$t1




	
sll$a0,$a1,2
