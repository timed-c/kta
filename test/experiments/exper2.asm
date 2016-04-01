

.text
	add	$a0,$zero,100
	addi	$a1,$zero,50
	jal	exper
stop:	j      	stop

#input: 
#  a0 = start counter
#  a1 = end number
#output:
#  v0 = sum result
exper:
	addi	$v0,$zero,0
loop:   add 	$v0,$v0,$a0
	addi	$a0,$a0,-1	
	bne	$a0,$a1,loop
return:	
	jr	$ra
