(+ (* 2 3) (div 10 2))


push 3
push 4
push add
call
7


---------------->

pusharg 
pusharg 4
push opp add
call ------>
7
ret
