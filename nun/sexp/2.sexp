(SAT
 (
 	(type u ($u_0 $u_1)) 
	(val b $u_0) 
	(val a $u_1) 
	(val P $u_1)
  	(fun apply
		(
		lambda 
			((v_0 u) (v_1 u))
    		(
				if (and (= v_1 $u_1) (= v_0 $u_1)) $u_0
     	   		(
					if (and (= v_1 $u_0) (= v_0 $u_1)) $u_0 
					$u_0
				)
			)
		)
	)
 )
)
