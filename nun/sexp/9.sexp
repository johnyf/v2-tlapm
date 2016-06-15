(SAT
 ((type alpha_u ($alpha_u_0 $alpha_u_1)) (val b $alpha_u_1)
  (val a $alpha_u_1) (val c $alpha_u_0)
  (val (unique_unsafe prop)
   (fun ((v_0 (-> prop prop)))
    (if
     (= v_0
      (fun ((P prop))
       (and (= P (mem_raw x y)) (exists ((a alpha_u)) (= a x))
        (exists ((b alpha_u)) (= b y))))) ((?__ _) v_0 v_1) ((?__ _) v_0))))
  (val mem (fun ((v_0 alpha_u) (v_1 alpha_u)) ((?__ _) v_0 v_1)))
  (val app (fun ((v_0 alpha_u) (v_1 alpha_u)) $alpha_u_1))
  (val mem_raw
   (fun ((v_0 alpha_u) (v_1 alpha_u))
    (if (and (= v_1 $alpha_u_1) (= v_0 $alpha_u_0)) true false)))
  (val trans_mem
   (fun ((v_0 alpha_u) (v_1 alpha_u))
    (if (and (= v_1 $alpha_u_1) (= v_0 $alpha_u_0)) true false)))
  (val dom
   (fun ((v_0 alpha_u)) (if (= v_0 $alpha_u_1) $alpha_u_0 $alpha_u_1)))))
