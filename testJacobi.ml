
#print_depth 20;;
#use "pInt.mli";;
#use "pInt.ml";;
open Pint;;

(*
let (q,r) = quotRem (fromInt 7) (fromInt 5);;
iToA q ;;
iToA r ;;

let (q,r) = quotRem (fromInt 5) (fromInt 7);;
iToA q ;;
iToA r ;;

let (q,r) = quotRem (fromInt 1001) (fromInt 9907);;
iToA q ;;
iToA r ;;

let x = jacobi (fromInt 7 ) (fromInt 5 );;

let x = jacobi (fromInt 5 ) (fromInt 7 );;

let x = jacobi (fromInt 9907 ) (fromInt 1001 );;

let t = jacobi (aToI "227913382624436437014903409949") (aToI "41660815127637347468140745041") ;;

let rec tJ1 n k c =
    if (k < c) then ()
    else 
       let () = Printf.printf " (%d/%d) = %d \n" n c  (jacobi (fromInt n) (fromInt c)) 
       in
       	tJ1 n k (c+1 )
    
let  tJ n k = tJ1 n k 1 
  *)
          
let t = jacobi (aToI "227913382624436437014903409949") (aToI "178893475118222242116134303738") ;;
          
