#print_depth 20;;

#use "pInt.mli";;
#use "pInt.ml";;
open Pint;;

randInit ;;

let mersenne a = (sub (power (fromInt 2)  (fromInt a)) (fromInt 1))

let testMR a =
   let  y  = rand a in
   if ( millerRabin 10 y) 
		then    Printf.printf " is %s prime ?  maybe yes\n "  (iToA y) 
   	else    Printf.printf " is %s prime ?  no\n"  (iToA y)  ;;
   
   
let rec prime  a =      
   if ( millerRabin 10 a)
   		then    Printf.printf " %s  is probably prime\n "  (iToA a ) 
   		else if ( isEven a) then  prime (addInt a 1) else prime (addInt a 2);;


let rec prime1  a =      
   if ( millerRabin 10 a)
   		then    Printf.printf " %s  is probably prime\n "  (iToA a ) 
   		else prime (rand a);;


prime (rand (mersenne 50));;

prime (rand (mersenne 55));;

prime (rand (mersenne 56));;
prime (rand (mersenne 57));;
prime (rand (mersenne 58));;
prime (rand (mersenne 59));;

prime (rand (mersenne 60));;

prime (rand (mersenne 70));;

prime (rand (mersenne 80));;

prime (rand (mersenne 90));;

prime (rand (mersenne 100));;

(* 
let rec iter x n = 
        let () = print_newline() in 
        let () = prime (rand n) in
        if (x = 0) then print_newline() else iter (x-1) n
*)
