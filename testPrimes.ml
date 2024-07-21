
#print_depth 20;;
#use "pInt.mli";;
#use "pInt.ml";;
open Pint;;


let mersenne a = (sub (power (fromInt 2)  (fromInt a)) (fromInt 1))


let rec testPrimes_ n k c =
    if (n < c) then ()
    else 
    let p = rand k in
    if (millerRabin 10 p) then 
    	 let () = print_newline() in
       let () = Printf.printf " %s is probably prime  \n"  (iToA p)
       in
       	testPrimes_ n k (c+1 )
    else
       	testPrimes_ n k c
    
let  testPrimes n k = testPrimes_ n k 1 
 