#print_depth 20;;
#use "pInt.mli";;
#use "pInt.ml";;
open Pint;;

randInit ;;

(* )
iToA  ( tonelliShanks (fromInt 101) (fromInt 56));;
iToA  ( tonelliShanks (fromInt 101) (fromInt 56));;
iToA  ( tonelliShanks (fromInt 101) (fromInt 56));;
iToA  ( tonelliShanks (fromInt 101) (fromInt 56));;
iToA  ( tonelliShanks (fromInt 101) (fromInt 56));;

iToA  ( tonelliShanks (fromInt 10009) (fromInt 1030));;
iToA  ( tonelliShanks (fromInt 10009) (fromInt 1030));;
iToA  ( tonelliShanks (fromInt 10009) (fromInt 1030));;
iToA  ( tonelliShanks (fromInt 10009) (fromInt 1030));;

iToA  ( tonelliShanks (fromInt 100049) (fromInt 44402));;
iToA  ( tonelliShanks (fromInt 100049) (fromInt 44402));;
iToA  ( tonelliShanks (fromInt 100049) (fromInt 44402));;

iToA  ( tonelliShanks (fromInt 1000000009) (fromInt 665820697));;
iToA  ( tonelliShanks (fromInt 1000000009) (fromInt 665820697));;
iToA  ( tonelliShanks (fromInt 1000000009) (fromInt 665820697));;
iToA  ( tonelliShanks (fromInt 1000000009) (fromInt 665820697));;

iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;
iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;
iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;

iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;

iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;

iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;

iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;

iToA  ( tonelliShanks (fromInt 1000000000039) (fromInt 881398088036));;

let t = jacobi       (aToI "261625816257469") (aToI "1625816257469") ;;

iToA ( tonelliShanks (aToI "261625816257469") (aToI "1625816257469")) ;;

let y = (aToI "30371965183805657") ;;
let z = (aToI "1625816257469197") ;;
let t = jacobi  y z ;;
iToA ( tonelliShanks y z) ;;

let y = (aToI "90279110948001673") ;;
let z = (aToI "16258162574691979") ;;
let t = jacobi  y z ;;
iToA ( tonelliShanks y z) ;;

let y = (aToI "562762115534513359") ;;
let z = (aToI "16258162574691974") ;;
let t = jacobi  y z ;;
iToA ( tonelliShanks y z) ;;



let y  =  (aToI "227913382624436437014903409949");;
let z  =  (aToI "41660815127637347468140745041");;
let t = jacobi  y z ;;
iToA ( tonelliShanks y z) ;;

iToA ( tonelliShanks y z) ;;

iToA ( tonelliShanks y z) ;;

iToA ( tonelliShanks y z) ;;
*)

let p = (aToI  "26959946667150639794667015087019630673557916260026308143510066298881" );;
let t = (fromInt 2021);;
let a = (aToI "18958286285566608000408668544493926415504680968679321075787234672564");;

let x = (add a (mul t (mul t t)));;
iToA x ;;

let x1 = (sub x (mul (fromInt 3 ) t ));;
iToA x1 ;;


iToA (tonelliShanks p x1);;
iToA (tonelliShanks p x1);;
iToA (tonelliShanks p x1);;
iToA (tonelliShanks p x1);;
                      
(*
iToA ( tonelliShanks y z) ;;

let t = jacobi (aToI "548285593277771470133") (aToI "41660815127637347468") ;;

iToA ( tonelliShanks (aToI "548285593277771470133")  (aToI "41660815127637347468")) ;;


let t = jacobi (aToI "227913382624436437014903409949") (aToI "41660815127637347468140745041") ;;

millerRabin 5 (aToI "100000000000000000000000000000000000000000000000577");;


iToA  ( tonelliShanks (aToI "26959946667150639794667015087019630673557916260026308143510066298881")
                      (aToI "18958286285566608000408668544493926415504680968679321075787234672564"));;


iToA  ( tonelliShanks (aToI "26959946667150639794667015087019630673557916260026308143510066298881")
                      (aToI "18958286285566608000408668544493926415504680968679321075787234672564"));;

iToA  ( tonelliShanks (aToI "26959946667150639794667015087019630673557916260026308143510066298881")
                      (aToI "18958286285566608000408668544493926415504680968679321075787234672564"));;



jacobi (aToI "100000000000000000000000000000000000000000000000577")
       (aToI "41660815127637347468140745042827704103445750172002") ;;

iToA  ( tonelliShanks (aToI "100000000000000000000000000000000000000000000000577")
                      (aToI "41660815127637347468140745042827704103445750172002"));;

iToA  ( tonelliShanks (aToI "100000000000000000000000000000000000000000000000577")
                      (aToI "41660815127637347468140745042827704103445750172002"));;

iToA  ( tonelliShanks (aToI "100000000000000000000000000000000000000000000000577")
                      (aToI "41660815127637347468140745042827704103445750172002"));;

*)