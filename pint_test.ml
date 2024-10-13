
#use "pInt.mli";;
#use "pInt.ml";;

module Atest = (Pint : PINT);;
open Atest ;;

let (a,b) = quotRem (fromInt 2) (facult 30) ;;
iToA a ;;
iToA b ;;

let len = 150 
let p = List.init  len (fun x -> x) ;;
let flen = facult len;;

randInit () ;;

let pp = permutation p (rand flen) ;;




