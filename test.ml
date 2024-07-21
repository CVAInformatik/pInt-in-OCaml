#print_depth 20;;



#use "pInt.mli";;
#use "pInt.ml";;

module Atest = (Pint : PINT);;



let a = Pint.aToI("797354927346482684582548164");;
let b = Pint.aToI("797354927346482684582548163");;
let aneg = Pint.aToI("-797354927346482684582548164");;
let bneg = Pint.changeSign( b);;

let c = Pint.mul a b;;

Pint.iToA c ;;

let x = Pint.aToI("104644849441");;
let y = Pint.aToI("19");;

let (z1, z2, z3) = Pint.gCD x y ;;

Pint.iToA z3 ;;
Pint.iToA z2 ;;
Pint.iToA z1 ;;



let x = Pint.aToI("100");;
let y = Pint.aToI("20");;

let (z1, z2, z3) = Pint.gCD x y ;;

Pint.iToA z3 ;;
Pint.iToA z2 ;;
Pint.iToA z1 ;;

let x = Pint.aToI("797354927346482684582548164");;
let y = Pint.aToI("797354927346482684582563");;

let (z1, z2, z3) = Pint.gCD x y ;;

Pint.iToA z3 ;;
Pint.iToA z2 ;;
Pint.iToA z1 ;;


let rec testGCD1 s1 s2 (r1, r2, r3) =
				Printf.printf " x %s y %s r1 %s r2 %s r3 %s\n  x * r1 %s\n  y * r2 %s\n (x * r1) + (y * r2) %s \n" 
		    		    (Pint.iToA s1) (Pint.iToA s2 ) (Pint.iToA r1) (Pint.iToA r2) (Pint.iToA r3) 
		    		    (Pint.iToA (Pint.mul s1 r1)) 
		    		    (Pint.iToA (Pint.mul s2 r2)) 
		    		    (Pint.iToA (Pint.add (Pint.mul s1 r1)  (Pint.mul s2 r2)));;
				 	

let rec testGCD s1 s2 = testGCD1 ( Pint.aToI s1) (Pint.aToI s2) (Pint.gCD (Pint.aToI s1) (Pint.aToI s2) );;


testGCD "797354927346482684582548164" "797354927346482684582563";;

testGCD "18286312628631286311237" "286328631286312863118";;

testGCD "104644849441" "19";;

testGCD  "1828631262863128"  "2863286312863128";;
testGCD  "797354927346482684582548164" "797354927346482684582563" ;;
testGCD  "1828631262863128"  "2863286312863128";;

Pint.iToA (Pint.fromInt 999999999999999) ;;
Pint.iToA (Pint.fromInt 9999999999999999) ;;
Pint.iToA (Pint.fromInt 99999999999999999) ;;
Pint.iToA (Pint.fromInt 999999999999999999) ;;

Pint.iToA (Pint.fromInt ~-999999999999999) ;;
Pint.iToA (Pint.fromInt ~-9999999999999999) ;;
Pint.iToA (Pint.fromInt ~-99999999999999999) ;;
Pint.iToA (Pint.fromInt ~-999999999999999999) ;;


let x1 = Pint.aToI "2941247924712947192479124771294791247917249";;
let x2 = Pint.aToI "8797963563428642";;
let x3 = Pint.aToI "123456789101112131415161718";;
let x4 = Pint.aToI "5";;
let x5 = Pint.aToI "123456789101112131415161718123456789101112131415161718";;
let x6 = Pint.aToI "123456789101112131415161718";;

      


let (q, m) = Pint.quotRem x2 x1 ;;

Printf.printf " q %s m %s \n" ( Pint.iToA q)  (Pint.iToA m) ;;

Printf.printf " q * x2 + m %s \n"  (Pint.iToA  (Pint.add (Pint.mul  q x2)  m)) ;;

Printf.printf " (q-1) * x2 + (m + x2)  %s \n"  
 (Pint.iToA  (Pint.add (Pint.mul  (Pint.sub q (Pint.fromInt 1)) x2 ) (Pint.add m x2))) ;;

let testQuotRem a b =
		let (q, m) = Pint.quotRem a b 
		in 	() = Printf.printf " q*a + m %s \n      a  %s \n      b  %s \n       q  %s \n       m  %s \n\n" 
		          ( Pint.iToA (Pint.add m ( Pint.mul q a)))  (Pint.iToA a) (Pint.iToA b) ( Pint.iToA q)  (Pint.iToA m)
		       
let x12 = testQuotRem x1 x2;;
Pint.iToA x2;;
Printf.printf " \n\n"  ;;
let x21 = testQuotRem x2 x1;;
Pint.iToA x1;;
Printf.printf " \n\n"    ;;
let x13 = testQuotRem x1 x3;;
Pint.iToA x3;;
Printf.printf  "\n\n"    ;;
let x31 = testQuotRem x3 x1;;
Pint.iToA x1;;
Printf.printf " \n\n"    ;;
let x14 = testQuotRem x1 x4;;
Pint.iToA x4;;
Printf.printf " \n\n"    ;;
let x41 = testQuotRem x4 x1;;
Pint.iToA x1;;
Printf.printf " \n\n"    ;;
let x15 = testQuotRem x1 x5;;
Pint.iToA x5;;
Printf.printf " \n\n"    ;;
let x51 = testQuotRem x5 x1;;
Pint.iToA x1;;
Printf.printf " \n\n"    ;;
let x16 = testQuotRem x1 x6;;
Pint.iToA x6;;
Printf.printf " \n\n"    ;;
let x61 = testQuotRem x6 x1;;
Pint.iToA x1;;
Printf.printf " \n\n"    ;;

Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 10));;
Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 20));;
Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 30));;
Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 32));;
Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 40));;
Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 50));;
Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 60));;
Pint.iToA (Pint.power (Pint.fromInt 2) (Pint.fromInt 64));;


		        