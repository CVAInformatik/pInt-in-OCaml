
 module Pint : PINT = struct 

type pInt = int list

(* on a 64 bit system int is 63 bit and not 31 as I originally thought, 
   the changes needed between 63 and 31 bit ints  are listed in the lines below  *)
let _MODULUS8 = 1_000_000_000_000_000_000
(* for 32 bit systems let _MODULUS8 = 100000000 *)

(* the string below can NOT be used as format string in Printf.sprintf for some advanced reasons, 
I dont quite understand yet, you will have to type it in yourself in  intsToString   *)
let _FSTRING =  "%018i"    (* %08i for 32 bit *)

(* end of  31/63 bit changes *)

exception UnexpectedChar

exception DivisionByZero 

exception NotaSquare  

exception InvalidArgument


let rec isEq l1 l2 =
       match l1,l2 with
       |  [], [] -> true;
       |  [], _  -> false
       |  _ , [] -> false
			 |  hd1::tl1, hd2::tl2 -> if( hd1 = hd2) 
			                          then ( isEq tl1 tl2)
			                          else false

let isZero l1 =
       match l1 with
       |  []  -> true;
       |  _::_ -> false

let rec isNegative l1 =
       match l1 with
       |  []  -> false;
       |  hd::tl -> if (hd = 0) 
       							then (isNegative tl)
       							else ( hd < 0 ) 
       										
let rec  mkpIntInvert  a =  ~-a 

let rec mkpIntFromStringAux  iList acc  mul acclist modulus =
 		match iList with
 		|  [] -> if (acc > 0 ) then acc::acclist  else acclist 
 		|  hd :: tl ->  if( hd == -1) 
 										then  List.map mkpIntInvert (acc::acclist) 
 		         				else 
 		            			if ( mul == (modulus/10)) 
 		            			then  mkpIntFromStringAux  tl 0  1  (( (mul*hd) +acc)::acclist ) modulus
  										else mkpIntFromStringAux  tl  (( mul * hd) + acc)   (mul * 10)  acclist modulus


let mapCharToInt  c =
    match c with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | '-' -> -1
    | _ -> raise UnexpectedChar 

let rec mkCharlistAux s pos  = 
    if(pos >= 0) 
    	then s.[pos] :: mkCharlistAux s (pos - 1)
    	else []

let rec mkCharList s =
     mkCharlistAux s ((String.length s ) - 1)

let rec mkpIntFromString iString =	List.rev (mkpIntFromStringAux  (List.map mapCharToInt (mkCharList iString )) 0  1  [] _MODULUS8) 

let aToI str = mkpIntFromString str 

let rec getSign pInt =
     match pInt with
     | [] -> " "
     |  hd::tl -> if (hd < 0) then "-"
                  else if (hd > 0 ) then " "
                       else getSign tl 

let rec  dropLeading0esAux s index =
		  match s.[index] with
			| '0' -> dropLeading0esAux s (index+1)		  
			| _   -> ( String.sub s index  ((String.length s)- index))

let rec dropLeading0es s = 
         if ( String.length s ) <= 1 then s
         else dropLeading0esAux s 0

let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))
  
let rec mkCompleteStringFrompInt pIntList = List.fold_left (^) "" pIntList 

let intsToString  i  = rev (Printf.sprintf "%018i" i )  (*  for  63 bit int , use %08i for 31 bit int*)

let mkPintPositive i = 		if( i>= 0) then i else ~- i

let rec mkStringFrompIntAux pInt = List.map intsToString (List.map  mkPintPositive  pInt)

let rec mkStringFrompInt pInt =
      match pInt with
      | [] -> (Printf.sprintf "0" ) ::[]
      | _::_ -> mkStringFrompIntAux pInt

let iToA pInt = (getSign pInt) ^ (dropLeading0es (rev (mkCompleteStringFrompInt (mkStringFrompInt pInt))))

let rec dropLeading0aux a = 
     match a with
     | [] -> []
     | 0 :: tl  -> dropLeading0aux tl
     | hd :: tl  ->  hd :: tl 

let rec dropLeading0 a = List.rev (dropLeading0aux (List.rev a))

let rec normalizepIntAux2 modulus carry sign  a  =
     match a with
     | [] -> []
		 |  0 :: tl ->  ( 0 + carry) :: ( normalizepIntAux2 modulus 0 sign tl ) 
		 | hd :: tl when  (( sign> 0 ) && ( (hd + carry) < 0 )) -> ( hd + carry + modulus) :: ( normalizepIntAux2 modulus ~-1 sign tl ) 
		 | hd :: tl when  (( sign< 0 ) && ( (hd + carry) > 0 )) -> ( hd + carry - modulus) :: ( normalizepIntAux2 modulus  1 sign tl ) 
		 | hd :: tl ->  ( hd +carry) :: ( normalizepIntAux2 modulus 0 sign tl ) 

(* adjust signs in so they match the sign of s *)
let rec normalizepIntAux1 modulus s a  =
     match a with
     | [] -> []
		 | hd :: tl -> normalizepIntAux2 modulus 0 s a 

(* drop MSDs, which are 0 *)
let rec normalizepIntAux modulus a =
     match a with
     | [] -> []
     | 0 :: tl  -> normalizepIntAux modulus tl 
     | hd :: tl -> normalizepIntAux1 modulus hd (List.rev a) 

(* drop MSDs, which are 0 and adjust signs, so they are consistent *)
let rec normalizepInt modulus a  = normalizepIntAux modulus (List.rev a) 

let rec carrypIntAux modulus a carry =
    match a with
    | []  -> if carry <> 0  then [carry] else []
    | hd :: tl when hd+carry >= modulus -> ( carry + hd - modulus):: (carrypIntAux modulus tl 1 )
    | hd :: tl when hd+carry <= ~-modulus -> ( carry + hd + modulus):: ( carrypIntAux modulus tl ~-1 )
    | hd :: tl -> ( carry + hd ):: ( carrypIntAux modulus tl 0 )

let rec carrypInt modulus a   =  carrypIntAux modulus  a 0 



let rec addpInt  a  b   =
   match  a, b   with
   | [], _ -> b
   | _, [] -> a
   | hd :: tl, hd2 ::tl2  -> (hd + hd2) :: (addpInt tl tl2)

let addPintM8 i1 i2 = dropLeading0 (normalizepInt _MODULUS8 (carrypInt  _MODULUS8 (addpInt i1 i2) ) )

let rec fromInt1 x = 
	if (x = 0 ) 
	  then  []
		else  (x mod _MODULUS8) :: fromInt1 (x / _MODULUS8)
		
let changeSign i1 = List.map mkpIntInvert i1
		
let rec fromInt x =
		if (x < 0 ) 
		then  changeSign (fromInt1  ~-x)
		else  fromInt1 x


let addInt i1 i2 = addPintM8  i1 (fromInt i2)

let add i1 i2 = addPintM8  i1 i2

let rec double2 i1 carry =
    match i1 with 
    | [] -> if (carry = 0) then [] else [1]
    | hd::tl ->  if ((hd+carry ) >= _MODULUS8) 
                 then (hd+carry - _MODULUS8 ) :: (double2 tl 1)
                 else (hd+carry ) :: (double2 tl 0)
and    
    double1 i1 =  (*  i1 >= 0 *)
    match i1 with 
    | [] -> []
    | _  ->     double2 ( List.map  (fun x-> x + x) i1 ) 0 

let double i1 = if (isNegative i1) then changeSign (double1 (changeSign i1)) else double1 i1

let sub i1 i2 = add i1 (changeSign i2)

let subInt i1 i2 = sub i1 (fromInt i2)

(*  isLt a b  ->  a > b *)
let isLt i1 i2 =   (not (isNegative ( sub i1 i2 )))

(* Russian Peasant multiplication   *)
let  isEven l1 = 
      match l1 with 
      | []  -> true 
      | hd::_ -> (0 = ( hd land 1 ))

let rec div2Aux2 l1 lc =
      match l1 with
      | [] ->   []
      | hd::tl -> ((hd + (List.hd lc))/2) ::  ( div2Aux2  (List.tl l1) (List.tl lc))

let div2Aux1 l1 =  div2Aux2 l1  (0 ::(List.map (fun x -> if ( 0 = 1 land x ) then  0 else _MODULUS8 ) l1))
        
let div2_ l1 =  List.rev (dropLeading0aux (div2Aux1 (List.rev l1  )))
      
let div2 l1 =  
	if (isNegative l1)  
		then changeSign ( div2_ (changeSign l1))
		else ( div2_ l1)
      
let rec rpMulAux i1 i2 acc = 
			match i1 with
			| []  -> acc
			| _::_	-> if( isEven i1) 
			           then rpMulAux (div2 i1 )  (double i2 ) acc 
			           else rpMulAux (div2 i1 )  (double i2) (add acc  i2)
 								
let rpMul_ i1 i2 = rpMulAux i1 i2 []

let rpMul i1 i2 = 
     match i1 with 
     |  [] -> []
     |  hd::tl -> if( isNegative i1)
     							then rpMul_ (changeSign i1) (changeSign i2) 
     							else rpMul_ i1  i2
     
     
     
(* Multiplication *)
let mul i1 i2 =  rpMul i1 i2

let rec power1 i1 i2 acc =
    match i2 with
    | [] ->  acc
    | _  -> if (isEven i2) 
    				then power1  (mul i1 i1) (div2 i2) acc
    				else power1  (mul i1 i1) (div2 i2) (mul i1 acc)

let power i1 i2 =
    match i1, i2 with
    | [], _ -> []
    | _, [] -> (fromInt 1)
    | _, _ -> power1 i1  i2 (fromInt 1)

(*   extended binary GCD *)
(* 
let rec debugPP1 x acc =
			match x with
			| []  -> List.rev acc
			| hd::tl	-> debugPP1 tl  ( hd::acc)

let debugPP x = debugPP1  x []
 		
*)	
 
let rec 
    gCD1 x y g =
		match(x, y) with
		| ([],_) -> ( [],[],[])
		| (_, []) -> ( [],[],[])
		| (_ , _ ) ->  
		     if((isEven x) && (isEven y) )
		     then gCD1 (div2 x) (div2 y) (double g)
		     else gCD2  x y x y [1]  [] []  [1] g 
and					
		gCD2 x y u v a b c d g =
    match ( u )  with
    |  [] -> ( [],[],[])
		|  _ ->  
		     if(isEven u) 
			    	then
		     			if((isEven a) && ( isEven b))
		     		   		then  gCD2 x y (div2 u)  v (div2 a) (div2 b) c d g 
		    					else  gCD2 x y (div2 u)  v (div2 (add y a)) (div2 (sub b x)) c d g
		    else 
		     gCD3 x y u v a b c d g
and		
		gCD3  x y u v a b c d g =
    match ( v)  with
    | [] -> ( [],[],[])
		| _  ->  
					if(isEven v) then
		     			if((isEven c) && (isEven d)) 
		    			then  gCD3 x y u (div2 v)   a  b (div2 c)  (div2 d) g 
		    			else  gCD3 x y u (div2 v)   a  b  (div2 (add c y)) (div2 (sub d x))  g
		    else
		      gCD4 x y u v a b c d g
and
    gCD4 x y u v a b c d g =
		if (isNegative ( sub u v)) 
		then gCD5 x y  u        (sub v u)        a         b  (sub  c a ) (sub d b ) g
		else gCD5 x y  (sub u v) v        (sub a c) (sub b d ) c           d         g
and
		gCD5 x y u v a b c d g = 
			match u with
			| []  ->   (c , d, (mul g v))
			| _   ->  gCD2 x y u v a b c d g
		
let gCD i1 i2 =  gCD1 i1 i2 [1]

let numVal x =
		match x with
		| [] -> []
		| _ -> if (isNegative x)  then (List.map  mkPintPositive  x) else x
	
 								
(* Quotient + Modulus 

   quotMod 22 7 =   ( 3,1)  since 22 = 3 * 7 + 1
   quotMod -22 7 =   ( -4, 6 )  since -22 = -4 * 7 + 6
   quotMod 22 -7 =   (-3, 1) since 22 = -3 *-7 +1
   quotMod -22 -7 =  (4, 6)  since -22 = 4 * -7 + 6
 *)

let rec quotRem2 p n =  
    let divisor = List.rev p in
		let multiplier = fromInt  (_MODULUS8 /(2+ (List.hd divisor ))) in   quotRem3  divisor (mul multiplier n)
and 
     quotRem3 d  acc =  (* make division with _MODULUS8 by throwing LSDs away *)
     match d,acc  with
     | _ ,[]  ->  []
     | [], _   -> acc
     |  _, _  ->  quotRem3  (List.tl d) (List.tl acc ) 
and 
     quotRem4 p n pseud  quotient =
     let 
       diff = (sub n (mul p quotient ) ) 
     in
   (*    let () = Printf.printf " qR4 p %s  n %s  quotient %s diff %s  ( pseud diff) %s\n"  (iToA p) (iToA n) (iToA quotient ) (iToA  diff ) (iToA ( pseud diff))   in  *)
       if( isLt p diff ) then quotRem5 p n quotient
       else 
       if (isZero ( pseud diff)) then quotRem4 p n pseud  (addInt quotient 1)
       else  quotRem4 p n pseud  (add quotient ( pseud diff)) 
and  
    quotRem5 a b quotient = 
      (* let () = Printf.printf " qR5 a %s  b %s  quotient %s diff %s \n"  (iToA a) (iToA b) (iToA quotient ) (iToA (sub b (mul a quotient ) ) )  in *)
      let 
         m =  (sub  b ( mul a quotient ))
      in
      		if (isNegative m) 
      		then ( (sub quotient (fromInt 1)) , (add m a) )  
      		else ( quotient, (sub b (mul a quotient)))
and  
   quotRem1 p n  = 
     let 
       pseudoReciprocal =  quotRem2 (numVal p)  (* curried application of quotRem2 and 3 *)
     in
       quotRem4 p n pseudoReciprocal []
and 
    quotRem p n =
    match p, n  with
    | [], _ ->  raise DivisionByZero
    | [x],[y] ->  ( (fromInt ( y/x))  , (fromInt ( y mod x)) )
    | _, [] ->  ( [], [])
    | _, _  ->  quotRem1 p n

(* modMul *)

let rec 
      rpModSum m x =
      if( isLt m x  ) then x else (sub x m )
and      
      rpMulModAux m i1 i2 acc = 
			match i1 with
			| []  -> acc
			| _::_	-> if( isEven i1) 
			           then rpMulModAux m (div2 i1 )  (rpModSum m (double i2)) acc 
			           else rpMulModAux m (div2 i1 )  (rpModSum m (double i2)) (rpModSum m (add acc  i2))
and 								
     rpMulMod_ m i1 i2 = rpMulModAux m i1 i2 []
and
     rpMulMod m  i1 i2 = 
     match i1 with 
     |  [] -> []
     |  hd::tl -> if( isNegative i1)
     							then rpMulMod_  m (changeSign i1) (changeSign i2) 
     							else rpMulMod_  m i1  i2
     							
and  rpMulMod1_ m i1 i2 =
		 if(isLt i1 i2) then rpMulMod m i1 i2 else rpMulMod m i2 i1
		 
let modMul m a b =
    let  (aq, ar) = quotRem m a in 
    let  (bq, br) = quotRem m b in 
    match ar, br  with
    | [], [] -> []
    | _ , [] -> []
    | [], _  -> []
    | _ , _  -> rpMulMod1_  m ar br 
    
    
(* modPow  *)
let rec modPow1 m  i1 i2 acc =
    match i2 with
    | [] ->  acc
    | _  -> if (isEven i2) 
    				then modPow1 m (modMul m i1 i1) (div2 i2) acc
    				else modPow1 m (modMul m i1 i1) (div2 i2) (modMul m i1 acc)

let modPow m i1 i2 =
    match i1, i2 with
    | [], _ -> []
    | _, [] -> (fromInt 1)
    | _, _ -> if( isEq m (fromInt 1) ) then [] else  modPow1 m i1  i2 (fromInt 1)
    
(*  jacobi symbol *)
    
let rec jacobi1 p n r =
(*     let () = Printf.printf "J1 p %s  n %s r  %d \n" (iToA p)  (iToA n)  r in *)
    match n with
    | []  ->  if (isEq  p (fromInt 1) ) then r else 0
    | _   -> jacobi2 p n r

 and 

    jacobi2 p n r =
(*    let () = Printf.printf "J2 p %s  n %s r  %d \n" (iToA p)  (iToA n)  r in *)
    match n with 
    | [] -> if (isEq  p (fromInt 1) ) then r else 0
    | hd::tl -> if (( hd land 1) = 0 ) 
    				then 
    					let s = (List.hd p ) land 7 in  
              if((s = 3) || (s = 5)) 
              then  jacobi2 p (div2 n)   ~-r
              else  jacobi2 p (div2 n)  r
            else
              jacobi3 n p r 
 and
    
   jacobi3 p n r =
(*     let () = Printf.printf "J3 p %s  n %s n  %d \n" (iToA p)  (iToA n)  r in *)
    match p, n with 
    | [], [] -> jacobi2 p n  r
    | [], _  -> jacobi2 p n  r
    | _ , [] -> jacobi2 p n  r
    | _ , _ ->  let (q,rem) = ( quotRem p n ) in 
                      if ((( List.hd p) land 3 =3) && (( List.hd n) land 3 = 3)) 
                      then jacobi2 p rem   ~-r
                      else jacobi2 p rem   r 
    
let jacobi p n = 
    match p, n with 
    | [], _ ->  1
    | _, _ ->   let (q,r) = quotRem p n  in if( isZero r ) then 0 else jacobi1 p n 1

(*    random numbers  *)

let randInit () = Random.self_init ()

let rec rand1 a b = 
     match a, b with
     | [], [] -> []
     | hda::tla, hdb::tlb  -> (dropLeading0 (List.rev ((hdb mod hda)::tlb))) 


let rand a =
    match a with 
    | [] -> []
    | _  -> rand1 (List.rev a ) (List.map (fun x -> ( Random.full_int  _MODULUS8) ) a)

(* Miller Rabin test  *)    

let rec millerRabin2 it n d s = 
				(*let () = Printf.printf "MR2 it %d   n %s d %s s %d \n"  it (iToA n)  (iToA d) s in*)
   			if( (isEven d) && (not (isZero d))) 
   			then  millerRabin2 it n (div2 d) (s+1)
   			else  millerRabin3 it n d s 

and 
    millerRabin3 it n d s =
				(*let () = Printf.printf "MR3 it %d   n %s d %s s %d \n"  it (iToA n)  (iToA d) s in*)
    if (it > 0)  then
	    if  millerRabin4 n d s  
    	    then false
  	      else millerRabin3 (it -1 ) n d s
    else true    			       
and 
    millerRabin4 n d s =
				(*let () = Printf.printf "MR4 n %s d %s s %d \n"   (iToA n)  (iToA d) s in*)
        let a = (add (fromInt 2) (rand ( sub n (fromInt 3) ) )) in
        let x = modPow n a d  in
        millerRabin5 n x s
and  
     millerRabin5 n x s =
  	 (*let () = Printf.printf "MR5.1 n %s x %s s %d \n"   (iToA n)  (iToA x) s in*)
     let one = fromInt 1 in
     if (s > 0) then
        let y = modMul n x x in
        let t = sub n x      in
	   	  (* let () = Printf.printf "MR5.2  y %s t %s \n"   (iToA y)  (iToA t) in*)
        if(( isEq y one) && (not (isEq x one)) && (not (isEq t one)) )
        then true 
        else millerRabin5 n y (s-1)
     else
        false = (isEq x one)
       
let rec millerRabin it n =
				(*let () = Printf.printf "NR it %d   n %s \n"  it (iToA n) in*)
     let d =  (sub n (fromInt 1))  in
     if (isZero n ) then false
     else if (isEven n ) 
          then false 
          else	millerRabin2 it n d 0

(*  we assume p is a prime ! *)


let rec tonelliShanks4 p =
     let z = rand p in 
     if ( -1 = (jacobi p z))  then z else tonelliShanks4 p  
     
let rec tonelliShanks6 p t i =
    if ( isEq t (fromInt 1)) then i else tonelliShanks6 p (modMul p t t)  (i+1)
     
let rec tonelliShanksLoop p c t m result =
     if( isZero t) then raise NotaSquare 
     else if (isEq t (fromInt 1)) then result
     else let  i  = tonelliShanks6 p t 0 in     
     let  temp2 = ( modPow p  (fromInt 2) (subInt (subInt m 1 ) i )) in
     let b = ( modPow p c temp2 ) in
     let c'= (modMul p b b) in
     let res = (modMul p result b) in
     tonelliShanksLoop p c'  (modMul p t c' ) (fromInt i) res 
 

let rec tonelliShanks2 p q n s = 
     if(isZero q ) then raise NotaSquare 
     else if(isEven q) then tonelliShanks2 p (div2 q) n (addInt s 1)
     else tonelliShanksLoop 
             p               (* p *)     
             (modPow p (tonelliShanks4 p ) q)  (* c *)
             (modPow p  n q )	(* t *)
             s                    (* m *)
             ( modPow p n  (div2 (addInt q 1)))  (*result *)

let rec tonelliShanks1 p n = tonelliShanks2 p (subInt p 1) n (fromInt 0)
     
let tonelliShanks p n =
   if (isZero p) then raise DivisionByZero
   else if (isNegative p) then raise InvalidArgument
   else if (isZero n) then raise DivisionByZero
   else if (isNegative n) then raise InvalidArgument
   else if (not (millerRabin 5 p)) then raise InvalidArgument
   else if ( (jacobi p n ) != 1) then raise NotaSquare 
   else  tonelliShanks1 p n 
 
 
 (* Schoolbook Multiplication  *)
 
 let _MODULUS10E9 = 1_000_000_000

let rec convToRadix10E9_ a =
      match a with 
      | [] -> []
      | hd::tl -> (hd mod  _MODULUS10E9) :: ( hd / _MODULUS10E9) :: (convToRadix10E9_ tl)

let rec convToRadix10E9 a = dropLeading0 (convToRadix10E9_ a )

let rec 
      convFromRadix10E9 a =
      match a with 
      | [] -> []
      | _  ->  convFromRadix10E9_ (List.hd a)  (List.tl a)      
      and       
      
			convFromRadix10E9_  a b  =
      match b with
			| [] ->  [a]
			| hd :: tl ->  (( (List.hd b) * _MODULUS10E9 ) + a ) :: ( convFromRadix10E9 (List.tl b) )

(* a and b are not zero and positive *)
 
let rec schoolbookMul1digitCarry a carry = 
     match a with 
     | [] -> if(carry > 0) then [carry] else []
     | hda::tla  -> if( (hda + carry) >= _MODULUS10E9 )
                    then ( ( hda + carry) mod _MODULUS10E9) :: (schoolbookMul1digitCarry tla ((hda + carry) / _MODULUS10E9) )
                    else ( hda + carry ) :: (schoolbookMul1digitCarry tla  0)
             
let rec schoolbookMul1digit a b = 
		let mulDigit a c = ( a * c) in 
		let t1 = ( List.map (mulDigit a )  b) in  schoolbookMul1digitCarry t1  0  


let rec schoolBookMulRadix10E9_ a b accu scale =  
    match a with 
    | [] -> accu
	  | h :: t  ->  let p = schoolbookMul1digit h b  in
	                    let sum1 = addpInt accu ( scale @ p ) in 
	                    let sum  = carrypInt _MODULUS10E9 sum1  in 
                      schoolBookMulRadix10E9_   t  b sum  ( 0 :: scale )  
      
let rec schoolBookMulRadix10E9 a b =  
    match a, b with 
    | [],[] -> []
    | _, [] -> []
    | [], _ -> []
	  | _, _ ->  schoolBookMulRadix10E9_  a b [] []

 
let rec schoolbookMul1 a b = 
   let product = schoolBookMulRadix10E9 (convToRadix10E9 a) (convToRadix10E9 b) in
   convFromRadix10E9  product 
     
let schoolbookMul a b =  
   match a, b with
    | [],[] -> []
    | _, [] -> []
    | [], _ -> []
    | _, _  -> if ((isNegative a)=( isNegative b)) 
              then  schoolbookMul1 (numVal a) (numVal b)
              else  changeSign (schoolbookMul1  (numVal a) (numVal b));;
 
    
end 

