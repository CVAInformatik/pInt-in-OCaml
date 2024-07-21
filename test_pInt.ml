



let _MODULUS8 = 100000000
let _MODULUS4 = 10000

(* the string below can NOT be used as format string in Printf.sprintf for some advanced reasons, 
I dont quite understand, you will have to type it in yourself. *)
let _FSTRING =  "%08i" 

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

exception UnexpectedChar

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
     | [] -> ""
     |  hd::tl -> if (hd < 0) then "-"
                  else if (hd > 0 ) then ""
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
  
let rec mkCompleteStringFrombInt bintList = List.fold_left (^) "" bintList 

let intsToString  i  = rev (Printf.sprintf "%08i" i )

let mkBintPositive i = 		if( i>= 0) then i else ~- i

let rec mkStringFrombIntAux bint = List.map intsToString (List.map  mkBintPositive  bint)

let rec mkStringFrombInt bint =
      match bint with
      | [] -> (Printf.sprintf "0" ) ::[]
      | _::_ -> mkStringFrombIntAux bint


let iToA pInt = (getSign pInt) ^ (dropLeading0es (rev (mkCompleteStringFrombInt (mkStringFrombInt pInt))))


let rec dropLeading0aux a = 
     match a with
     | [] -> []
     | 0 :: tl  -> dropLeading0aux tl
     | hd :: tl  ->  hd :: tl 

let rec dropLeading0 a = List.rev (dropLeading0aux (List.rev a))

let rec normalizebIntAux2 carry sign  a modulus =
     match a with
     | [] -> []
		 |  0 :: tl ->  ( 0 + carry) :: ( normalizebIntAux2 0 sign tl modulus) 
		 | hd :: tl when  (( sign> 0 ) && ( (hd + carry) < 0 )) -> ( hd + carry + modulus) :: ( normalizebIntAux2 ~-1 sign tl modulus) 
		 | hd :: tl when  (( sign< 0 ) && ( (hd + carry) > 0 )) -> ( hd + carry - modulus) :: ( normalizebIntAux2   1 sign tl modulus) 
		 | hd :: tl ->  ( hd +carry) :: ( normalizebIntAux2 0 sign tl modulus) 

(* adjust signs in so they match the sign of s *)
let rec normalizebIntAux1 s a modulus =
     match a with
     | [] -> []
		 | hd :: tl -> normalizebIntAux2 0 s a modulus

(* drop MSDs, which are 0 *)
let rec normalizebIntAux a modulus =
     match a with
     | [] -> []
     | 0 :: tl  -> normalizebIntAux tl modulus
     | hd :: tl -> normalizebIntAux1 hd (List.rev a) modulus


(* drop MSDs, which are 0 and adjust signs, so they are consistent *)
let rec normalizebInt a modulus = normalizebIntAux (List.rev a) modulus 

let rec carrybIntAux a carry modulus =
    match a with
    | []  -> if carry <> 0  then [carry] else []
    | hd :: tl when hd+carry >= modulus -> ( carry + hd - modulus):: (carrybIntAux tl 1 modulus)
    | hd :: tl when hd+carry <= ~-modulus -> ( carry + hd + modulus):: ( carrybIntAux tl ~-1 modulus)
    | hd :: tl -> ( carry + hd ):: ( carrybIntAux tl 0 modulus)

let rec carrybInt a modulus  =  carrybIntAux a 0 modulus

let rec addbInt  a  b   =
   match  a, b   with
   | [], _ -> b
   | _, [] -> a
   | hd :: tl, hd2 ::tl2  -> (hd + hd2) :: (addbInt tl tl2)


let addPintM8 i1 i2 = dropLeading0 (normalizebInt (carrybInt (addbInt i1 i2) _MODULUS8 ) _MODULUS8)

(* for use in multiplikation *)
let addPintM4 i1 i2 = dropLeading0 (normalizebInt (carrybInt (addbInt i1 i2) _MODULUS4 ) _MODULUS4)

let addPint i1 i2 = addPintM8  i1 i2

let changeSign i1 = List.map mkpIntInvert i1

let subPint i1 i2 = dropLeading0 (normalizebInt (carrybInt (addbInt i1 (changeSign i2)) _MODULUS8 ) _MODULUS8)

(* multiplication   *)
 						
let  isEven l1 = 
      match l1 with 
      | []  -> false 
      | hd::_ -> if (0 =  ( hd land 1 )	) then true else false

let rec div2Aux2 l1 lc =
      match l1 with
      | [] ->   []
      | hd::tl ->  (hd + ( (List.hd lc) * _MODULUS8))/2 ::  ( div2Aux2  (List.tl l1) (List.tl lc))
            
let div2Aux1 l1 =   div2Aux2 l1  (0::(List.map (fun x -> x land 1) l1))            
        
let div2_ l1 =  List.rev (dropLeading0aux (div2Aux1 (List.rev l1  )))
      
let div2 l1 =  
	if (isNegative l1)  
		then changeSign ( div2_ (changeSign l1))
		else ( div_2 l1)
	
      
let rec rpMulAux i1 i2 acc = 
			match i1 with
			| []  -> acc
			| _::_	-> if( isEven i1) 
			           then rpMulAux (div2 i1 )  (addPint i2 i2) acc 
			           else rpMulAux (div2 i1 )  (addPint i2 i2) (addPint acc  i2)
 								
 								
let rec rpMul i1 i2 = rpMulAux i1 i2 []

let a = aToI("797354927346482684582548164");;
let b = aToI("797354927346482684582548163");;
let aneg = aToI("-797354927346482684582548164");;
let bneg = changeSign( b);;

iToA( addPint a b);;
iToA( addPint a bneg);;
addPint a aneg;;
iToA( addPint a aneg);;

#use "pInt.mli";;
#use "pInt.ml";;

module Atest = (Pint : PINT);;

let a = Pint.aToI("797354927346482684582548164");;
let b = Pint.aToI("797354927346482684582548163");;
let aneg = Pint.aToI("-797354927346482684582548164");;
let bneg = Pint.changeSign( b);;

let c = Pint.mulPint a b;;

Pint.iToA c ;;



    
