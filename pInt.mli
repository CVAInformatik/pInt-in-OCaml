
module type PINT  = 
    sig 
			type pInt
  	  val aToI : string -> pInt 
   	  val iToA : pInt -> string
    	val add : pInt ->  pInt -> pInt
    	val double : pInt -> pInt  (* a -> a+a *)
    	val addInt : pInt ->  int -> pInt
    	val sub : pInt ->  pInt -> pInt  (* sub a b -> (a - b) *)
    	val subInt : pInt ->  int -> pInt
    	val changeSign : pInt ->  pInt 
    	val numVal : pInt -> pInt        (* absolute/numerical value of a *)
    	val mul : pInt ->  pInt -> pInt
    	val schoolbookMul : pInt ->  pInt -> pInt  
    	val div2 : pInt -> pInt 
    	val isEq : pInt -> pInt -> bool
    	val isLt : pInt -> pInt -> bool  (*  isLt a b  ->  a > b *)
    	val isNegative : pInt -> bool 
    	val isZero : pInt -> bool 
    	val isEven : pInt -> bool 
    	val power  : pInt -> pInt -> pInt (*  power a b -> a^b *)
    	val gCD : pInt -> pInt -> (pInt * pInt * pInt) (* gCD a b ->  ( ba, bb, gcd(a,b) such that ba*a + bb*b = gcd(a,b) *)
    	val quotRem: pInt -> pInt -> (pInt * pInt) (* quotRem p n -> (q,r)  such that p*q + r = n *)
    	val fromInt : int -> pInt 
    	val toInt  :  pInt -> int
    	val modMul : pInt ->pInt -> pInt -> pInt   (* ModMul m a b ->   (a x b) mod m  *)
    	val modPow : pInt ->pInt -> pInt -> pInt   (* ModPow m a b ->   (a ^ b) mod m  *)
    	val jacobi : pInt -> pInt -> int           (* jacobi p n ->     (n/p): -1,0,-1 *)
    	val randInit: unit  -> unit
    	val rand   : pInt -> pInt                  (* rand a  returns a random number in the range [0..a[ *)
    	val millerRabin : int -> pInt -> bool      (* is pInt argument likely to be prime ? the int argument is number of iterations *)
    	val tonelliShanks : pInt -> pInt -> pInt   (* tonneliShanks m x  returns y such that x = (y*y) mod m, raises NotaSquare exception *)
    	val facult : int -> pInt 
    	val permutation : 'a list  -> pInt -> 'a list
    	val findN : int list -> pInt 
    	exception UnexpectedChar
    	exception DivisionByZero
    	exception NotaSquare
    	exception InvalidArgument
    end;;