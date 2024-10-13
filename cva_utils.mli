
module type CVAutils  = 
  sig 
	val  explode_string    : string -> char list 	
	val  implode_char_list : char list -> string
	val  permute           : 'a list  -> int -> 'a list
	val  permutations      : 'a list -> int ->  'a list list 
	(*  Association List Functions *)
	exception KeyNotFound
	val  addAssociation     : 'a  -> 'b  -> ('a * 'b) list -> ('a * 'b) list
	val  findAssoc         : ('a -> 'a  -> bool) -> ( 'a * 'b) list -> 'a ->  'b
	val  removeAssoc       : ('a -> 'a  -> bool) -> ( 'a * 'b) list -> 'a -> ('a * 'b) list 
	val  genericCompare    : 'a -> 'a  -> bool 
	(*  Functional Array functions *)
	type 'a tree = 
	         |  Lf 
	         |  Br of 'a * 'a tree * 'a tree 

	exception ArrayFailure 
	val lookup  					: 'a tree -> int -> 'a
	val update            : 'a tree -> int -> 'a	 -> 'a tree 
end;;
