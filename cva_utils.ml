module CVAutils : CVAutils = struct 
       
let rec 
			(*   insert a at pos in l  *)
      insertAux a pos l acc =  if (pos = 0) then (( List.rev acc) @ a::l )      
                                            else insertAux a (pos -1) (List.tl l)  ((List.hd l )::acc)
   and
			insert a pos l =  if( pos = 0) then a::l  else insertAux a  pos  l []
   and   
      permuteAux   li n   acc =
      match li with 
      |  [] -> List.rev acc
      | hd::tl  -> 
      let  p = (List.length acc) 
           in let pos = ( n mod (p+1))
           in let n1 = (n / (p+1)) 
           in permuteAux tl n1 (insert hd pos acc)
    and
	    (*   the nth permutation of li ( 0 is the null permutation *)
      permute li n =
      match li with 
      | []  -> []         (* empty list*)
      | hd::[] ->  [hd]   (* one element *)
      | hd::tl ->  permuteAux tl n [hd] (* more than one element list*)

(* make all permutations of length s of li *)
let permutations li s = List.map (fun x -> permute li x  ) (List.init s (fun x -> x) );;
      
                   
let list_to_int lst = lst |> List.map string_of_int |> String.concat " " 

let rec iter  n x  = 
        let () =  Printf.printf "  %s  \n"  (list_to_int (permute n x ) )  
        in
        if (x = 0) then print_newline() else iter n (x-1)  ;;
        
let explode_string s = List.init (String.length s) (String.get s);;

let implode_char_list l = String.of_seq (List.to_seq l);;

let genericCompare a b =  a == b 

exception KeyNotFound 

let addAssociation  a b l = 
    match l with
    [] -> ( a , b) :: []
    | _  -> ( a , b) :: l

let rec findAssoc  f  l a =
    match l with
    [] -> raise KeyNotFound
    | lhd::ltl  -> 
          let (ax,bx ) = lhd in
             if( f ax a) 
             then bx 
             else findAssoc f ltl a  

let rec removeAssocAux  f  l a acc =
    match l with
    [] -> raise KeyNotFound
    | lhd::ltl  -> 
          let (ax,bx ) = lhd in
             if( f ax a) 
             then  ltl @ acc 
             else removeAssocAux f ltl a (lhd :: acc  )


let rec removeAssoc  f  l a =
    match l with
    [] -> raise KeyNotFound
    | _  -> removeAssocAux f l a []


exception ArrayFailure 
 
type 'a tree = 
	         |  Lf 
	         |  Br of 'a * 'a tree * 'a tree 

let rec lookup atree aindex =
 				match atree with
 				| Lf   -> raise ArrayFailure
 				| Br( v, tl1, tl2) ->
 						if( aindex = 1)
 				    then v
 				    else  if((aindex mod 2) == 0)
 				          then lookup tl1 (aindex/2)
 				          else lookup tl2 (aindex/2)
		
let rec update atree aindex a = 
 				match atree with
 				| Lf   -> if (aindex = 1)
 				          then Br( a, Lf,Lf)
 				          else raise ArrayFailure
 				| Br( v, tl1, tl2) ->
 						if( aindex = 1) 
 				    then Br( a, tl1, tl2)
 				    else  if((aindex mod 2) == 0)
 				          then Br( v, (update tl1 (aindex/2) a), tl2 )
 				          else Br( v, tl1, (update tl2 (aindex/2)  a))
			           
end