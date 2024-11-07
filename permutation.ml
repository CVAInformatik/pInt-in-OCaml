#print_depth 20;;
       
let rec 
			(*   insert a at pos in l  *)
      insertAux a pos l acc =  
      match l with 
      | [] -> if (pos = 0) 
              then List.rev ( a::acc) 
              else List.rev acc 
      | hd::tl -> if (pos = 0) 
               then insertAux a (pos - 1) l  (a::acc)
               else insertAux a (pos - 1) tl (hd::acc)
   and
			insert a pos l =   insertAux a  pos  l []
   and   
      permuteAux   li n   acc =
      match li with 
      |  [] -> acc
      | hd::tl  -> 
      let  p = (List.length acc) 
           in let pos = ( n mod (p+1))
           in let n1 = (n / (p+1)) 
           in permuteAux tl n1 (insert hd pos acc)
    and
	    (*   the nth permutation of li ( 0 is the null permutation *)
      permute rli n =
     	let li = List.rev rli in
      match li with       | []  -> []         (* empty list*)
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


(*   finding the n for a given permuation               *)


let rec find0 alist =  
        find0aux alist 0 
		and
		    find0aux alist count =
          match alist with
          | []  -> -1
          | hd::tl -> if (hd = 0 ) 
                      then  count
                      else find0aux tl (count+1)
 
 let rec remove0aux  alist acc =
 		 match alist with 
 		 | []  -> List.rev acc
 		 | hd::tl -> if (hd = 0)
 		             then remove0aux tl acc
 		             else remove0aux tl ((hd - 1)::acc)
 
 let remove0 alist = remove0aux alist []
 
 		               
 
let rec fac len res =
           if(len <= 0) 
           then res
           else fac (len-1) (len *res)


        
let rec findNaux alist ll offset  increment =
			 (*  let () = Printf.printf "findNaux:0 ll %d offset %d   increment %d \n" ll offset  increment  in  *)
        match alist with
        | [] -> ll
        | [0] -> ll
        | [0;1] -> if( 0 = (ll mod 2)) then ll else (ll + 1)
        | [1;0] -> if( 0 = (ll mod 2)) then (ll + 1 ) else ll 
        |  _    -> 
                let pos = find0 alist in
        				let alen = (List.length alist) in
        				let fac_alen_minusOne = (fac (alen - 1) 1)  in   
				(* )			  let () = Printf.printf "findNaux:1 pos %d  alen %d fac_alen_minusOne %d \n" pos alen fac_alen_minusOne  in  *)
        				if( ((pos*fac_alen_minusOne) + offset ) < ll) 
           					then if (increment = 0 ) 
           								then findNaux alist  ll   ll  (fac_alen_minusOne * alen)
           								else findNaux alist  ll  ( increment  + offset )   increment
					 					else findNaux (remove0 alist)  ((pos*fac_alen_minusOne) + offset) 0  0

let rec findN alist = findNaux alist  0 0  0


					 