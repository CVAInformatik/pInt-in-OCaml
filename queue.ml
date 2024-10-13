module QUEUE : QUEUE = struct
   type 'a queue = Queue of ('a list * 'a list)
   exception EmptyQueue

   let norm q =
   match q with
   | Queue ([],[]) -> Queue ([],[])
   | Queue( [], a ) -> Queue ( List.rev a, [])
   | Queue ( a, b)  -> Queue ( a, b) ;;
   
   let qhd q  =
   match q with
   | Queue ([],[]) -> raise EmptyQueue
   | Queue (ah::at, _ ) -> ah;;

   let enq q x =
   match q with
   | Queue ( a, b ) -> norm ( Queue(a, x::b ));;

   let deq q =
   match q with
   | Queue ([],[]) -> raise EmptyQueue
   | Queue( ah::at, b ) -> norm ( Queue ( at, b));;

	 let qempty q = 
	 match q with
   | Queue ([] , []) -> true
   | Queue (_::_ , _)-> false

   let qpushback q x =
   match q with
   | Queue ( a, b ) -> Queue(x::a, b );;

end