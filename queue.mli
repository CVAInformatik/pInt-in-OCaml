module type QUEUE =
sig
   type 'a queue = Queue of ('a list * 'a list)
   exception EmptyQueue
   val   qhd :  'a queue -> 'a
   val   enq :  'a queue -> 'a -> 'a queue
   val   deq :  'a queue -> 'a queue
   val   qempty : 'a queue -> bool
   val   qpushback : 'a queue -> 'a  -> 'a queue
end;;