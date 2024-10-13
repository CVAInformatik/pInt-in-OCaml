

#use "queue.mli";;
#use "queue.ml";;

open QUEUE ;;

let c = Queue([],[]);;

let c = enq c "abdc" ;;
let c = enq c "efgh" ;;
let c = enq c "ijkl" ;;

qempty c ;;

qhd c ;;

let c = deq c ;;
qhd c ;;
qempty c ;;
let c = deq c ;;
qhd c ;;
qempty c ;;
let c = deq c ;;
qempty c ;;


let d = Queue ([],[]);;
let d = enq d 1 ;;
let d = enq d 2 ;;
let d = enq d 3 ;;
let d = enq d 4 ;;
qhd d ;;
let d = deq d ;;
qhd d ;;
let d = enq d 5 ;;
qhd d ;;
let d = deq d ;;
qhd d ;;
let d = deq d ;;
qhd d ;;
let d = deq d ;;
qhd d ;;
let d = deq d ;;
qempty d;;