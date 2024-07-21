
#use "pInt.ml";;

let x1 = aToI "2941247924712947192479124771294791247917249";;
let x2 = aToI "8797963563428642";;
let x3 = aToI "123456789101112131415161718";;
let x4 = aToI "5";;
let x5 = aToI "123456789101112131415161718123456789101112131415161718";;
let x6 = aToI "123456789101112131415161718";;

      


let (q, m) = quotMod x2 x1 ;;

Printf.printf " q %s m %s \n" ( iToA q)  (iToA m) ;;

Printf.printf " q * x2 + m %s \n"  (iToA  (add (mul  q x2)  m)) ;;

Printf.printf " (q-1) * x2 + (m + x2)  %s \n"  (iToA  (add (mul  (sub q (fromInt 1)) x2
                                                            ) (add m x2)
                                                       )) ;;



