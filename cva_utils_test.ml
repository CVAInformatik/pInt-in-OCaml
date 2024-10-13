

#use "cva_utils.mli" 
#use "cva_utils.ml"

open CVAutils ;;
(*
let a = addAssociation  1 "test1" [] ;;
let a = addAssociation  2 "test2" a ;;
let a = addAssociation  3 "test3" a ;;

let simpleFind = findAssoc genericCompare ;;
let simpleRemove = removeAssoc genericCompare ;;

let b = simpleFind a 1 ;;
let c = simpleRemove a 2 ;;
let d = simpleFind c 1 ;;
 let d = simpleFind c 2 ;;*)

let c = update CVAutils.Lf 1 "abc";;
let c = update c 2 "abcd";;
let c = update c 3 "abcde";;
let c = update c 4 "abcdef";;
let s = lookup c 2 ;;
