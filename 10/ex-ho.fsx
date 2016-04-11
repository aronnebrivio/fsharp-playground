#r "FsCheck"
open FsCheck

let rec fold f e = function
    | x::xs -> fold f (f e x) xs
    | [] -> e ;;

let rec foldBack f xs e =
  match xs with
    | [] -> e
    | y :: ys -> f y (foldBack f ys e);;

// 1. Definire mediante foldBack le seguenti funzioni  e controllare equivalenza con FsCheck
// 1.1 List.concat
let concat xs ys = List.foldBack (fun x ls -> x::ls) xs ys;;
// 1.2 List.filter
let filter f xs = List.foldBack (fun y ys -> if f y then y::ys else ys) xs [];;

// check
let concat_prop xs ys =
    concat xs ys = List.concat [xs;ys];;
do Check.Quick concat_prop;;

let filter_prop f xs =
    filter f xs = List.filter f xs;;
do Check.Quick concat_prop;;

(* 2.

Vi sono dei casi in cui l'operazione che intendiamo implementare opera
su una list **NON* vuota

es: List.min : ('a list -> 'a) when 'a : comparison

In questo caso si può usare la funzione List.reduceBack, che è simile a
foldBack, ma lavora solo su liste non vuote e su un tipo solo T:

 List.reduceBack : ('T -> 'T -> 'T) -> 'T list -> 'T

"Applies a function to each element of the collection, threading an
accumulator argument through the computation.  If the input function
is f and the elements are i0...iN, then this function computes
             f i0 (...(f iN-1 iN))."

   *)

let minlist xs = List.reduceBack min xs

// 2.1 Definire per ricorsione esplicita la funzione reduceBack
let rec reduceBack f xs =
  match xs with
    | [] -> raise(System.ArgumentException("Lista vuota!!!"))
    | [y] -> y
    | y :: ys -> f y (reduceBack f ys);;

let rec reduceBack2 f xs =
  match xs with
    | [] -> raise(System.ArgumentException("Lista vuota!!!"))
    | y :: ys -> 
        if xs.Length=1 then y
        else f y (reduceBack2 f ys);;

let last xs = reduceBack2 (fun ys y -> y) xs;;