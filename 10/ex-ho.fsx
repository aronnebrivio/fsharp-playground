#r "FsCheck"
open FsCheck

let rec fold f e = function
    | x::xs -> fold f (f e x) xs
    | [] -> e ;;

// 1. Definire mediante foldBack le seguenti funzioni  e controllare equivalenza con FsCheck
// 1.1 List.concat
let concat xs ys = List.foldBack (fun x ls -> x::ls) xs ys;;
// 1.2 List.filter
let filter f xs = List.foldBack (fun y ys -> if f y then y::ys else ys) xs [];;