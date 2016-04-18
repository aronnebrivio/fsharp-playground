// i)
let rec insert x ls =
    match ls with
    | [] -> [x]
    | y::ys -> if x<y then x::ls else y::(insert x ys);;

let rec isort ls =
    match ls with
    | [] -> []
    | x::xs -> insert x (isort xs);;


// ii)
let rec insertBy less x ls =
    match ls with
    | [] -> [x]
    | y::ys -> if less x y then x::ls else y::(insert x ys);;

let rec isortBy less ls =
    match ls with
    | [] -> []
    | x::xs -> insertBy less x (isortBy less xs);;


// iii)
let voti = [("Bianchi",1000,26);("Verdi",1004,25);("Rossi",1002,25);("Bianchini",1010,32)];;

let lsa = isortBy (fun (n1,_,_) (n2,_,_) -> n1 < n2) voti;;
let lsb = isortBy (fun (_,m1,_) (_,m2,_) -> m1 < m2) voti;;

// iv)
#r "FsCheck"
open FsCheck

// trasformare la less in una funzione che restituisca 1 o -1
let toInt less x y = if less x y then -1 else 1;;

let prop_sort less (xs: int list) =
    List.sortWith (toInt less) xs = isortBy less xs;;
do Check.Quick prop_sort;;