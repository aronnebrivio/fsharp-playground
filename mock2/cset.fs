module FSet

type FSet<'a when 'a : equality> = FS of 'a list;;

let empty = FS [];;

let rec contains a (FS fs) =
    match fs with
    | [] -> false
    | x::xs -> if x=a then true else contains a (FS xs);;

let singleton x = FS [x];;

let add a (FS xs) = if contains a (FS xs) then FS xs else FS (a::xs);;

let rec union (FS s1) (FS s2) =
    match s2 with
    | [] -> FS s1
    | x::xs -> if contains x (FS s1) then union (FS s1) (FS xs)
               else union (FS (x::s1)) (FS xs);;

let rec ofList ls = 
    match ls with
    | [] -> empty
    | x::xs -> add x (ofList xs);;