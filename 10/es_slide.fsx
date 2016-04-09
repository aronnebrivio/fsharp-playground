let rec filter p = function
    | [] -> []
    | x::xs -> if p x then x :: filter p xs
                else filter p xs;;

let rec inter xs ys =
    match ys with
    | [] -> []
    | y::yq -> if (List.exists ((=)y) xs) then y :: inter xs yq
                else inter xs yq;;

let disjoint xs ys = if (inter xs ys)=[] then true
                     else false; 