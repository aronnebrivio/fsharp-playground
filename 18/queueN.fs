module Queue

exception EmptyQueue

type Queue<'a> =  Q of  'a list;;

let empty = Q [];;

let get  (Q qs) =
  match qs with
    | [] ->   raise EmptyQueue
    | x :: xs ->  (x , Q xs);;

let rec put y (Q qs) =
  match qs with
    | [] -> Q [y]
    | x :: xs ->
      let (Q ys) = put y ( Q xs)
      Q( x :: ys);;

let isEmpty (Q qs) = 
    match qs with
    | [] -> true
    | _ -> false;;

let toList (Q qs) = qs;;

let rec ofList ls =
    match ls with
    | [] -> empty
    | _ -> (Q ls);;

let put_list ls (Q qs) = 
    let tmp = qs@ls
    (Q tmp);;