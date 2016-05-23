module Stack
exception EmptyStack

type Stack<'a> = S of 'a list;;

let empty = S [];
let push x (S xs) = 
    let tmp = x::xs
    (S tmp);;
let pop (S xs) = 
    match xs with
    | [] -> raise EmptyStack
    | x::s -> (x, S s);;

let top (S xs) =
    match xs with
    | [] -> raise EmptyStack
    | x::s -> x;;

let size (S xs) = List.length xs;;