module Queue
exception EmptyQueue

type Queue<'a> = {front: 'a list; rear: 'a list};;

(*

front =  primi elementi della coda
rear  =  tutti gli altri elementi, in ordine inverso

*)   


let empty = {front = []; rear = []};;
// val empty : Queue<'a>

let put y {front = fs; rear = rs} = {front = fs; rear = y::rs};;
// y va inserito in testa a rear
// val put : 'a -> Queue<'a> -> Queue<'a>

let rec get = function
// va tolto il primo elemento di front, trattando a parte il caso in cui front e' vuoto  
              | {front = []; rear = []} -> raise EmptyQueue  // coda vuota
              | {front = x::fs; rear = rs} ->
                    (x,{front = fs; rear = rs})
              | {front = []; rear = rs} ->  // rear non e' vuota
                    get {front = List.rev rs; rear = []};;

let isEmpty {front = fs; rear = rs} =
    match fs,rs with
    |[],[] -> true
    | _,_ -> false;;

let toList {front = fs; rear = rs} = fs @ List.rev rs;;

let ofList ls = {front = []; rear = List.rev ls};;

let put_list ls {front = fs; rear = rs} = {front = fs; rear = (List.rev ls @ rs)};;