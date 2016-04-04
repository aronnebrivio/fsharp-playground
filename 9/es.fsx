(*
    ESERCIZIO 2

    2.1) Definire la funzione ricorsiva map tale che, 
    data una funzione f e una lista ls aventi tipo

      f : 'a -> 'b      ls : 'a list

    il valore di
     
       map f ls

    e' la lista di tipo 'b list ottenuta applicando a ogni elemento x di ls la funzione f.
*)
let f x = x + 1;;

let rec map f ls = 
    match ls with
    | [] -> []
    | [x] -> [f(x)]
    | x::xs -> f(x) :: map f xs;;

    // test
    let ls = [1;2;3;4;5;6;7;8;9];;