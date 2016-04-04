#r "FsCheck" ;;
open FsCheck ;;

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
    let ls = [1;2;3;4;5;6;7;8;9;10];;
    map f ls;;

(*
    2.2) Sia l1 la lista contenente i numeri da 1 a 10.

    Applicando map a una opportuna funzione f e alla lista l1 costruire le seguenti liste
    (in entrambi i casi, scrivere la funzione f come funzione anonima):

    l2 =  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100] // quadrati dei primi 10 numeri

    l3 = [(1, "dispari"); (2, "pari"); (3, "dispari"); (4, "pari"); (5, "dispari"); (6, "pari"); 
          (7, "dispari"); (8, "pari"); (9, "dispari"); (10, "pari")]
*)
let pow x = x*x;;
let pardis x = 
    match x%2 with
    | 0 -> (x, "pari")
    | _ -> (x, "dispari");;

    //test
    let l2 = map pow ls;;
    let l3 = map pardis ls;;

(*
    2.3) Consideriamo la lista

    let names = [ ("Mario", "Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")] ;;

    Applicando map a una opportuna funzione e alla lista names, costruire la lista
     
    names1 =  ["Dott. Mario Rossi"; "Dott. Anna Maria Verdi"; "Dott. Giuseppe Di Gennaro"]
*)
let names = [ ("Mario", "Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")] ;;

let dottname (n,s) = "Dott. " + n + " " + s;;

    //test
    map dottname names;;

(*
    2.3B) Definire e  verificare con QuickCheck le seguenti  proprieta' di map:


    i)    prop_map f (ls : int list)

    map e List.map calcolano gli stessi valori
    (piu' precisamente, le applicazioni 'map f ls' e 'List.map f ls' producono la stessa lista).

    ii)   prop_map_pres_len f (ls :int list)

    La lista  'map f ls' ha la stessa lunghezza di ls
    (per calcolare la lunghezza di una lista si puo' usare List.length)
*)

// i)
let prop_map f (ls: int list) =
    List.map f ls = map f ls;;
do Check.Quick prop_map;;

// ii)
let prop_map_pres_len f (ls : int list) =
    let l1 = map f ls
    let l2 = List.map f ls
    List.length l1 = List.length l2;;
do Check.Quick prop_map_pres_len;;