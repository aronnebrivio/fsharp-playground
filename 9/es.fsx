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

(*
ESERCIZIO 3  (FILTER)   
=====================

    3.1) Definire la funzione ricorsiva filter tale che, 
     data una funzione pred (predicato)  e una lista ls  aventi tipo

      pred : 'a -> bool     ls :  'a list

    il  valore di

       filter pred ls
       
    e' la lista di tipo 'a list contenente gli elementi di ls che verificano pred.
    La lista risultante contiene quindi gli elementi x di ls tali che pred x e' true  (pred funge da filtro).

    Il tipo di filter e':

     filter: ('a -> bool) -> 'a list -> 'a list

    ed e'  una funzione higher-order
*)
let rec filter pred ls =
    match ls with
    | [] -> []
    | [x] -> if pred(x) then [x]
             else []
    | x::xs -> if pred(x) then x :: filter pred xs
                else filter pred xs;;

(*
    3.2) Usando fiter, definire la funzione

       mult3 : int  -> int list

    che costruisce la lista dei multipli di 3 compresi fra 1 e n
    (applicare in modo opportuno filter sulla lista [1 .. n]).
*)

let mult3 x =
    let ls = [1 .. x]
    let p x = x%3 = 0
    filter p ls;;

(*
    3.2B
    QuickCheck
    ^^^^^^^^^^
    Definire e  verificare con QuickCheck le seguenti  proprieta' di filter:

    i) prop_filter pred (ls : int list) 

    filter e List.filter calcolano gli stessi valori.

    ii)  prop_filter_len pred (ls :int list)

    La lista   'filter pred ls' non puo' essere piu' lunga della lista ls.
*)
// i)
let prop_filter pred (ls: int list) =
    filter pred ls = List.filter pred ls;;
do Check.Quick prop_filter;;

// ii)
let prop_filter_len pred (ls: int list) =
    let l1 = filter pred ls
    List.length l1 <= List.length ls;;
do Check.Quick prop_filter_len;;

(*
ESERCIZIO 4 (VARIANTE DI FILTER)    
================================

    4.1) Definire la funzione ricorsiva filter1 analoga a filter in cui pero'

       filter1 pred ls = ( lsTrue, lsFalse )

    dove:

    - lsTrue    contiene gli elementi di ls che verificano pred
    - lsFalse   contiene gli elementi di ls che non verificano pred

    Il tipo di filter1 e':

       ('a -> bool) -> 'a list -> 'a list * 'a list
*)
let rec filter1 pred ls =
    match ls with
    | [] -> ([],[])
    | [x] -> if pred(x) then ([x],[])
             else ([],[x])
    | x::xs -> 
        let (l, r) = filter1 pred xs
        if pred(x) then (x :: l, r)
                else (l, x :: r);;


(*
    4.2) Usando filter1 e le definizioni nell'esercizio precedente, costruire le coppie di liste

    p1 =  ( [3; 6; 9; 12; 15; 18] , [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16; 17; 19; 20] )
     //  ( multipli di 3 , non-multipli di 3 ) 
*)
    // test
    let p x = x%3 = 0;;
    let p1 = filter1 p [1 .. 20];;

(*
    4.3) Usando filter1, definire la funzione
      
       multNonmult : int -> int list * int list

    che, dato un intero n, partiziona la lista [1 .. n] 
    nella coppia di liste  

        ( multipli di 3 , non-multipli di 3 ) 

    Ad esempio:

       multNonmult 16 =   ( [3; 6; 9; 12; 15] , [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16] )
*)

let multNonmult x =
    let ls = [1 .. x]
    let p x = x%3 = 0
    filter1 p ls;;

    //test
    multNonmult 16;; //( [3; 6; 9; 12; 15] , [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16] )

(*
    4.3B)

    QuickCheck
    ^^^^^^^^^^

    Definire e  verificare con QuickCheck le seguenti  proprieta' di filter1:

    i) prop_filter1_len pred (ls : int list)

    Sia  (xs,ys) il risultato di 'filter1 pred ls'.
    Allora concatenando xs e ys si ottiene una lista avente la stessa lunghezza di ls.

    ii) t prop_filter1_app pred (ls :int list)

    Sia  (xs,ys) il risultato di 'filter1 pred ls'.
    Allora, concatenando xs e ys si ottiene una lista avente gli stessi elementi di ls.
*)
// i)
let prop_filter1_len pred (ls : int list) =
    let (xs,ys) = filter1 pred ls
    List.length ls = List.length (xs @ ys);;
do Check.Quick prop_filter1_len;;