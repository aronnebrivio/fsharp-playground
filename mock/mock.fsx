#r "FsCheck"
open FsCheck

(*
ESERCIZIO 1
===========
*)
// 1.1)
let rec dotProduct xl yl =
    match xl with
    | [] -> 0
    | x::xs -> match yl with
               | [] -> 0
               | y::ys -> (x*y) + dotProduct xs ys;;

let rec dotProduct2 xl yl =
    match xl,yl with
    | x,[] | [],x -> 0
    | x::xs,y::ys -> (x*y) + dotProduct2 xs ys;;

    // test
    let a =  [1;  3; -5 ];;
    let b =  [4; -2; -1 ];;
    dotProduct a b;;
    dotProduct2 a b;;

// 1.2)
let rec dotProductHO f xl yl =
    match xl,yl with
    | x,[] | [],x -> 0
    | x::xs,y::ys -> f x y + dotProductHO f xs ys;;

// ---> ANZI!!!
let dotProductHO2 f xl yl = List.fold (+) 0 (List.map2 f xl yl);;
let dotProductHO3 xl yl = List.fold2 (fun acc x y -> acc + x*y) 0 xl yl;;

let dotProd_prop (xl: int list) (yl: int list) =
    (xl.Length = yl.Length) ==> lazy (dotProduct2 xl yl = dotProductHO3 xl yl);;
do Check.Quick dotProd_prop;;

(*
ESERCIZIO 2
============
*)
(*
    2.1 Definire una funzione

       takeWhile :  ('a -> bool) -> 'a list -> 'a list

    che, applicata a un predicato p : ('a -> bool) e a una lista ls : 'a
    list, genera il **prefisso piu' lungo** di tutti gli elementi di ls che
    soddifano p.

    In altri termini, se ls contiene gli elementi x0, x1, x2, ...  va
    restituita la piu' lunga sottolista di ls che inizia da x0 e i cui
    elementi soddisfano il predicato p (notare che il prefisso puo' essere
    la lista vuota).
*)
let rec takeWhile f xl =
    match xl with
    | [] -> []
    | x::xs -> if f x then x :: takeWhile f xs
               else [];;

// test
let p1 = takeWhile (fun x -> x < 10) [ 1 .. 100];;
// p1 = [1; 2; 3; 4; 5; 6; 7; 8; 9]
let p2 = takeWhile (fun x -> x < 0) [ 1 .. 100];;
// p2 = []    
let p3 =  takeWhile (fun x -> x % 2 = 0) [2;4;5;6];;
// p3 = [2; 4]
let p4 = takeWhile (fun x -> x % 2 = 1) [2;4;5;6];;
// p4 = []

(*
    2.2 Definire la funzione duale

    dropWhile : ('a -> bool) -> 'a list -> 'a list

    che elimina elementi fino a che il predicato è vero e ritorna il
    residuo quando è falso.
*)
let rec dropWhile f xl =
    match xl with
    | [] -> []
    | x::xs -> if f x then dropWhile f xs
               else x :: xs;;
// test
let d1 = dropWhile (fun x -> x < 10) [ 1 .. 20];;
// let d2 =  [10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20]
let d2 = dropWhile (fun x -> x > 50) [ 1 .. 10];;
// d2 = [1..10]
let d3 =  dropWhile (fun x -> x % 2 = 0) [2;4;5;6];;
//val d3 : int list = [5; 6]
let d4 = dropWhile (fun x -> x % 2 = 1) [2;4;5;6];;
// d4 = [2; 4; 5; 6]

(*
ESERCIZIO 3
============
    3.1. Definire una funzione di divisione

     safeDiv : int option -> int option -> int option

    che prende due espressioni di tipo int option e restituisce il
    risultato solo se ambo gli arogmenti sono Some n e il divisore e'
    diverso da zero
*)
let safeDiv num den =
    match num,den with
    | _,Some 0 | _,None -> None
    | Some 0,_ -> Some 0
    | _,_ -> Some (num.Value/den.Value);;
// test
let d1 = safeDiv (Some (2 + 1)) (Some 4);;
// d1 = Some 0
let d2 = safeDiv (Some 3) (Some 0);;
// d2 = None
let d3 = safeDiv (Some 3) None;;
// d3 = None

(*
    3.2. Definire una funzione 

    optMapBinary   : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option

    che accetta una funzione binaria e due argomenti opzione e  
    applica la funzione agli argomenti se entrambi sono definiti, 
    altrimenti restituisce None.
*)
let optMapBinary f a b =
    match a,b with
    | None,_ | _,None -> None
    | _,_ -> Some (f a.Value b.Value) ;;
// test
let x1 =  optMapBinary (fun a  b ->  2*(a + b) ) (Some 2) (Some 3);;
// x1 = Some 10
let x2 =  optMapBinary (fun a  b ->  2*(a + b) )  None (Some -2);;
// x2 = None

(*
    3.3. Usare questa funzione per definire le  versioni "option" di addizione e
    moltiplicazione

       optPlus :  int option -> int option -> int option   
       optTimes : int option -> int option -> int option   
*)
let optPlus a b = optMapBinary (fun x y -> x+y) a b;;
let optTimes a b = optMapBinary (fun x y -> x*y) a b;;
// test
let y1 = optPlus (Some 3) (Some 1);;
// y1 = Some 4
let y2 = optPlus (Some 3) None;;
// y2  = None
let y4 =  optTimes  (safeDiv (Some 1) (Some 0)) (Some 1);;
// y4 = None

(*
ESERCIZIO 4
====================

Si consideri un linguaggio per la logica proposizionale

f ::= b | - f | f1 /\ f2 | f1 \/ f2

dove b è una constante booleana; poi abbiamo negazione, congiunzione e
disgiunzione.

    4.1 Si dia una dichiarazione di type appropriata per i tagged value di
    tipo "form"
*)
type form =
    | F of bool
    | Not of form
    | And of form*form
    | Or of form*form;;

(*
    4.2 Si definisca una funzione eval : form -> bool che data una
    formula ritorna il suo valore booleano.
*)
let rec eval f =
    match f with
    | F(e) -> e
    | Not(e) -> not (eval e)
    | And (e1, e2) -> (eval e1) && (eval e2)
    | Or (e1, e2) -> (eval e1) || (eval e2) ;;

let f1 = eval (And (F(true), Not (F(3 = 4))));;

(*
    4.3 Si definisca una funzione toString : form -> string che data  
    f : form, la renda come una stringa seguendo la grammatica informale
    (infissa, parentesizzata etc)
*)
let rec toString f =
    match f with
    | F(e) -> sprintf "%b" e
    | Not(e) -> sprintf "-%s" (toString e)
    | And (e1, e2) -> sprintf "(%s /\ %s)" (toString e1) (toString e2)
    | Or (e1, e2) -> sprintf "(%s \/ %s)" (toString e1) (toString e2) ;;
let f2 = toString (And (F(true), Not (F(3 = 4))));;

(*
    4.4 si scriva una funzione main : form -> string che componga le due
    sopra, ovvero valuti f ma la  presenti come stringa:

           " il risultato di valuatare <f>  è <res> "
*)
let main f = sprintf "il risultato da valutare di %s è %b" (toString f) (eval f);;
let f3 = main (And (F(true), Not (F(3 = 4))));;