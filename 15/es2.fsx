(*
CRIVELLO DI ERATOSTENE
======================

Scopo dell'esercizio e'  definire la sequenza infinita dei numeri primi
implementando la procedura nota come "crivello di Eratostene".
Tale procedura  lavora su insiemi infiniti che possono essere rappresentati
da sequenze (non occorre porre un limite superiore ai numeri da considerare, 
come avviene nelle implementazioni basate su linguaggi imperativi).


i) Definire la funzione 

    sift : int -> seq<int> -> seq<int>
  
che, dati un intero a > 0 e  una sequenza infinita sq di interi,
restituisce la sequenza (infinita) di interi  ottenuta eliminando da sq i multipli di a.

Date le definizioni

let nat = sequenza infinita dei numeri naturali 0, 1, 2, 3, ...
let sq1 = sift 2 nat
let sq2 = sift 3 nat

verificare che:

- La lista dei primi 10 elementi di sq1 e'

    [1; 3; 5; 7; 9; 11; 13; 15; 17; 19]

- La lista dei primi 15 elementi di sq2 e'

    [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16; 17; 19; 20; 22]

*)
let nat = Seq.initInfinite (fun x -> x);;

let sift i sq = Seq.filter (fun x -> x%i>0) sq;;

//test
sift 2 nat |> Seq.take 15 |> Seq.toList;;
sift 3 nat |> Seq.take 15 |> Seq.toList;;
