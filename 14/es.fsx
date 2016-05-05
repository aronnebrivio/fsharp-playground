(*
MAP
===

i) Definire la funzione ricorsiva higer-order

    map : ('a -> 'b) -> seq<'a> -> seq<'b>

che effettua la map di una sequenza infinita.

Piu' precisamente, data una sequenza infinita 

   sq = seq [ e0 ; e1 ; e2 ; .... ]   : seq<'a>

e una funzione f : 'a -> 'b, vale:

    map sq f   =  seq [ f(e0) ; f(e1) ; f(e2) ; .... ]   : seq<'b>

Notare che si *assume* che sq sia infinita. 
Questo implica che:

- la testa di sq e' sempre definita;
- la coda si sq e' a sua volta infinita,  quindi la chiamata ricorsiva sulla coda e' corretta
  (infatti, la coda di una sequenza infinita e' infinita, quindi la sequenza
   passata come argomento nella chiamata ricorsica verifica la condizione richiesta).
*)
let rec map f sq = seq {
    yield f (Seq.item 0 sq)
    yield! map f (Seq.skip 1 sq)
};;
// test
let sq = seq {1..10};;
map (fun x -> x*2) sq;;

(*
ii)  Applicare map alla sequenza infinita nat dei naturali 
per generare la sequenza infinita squares  dei quadrati dei naturali.
  
Verificare che la lista dei primi 15 elementi di squares e':

[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196; 225; 256; 289; 324; 361]
*)
let nat = Seq.initInfinite (fun x -> x);;
let squares = map (fun x -> x*x) nat;;
// test
squares |> Seq.take 15 |> Seq.toList;;

(*
iii)  Modificare la definizione di map in modo che funzioni correttamente 
anche su sequenze finite.

Rispetto a prima, occorre anche considerare il caso in cui la sequenza sq e' vuota
(infatti, dopo un certo numero di chiamate ricorsive su una sequenza finita, si genera la sequenza vuota).
Notare che sulle sequenze non e' possibile fare pattern matching.
Puo' essere utile usare la funzioni

   Seq.isEmpty : seq<'a> -> bool  // verifica se una  sequenza e' vuota
   Seq.empty   :  seq<'a>         // costante che definisce la sequenza vuota
*)
let rec map2 f sq = seq {
    yield! if Seq.isEmpty sq then Seq.empty
           else seq {
               yield f (Seq.item 0 sq)
               yield! map f (Seq.skip 1 sq)
           }
};;
// test
map2 (fun x->x*2) Seq.empty;;
map2 (fun x->x*2) (seq {1..10});;

(*
FILTER
======

i) Definire la funzione ricorsiva

 filter : ('a -> bool) -> seq<'a> -> seq<'a>

che, dato un predicato pred : 'a -> bool e una sequenza *infinita* sq,
genera la sequenza degli elementi di sq che verificano sq.

ii) Applicare filter alla sequenza infinita nat dei naturali
per generare la sequenza dei multipli di 3 (0, 3, 6, ...)

Verificare che la lista dei primi 20 elementi della sequenza generata e'

 [0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51; 54; 57]
*)
let rec filter f sq = seq {
    if f (Seq.item 0 sq) then yield Seq.item 0 sq
    yield! filter f (Seq.skip 1 sq)
};;
// test
filter (fun x -> x%3=0) nat |> Seq.take 20 |> Seq.toList;;