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

(*
SEQUENZA DI FIBONACCI
=====================

Lo scopo dell'esercizio e' definire la sequenza infinita

   fibSeq : seq<int> 

dei numeri di Fibonacci.

i) Definire la funzione ricorsiva

   fibFrom :  int -> int -> seq<int>

che, dati due interi a e b, genera la sequenza di Fibonacci i cui primi due numeri sono a e b,
ossia la sequenza di interi  x0, x1, x2, ... tale che

    x0  =  a
    x1  =  b
    xn  =  x(n-2) + x(n-1) per ogni  n >= 2

Ad esempio, la lista dei primi 10 elementi di

  fibFrom 5 10

e'  

 [5; 10; 15; 25; 40; 65; 105; 170; 275; 445]

Suggerimento
^^^^^^^^^^^^

Notare che
  
   fibFrom a b 

e' la sequenza 

  a ; b ; a + b  ; b + (a+b) ;  (a+b) + (b+(a+b)) ; .... 

Osservare come e' fatta la sottosequenza che parte da b:

  b ; a + b  ; b + (a+b) ;  (a+b) + (b+(a+b)) ; .... 
*)
let rec fibFrom a b =
    seq {
        yield a
        yield! fibFrom b (a+b)
    };;
// test
fibFrom 5 10 |> Seq.take 10 |> Seq.toList;;

(*
ii) Definire la funzione

  fib : int -> int

che, dato n >= 0, calcola il numero di Fibonacci di indice n.

Poiche' i numeri di Fibonacci crescono esponenzialmente,
conviene rappresentarli usando il tipo uint64 invece di int;
le costanti di tipo  uint64 hanno suffisso UL 
(ad esempio, la costante 5 di tipo uint64 e' 5UL).
*)
let fib i =
    let fibseq = fibFrom 1 1
    Seq.item i fibseq;;
// test
fib 0 ;;   // 1
fib 1 ;;   // 1
fib 2 ;;   // 2
fib 3 ;;   // 3
fib 4 ;;   // 5
fib 10 ;;  // 89

(*
SEQUENZA DELLE SOMME DI UNA SEQUENZA
====================================

i) Definire la funzione
   
   sumSeq : seq<int> -> seq<int>

che, data una sequenza infinita di interi,

  n0, n1, n2, n3, .....

costruisce la sequenza delle somme 

 n0, n0 + n1, n0 + n1 + n2, n0 + n1 + n2 + n3, ....

Suggerimento
^^^^^^^^^^^^

Consideriamo la sottosequenza che parte da n0 + n1
   
  n0 + n1, n0 + n1 + n2, n0 + n1 + n2 + n3, ...
  
Tale sequenza puo' essere ottenuta come sumSeq della sequenza infinita

  n0 + n1, n2,  n3, ...


ii) Verificare che la lista dei primi 15 elementi della sequenza

    sumSeq nat 

e'

 [0; 1; 3; 6; 10; 15; 21; 28; 36; 45; 55; 66; 78; 91; 105]

*)
let rec sumSeq sq =
    seq {
        yield Seq.item 0 sq
        let first = ((Seq.item 0 sq)+(Seq.item 1 sq))
        let sq = seq {
            yield first
            yield! Seq.skip 2 sq
        }
        yield! sumSeq sq
    };;
// test
sumSeq nat |> Seq.take 15 |> Seq.toList;;
