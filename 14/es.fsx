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