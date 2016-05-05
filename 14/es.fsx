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