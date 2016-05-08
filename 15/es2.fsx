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

(*
ii) Definire la funzione ricorsiva

    sieve : seq<int> -> seq<int>

che esegue   il crivello di Eratostene su una sequenza infinita di interi.
Piu' precisamente, data una sequenza sq di interi, va eseguito il seguente processo infinito:

-----------------------------------------------------------------------------------------
| 1. x0  :=  primo elemento di sq                                                        |  
| 2. Cancella da sq i multipli propri di x0                                              | 
|    (ossia, x0 rimane in sq e sono cancellati da sq i multipli di x0 maggiori di x0)    |
| 3. Ripeti il processo da 1                                                             |
----------------------------------------------------------------------------------------- 

Al passo 2 usare la funzione sift.

*)
let rec sieve sq =
    seq {
        let first = Seq.item 0 sq
        if first=0 || first=1 then
            yield! sieve (Seq.skip 1 sq)
        else
            yield first
            yield! sieve (sift first sq)
    };;
//test
sieve nat;;

(*
iii) Sia nat2 la sequenza infinita degli interi n >= 2.
La sequenza infinita primes dei numeri primi puo' essere costruita applicando sieve a nat2.

Verificare che la lista dei primi 10 numeri primi e'
 
 [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
*)
let nat2 = Seq.skip 2 (Seq.initInfinite (fun x -> x));;
sieve nat2;;

(*
iv) L'implementazione fornita del Crivello di Eratostene e' poco efficiente,
in quanto le sequenze usate vengono continuamente rigenerate da capo.

Per migliorare l'efficienza si puo' usare il meccanismo di caching 
che memorizza gli elementi della sequenza che sono gia' stati costruiti.
In questo modo si evita di calcolare piu' volte lo stesso elemento di una sequenza.

Procedere come segue:

a)  Definire la versione cached della funzione sift:

let siftC a sq = Seq.cache  ( sift a sq )   
// siftC esegue sift e usa  cache per memorizzare la sequenza ottenuta

b) Definire la funzione sieveC, analoga a sieve, in cui pero' si usa  siftC al posto di sift 
   e sieveC nelle chiamate ricorsive.


c) Definire la sequenza cached dei numeri primi usando sieveC:

let primesC = Seq.cache (sieveC nat2)

Si puo' verificare sperimentalmente che l'implementazione con caching e' piu' efficiente
(provare a generare una lista di numeri primi grande usando le due versioni).

Notare inoltre che eseguendo piu' volte la ricerca di un numero primo, ad esempio

Seq.nth 1000 E.primesC ;;
//val it : int = 7927
Seq.nth 1000 E.primesC ;;
//val it : int = 7927

la seconda richiesta ha una risposta immediata (il risultato e' stato memorizzato dopo la prima computazione).
*)
let siftC a sq = Seq.cache (sift a sq);;
let rec sieveC sq =
    seq {
        let first = Seq.item 0 sq
        if first=0 || first=1 then
            yield! sieveC (Seq.skip 1 sq)
        else
            yield first
            yield! sieveC (siftC first sq)
    };;
let primesC = Seq.cache (sieveC nat2);;

(*
ELENCO DEI FILE IN UNA DIRECTORY
=================================

Definite la funzione

  allFiles : string -> seq<string>

che dato il path di una directory (una stringa) costruisce la sequenza
dei file contenuti nella directory specificata e, ricorsivamente, 
in tutte le sue sottodirectory.

Usare le seguenti funzioni di F#:

* Directory.GetFiles  : string -> string [] 
 
  Restituisce un array di string contenente i nomi dei file nella directory specificata.
  
* Directory.GetDirectories : string -> string [] 
 
  Restituisce un array di string contenente i nomi delle directory nella directory specificata.
 
Per usare queste funzioni occorre aprire System.IO:

  open System.IO ;;

Notare che gli array possono essere visti come sequenze,
quindi e' possible applicare agli array le funzioni definite sulle sequenze.

Occorre inoltre usare la funzione Seq.collect.


Esempio:

let myDir = "/home/bob/tmp"  // definizione di un path 
let fileSeq = allFiles myDir

Seq.nth 10 fileSeq ;;  // file in posizione 10 in fileSeq
// val it : string = "/home/bob/tmp/prova/xx.txt"


Calcolare il numero dei file nella sequenza ottenuta.
*)
open System.IO;;

let rec allFiles path =
    seq {
        yield! Directory.GetFiles path
        yield! Seq.collect Directory.GetFiles (Directory.GetDirectories path)
    };;
// test
let down = allFiles "/home/aronne/Scaricati";;
let downc = Seq.length down;;

(*
VALUTAZIONE  LAZY DEL PRODOTTO
==============================

Definire una funzione prod che calcola il prodotto di due interi in modo lazy.
Quindi, se il primo argomento e' zero, la funzione restituisce zero, senza valutare
il secondo argomento.

Seguire lo stesso procedimento usato nella funzione lazyAnd
*)
let prod a b = 
    let lzy = lazy(a*b)
    lzy.Force ();;

(*
APPROSIMAZIONE FUNZIONE ESPONENZIALE MEDIANTE SERIE DI TAYLOR
============================================================
(** Esercizio difficile, vedi es. 11.7 del libro  **)


La funzione esponenziale puo' essere approssimata usando la serie di Taylor (ST) definita come segue: 

 e^x  =   1/0!  +   x^1 / 0!  +  x^2 / 2! + ... +  x^k/ k! + .....       (ST)

Vogliamo definire una funzione che calcola e^x con una precisione fissata (un numero decimale)
usando (ST).


i) Sia t(x,k) il termine  della serie (ST)  di posto k, ossia:

     t(x,k) :=  x^k / k!


Definire la funzione 

    f : float -> int -> float

tale che

   f x k = t(x,k)     dove  x:float e k:int
       


Suggerimento
^^^^^^^^^^^

Definire f per induzione su k; osservare come t(x,k)  puo' essere ottenuto da t(x,k-1).
*)