

(****    SEQUENZE      ****)

// ***  Cap. 11 del libro, in particolare le sezioni 11.1, 11.2, 11.6  ***

(*

Una  *sequenza* e' una collezione  eventualmente infinita di elementi dello stesso tipo.
Il *tipo*  di una sequenza i cui elementi hanno tipo  T e'  seq<T>
(il tipo  T puo' essere polimorfo).

La notazione
            
    seq [ e0 ; e1 ; e2 ;  ... ]

rappresenta una sequenza i cui elementi sono e0, e1, e2, ...




Sequence expressions
^^^^^^^^^^^^^^^^^^^^

Un modo per definire una sequenza e' quello di  descriverne gli elementi
tramite *sequence expression* (tipo particolare di *computation expression* ).

 - una sequence expression genera uno o piu' elementi di una sequenza.

-  L'espressione

      seq{ 
         seq_expr0   // sequence expression 0
         seq_expr1   // sequence expression 1
          ...
         seq_exprn   // sequence expression n
      } 

    definisce la sequenza ottenuta eseguendo in successione
    le sequence expression seq_expr0, seq_expr1, ...  ,seq_exprn.

     
Le sequence expression descrivono il processo (eventualmente infinito)
di generazione degli elementi della sequenza.
Gli elementi vengono calcolati solamente quando richiesto (on demand),
e questo permette di lavorare su sequenze infinite.
Questa modalita' di valutazione, in cui la computazione effettiva degli elementi e' ritardata,
e' detta *lazy evaluation*.



Esempi di sequence expression  
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

** Vedi Tabella 11.2 del libro **

o   yield  el   // generate element 

    Aggiunge alla sequenza l'elemento el.     
    Se si sta definendo una sequenza di tipo seq<T>, el deve avere tipo T.  

o   yield! sq  // generate sequence 

    Aggiunge alla sequenza tutti gli elementi della sequenza sq (concatenazione).
    Se la sequenza che si sta definendo ha tipo seq<T>, sq deve avere tipo seq<T>.  


o   let x = expr   // local declaration               
    seq_expr

    Analoga a let definition di F#.


o   if bool_expr then seq_expr   // filter       
 
    Se bool_expr e' vera, allora viene valutata la sequence expression seq_expr.


o   if bool_expr then seq_expr1  else  seq_expr2     // conditional

    Se bool_expr e' vera, allora viene valutata seq_expr1,
    altrimenti viene valutata seq_expr2.


Nota
^^^^

Non confondere le  sequence expression con le espressioni F#.
Ad esempio, nelle sequence expression e' possibile usare il costrutto
'if-then' senza else (filter), che non ha senso nelle espressioni F#.




*)   

// Esempi di def. di sequenze tramite sequence expression


let sq1 = seq {
  yield 0  // yield genera un elemento
  yield 1
  yield 2
  yield 3
  } ;;

// sq1 : seq<int>   e'  la sequenza seq [ 0; 1; 2; 3 ]

let sq2 = seq{
  yield 100
  yield! sq1  // yield! aggiunge tutti gli elementi di sq1
  yield 200
  yield! sq1
  }

(* sq2 = seq[ 100 ;   0 ;  1  ;  2  ; 3  ; 200 ;   0  ; 1  ; 2  ;  3  ] 
               e0    e1   e2    e3   e4     e5    e6   e7   e8    e9
                      ^^^^^^^^^^^^^^^^^^           ^^^^^^^^^^^^^^^^^^
                           sq1                           sq1      

*)

(***   FUNZIONI DEFINITE SU SEQUENZE   ***)

(*  Estrazione elemento di indice specificato
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  La funzione

     Seq.nth : int -> seq<'a> -> 'a

  estrae da una sequenza l'elemento di indice specificato.
  Data la sequenza

    sq = seq [ e0 ; e1 ; e2 ; .... ]   

   e un intero n >= 0, vale:

        Seq.nth  n sq   = e(n)  (elemento di sq di indice n)
  
  Se sq ha meno di n elementi, viene sollevata una eccezione.
 
*)

Seq.nth 0 sq2 ;;  // 100  (e0, primo elemento di sq2)
Seq.nth 2 sq2 ;;  // 2    (e2)
Seq.nth 5 sq2 ;;  // 200  (e5)
Seq.nth 9 sq2 ;;  // 3    (e9, ultimo elemento di sq2)
// Seq.nth 10 sq2  solleva eccezione

(*

 Esiste una analoga funzione per le liste:

  List.nth  'a list -> int -> 'a list

 Attenzione che l'ordine degli argomenti e' invertito:

  List.nth ls n = elemento in posizione n di ls
  
*)

List.nth [0 .. 9 ] 0 ;;  // 0 , primo elemento di [0 .. 9 ]
List.nth [0 .. 9 ] 9 ;;  // 9 , ultimo elemento di [0 .. 9 ]
// List.nth [0 .. 9 ] 10 solleva una eccezione

(*

Estrazione parte iniziale di una sequenza
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  La funzione

     Seq.take : int -> seq<'a> -> seq<'a>

  estrae la sottosequenza formata dai primi n elementi di una sequenza.
   
  Data la sequenza

    sq = seq [ e0 ; e1 ; e2 ; .... ]   

  e un intero n >= 0, vale:
  
     Seq.take n sq   =  seq [ e0 ; e1 ; ... ; seq(n-1) ] 

  
*)   

let sq3 = Seq.take 2 sq1 ;;
// sq3 = seq [ 0 ; 1]


(*

Estrazione parte finale di una sequenza
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  La funzione 
   
    Seq.skip : int -> seq<'a> -> seq<'a>

  estrae la sottosequenza ottenuta saltando i primi n elementi di una sequenza.

  Data la sequenza

    sq = seq [ e0 ; e1 ; e2 ; .... ]   

  e un intero n >= 0, vale:
  
     Seq.skip n sq   =  seq [ e(n) ; e(n+1) ; e(n+2) ; .... ] 

  
*)   

let sq4 = Seq.skip 2 sq2 ;; 
// sq4 = seq [1; 2; 3; 200; 0; 1; 2; 3]

 Seq.nth 0 sq4 ;; // 1
 Seq.nth 1 sq4 ;; // 2


(*

Trasforma sequenza in lista
^^^^^^^^^^^^^^^^^^^^^^^^^^^

La funzione

  Seq.toList : seq<'a> -> 'a list ;;
 
trasforma una sequenza *finita* in una lista.
Se applicata a una lista infinita, la computazione non termina.

*)

let l1 = Seq.toList sq1 ;;
// l1 : int list = [0; 1; 2; 3]

let l2 = Seq.toList sq2 ;;
// l2 : int list = [100; 0; 1; 2; 3; 200; 0; 1; 2; 3]


(*

Esercizio
^^^^^^^^^

i) Usando delle opportune sequence expression, definire la funzioni

     cons  :  'a ->  seq<'a> -> seq<'a>
   append  :  seq<'a> -> seq<'a> -> seq<'a>

analoghe alle omonime funzioni su liste.

Verificare che:

- la lista degli elementi nelle sequenza  'cons 100 sq2' e'
   [100; 100; 0; 1; 2; 3; 200; 0; 1; 2; 3]
- la lista degli elementi nelle sequenza 'append sq1 sq1' e'
   [0; 1; 2; 3; 0; 1; 2; 3]

ii) Usando le funzioni predefinite sulle sequenze, definire le funzioni

     head  :  seq<'a> -> 'a
     tail  :  seq<'a> -> seq<'a>

analoghe alle omonime funzioni su liste.     
Vericare che:

-  head sq2 = 100
-  la lista degli elementi nella sequenza 'tail sq2' e'
   [0; 1; 2; 3; 200; 0; 1; 2; 3]

*)

let cons x sq = seq{
    yield x
    yield! sq
    }
// cons :  x:'a -> sq:seq<'a> -> seq<'a>

let l3 = cons 100 sq2 |> Seq.toList 
// l3 : int list = [100; 100; 0; 1; 2; 3; 200; 0; 1; 2; 3]


let append sq1 sq2 = seq{
  yield! sq1
  yield! sq2
  }
// append : sq1:seq<'a> -> sq2:seq<'a> -> seq<'a>

let l4 = append sq1 sq1 |> Seq.toList
// l4 : int list = [0; 1; 2; 3; 0; 1; 2; 3]

let head sq = Seq.nth 0 sq ;;
// head : sq:seq<'a> -> 'a

let tail sq = Seq.skip 1 sq ;;  
//  tail : sq:seq<'a> -> seq<'a>

let h = head sq2 ;; // 100
let t = tail sq2 |> Seq.toList 
// t : int list = [0; 1; 2; 3; 200; 0; 1; 2; 3]

(*

Nota
^^^^

A differenza delle liste, sulle sequenze non e' possibile effettuare pattern-matching.
L'unico modo per decomporre una sequenza e' usare le funzioni definite sopra.



*)   




/////////////////////////////////////////////////////////////////////////////////


(***  LAZY EVALUATION  VERSUS STRICT (O EAGER)  EVALUATION  ***)

(*

La valutazione *lazy (pigra)*  ritarda la computazione di una espressione
fino a quando il risultato deve essere utilizzato.

Le sequence expression sono valutate in modo lazy.
Quando si definisce una sequenza, durante la computazione 
e' costruita solamenta la porzione finita della sequenza effettivamente usata.
Questo permette di lavorare su sequenza infinite.

*)   

// esempio su  lazy evaluation 

let sqLz = seq {
  yield 0 
  yield 1
  yield 2/0  // notare che la valutazione di 2/0 solleva una eccezione 
  yield 3
  } ;;

// La definizione di seqlz non produce errori in quanto la valutazione di sqLz non genera alcun elemento

Seq.nth 0  sqLz ;; // 0
// nella valutazione viene generato solamente il primo elemento di sqLz

Seq.nth 1  sqLz ;; // 1
// nella valutazione vengono  generati solamente i primi due elementi di sqLz

(*

Verificare cosa succede valutando

Seq.nth 2  sqLz ;; 
Seq.nth 3  sqLz ;;

In entrambi i casi viene valutata l'espressione 2/0, e questo solleva una eccezione.


*)   


(*

Strict (eager) evaluation
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Le espressioni F# viste finora sono valutate in modalita' *strict (eager, golosa, avida)*,
che e' la modalita' usata nei linguaggi imperativi:

*  per valutare una espressione della forma

       expr1 Op expr2     

   le espressioni expr1 e expr2 sono valutate prima di applicare l'operatore Op   

*  Analogamente, per valutare una applicazione di funzione della forma

      f t1 t2 .... tn

    gli argomenti t1 , t2 , ...  , tn sono valutati prima di applicare f.



Le liste sono valutate in modo strict.
Infatti, la lista

  [1;2] 

corrisponde al termine

 1 :: ( 2 :: [] )

L'operatore :: (cons) e' valutato in modo  strict,
quindi tutti gli elementi nella lista sono  valutati prima che venga costruita la lista.

Notare le differenza fra

let sqLz = seq {
  yield 0 
  yield 1
  yield 2/0  
  yield 3
  } 

e  

let listErr = [ 0; 1; 2/0; 3]

- la definizione di sqLz non produce errori  (l'espressione 2/0 non e' valutata) 
- la definizione di listErr solleva una eccezione in quanto tutti gli elementi della lista
  sono valutati e la valutazione di 2/0 solleva una eccezione.


Operatori con valutazione lazy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Esempi di operatori  valutati in F#  (e nei linguaggi imperativi) in modo  lazy
sono gli operatori booleani && (AND) e || (OR).

Per valutare l'espressione booleana
  
   B1 && B2 

- viene prima valutata l'espressione booelana B1;
- se B1 e' false, il risultato della valutazione e' false (e B2 non viene valutata);
- se B2 e' true, il risultato e' dato dalla valutazione di B2.

La valutazione di B1 || B2 e' analoga.

Quindi

 ( 10 < 0 ) &&  ( 2/0 > 0 ) ;;

e' false.
Se && venisse valutato in modo strict, la valutazione di 2/0 > 0  solleverebbe una eccezione.

Analogamente, il risultato della valutazione di

  ( 10 > 0 ) || ( 2/0 > 0 ) ;;

e' true  (l'espressione  2/0 > 0 non e' valutata).


Il costruttore seq
^^^^^^^^^^^^^^^^^^

E' possibile definire una sequenza applicando il costruttore seq a una lista.

Ad esempio

 seq [ 0 .. 10 ] ;;

definisce la sequenza di tipo seq<int>  contenente gli interi 0, 1, ... , 10.

 
 seq [ "asino" ; "bue" ; "cane" ] ;;

definisce la sequenza di tipo  seq<string> contenente le tre stringhe specificate.

L'uso del costruttore  seq e' utile per definire sequenze finite.
Notare che la lista e' valutata in modo strict, quindi tutti gli
elementi della sequenza sono costruiti.
Ad esempio
 
   seq [ 0; 1; 2/0; 3] ;;

solleva una eccezione.

Analogo a seq e' la funzione

   Seq.ofList : 'a list -> seq<'a> 

che trasforma una lista in una sequenza:

  Seq.ofList [ 0 .. 10 ] ;;
  Seq.ofList [ "asino" ; "bue" ; "cane" ] ;;

*)   


/////////////////////////////////////////////////////

(****  SEQUENZE INFINITE  ****)

(*

Per definire una sequenza infinita si puo' usare la funzione

   Seq.initInfinite : (int -> 'a) -> seq<'a>

Data una funzione  f: int -> 'a, la applicazione

   Seq.initInfinite f  

definisce la sequenza infinita di tipo  seq<'a> i cui elementi sono

   f(0) , f(1) , f(2) , ...



Nota sulle sequenze infinite
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In una sequenza infinita sqInf, per ogni n >= 0 l'elemento

  Seq.nth n sqInf

e'  definito (non esiste l'ultimo elemento di sqInf).
Al contrario, se sqFin e' una sequenza finita,
esiste almeno un intero k tale che la chiamata

    Seq.nth k sqFin

solleva una eccezione (ad esempio, se sqFin ha lunghezza n, basta porre k=n).    

Notare che:

-  La sequenza 'Seq.take n sq' e' sempre finita (ha n elementi)

-  Se sqInf e' infinita, anche la sequenza  'Seq.skip n sqInf' e' infinita.


Esercizio
^^^^^^^^^

Usando  Seq.initInfinite definire le seguenti sequenze infinite:

- nat : sequenza dei numeri naturali 0, 1, 2, ...
- nat1: sequenza dei numeri naturali senza il numero 5
- nat2: sequenza dei numeri naturali in cui il numero 5 e' sostituito da -5
- even10 : sequenza dei numeri pari n >= 10  
- sqTrue : sequenza costante true, true, true, ....
- sqTrueFalse: sequenza true, false, true, false, true, false, ...

Per ciascuna sequenza generare la lista dei primi 10 elementi.

*)   




let nat =   Seq.initInfinite (fun x -> x) ;;
let nat1 =  Seq.initInfinite (fun x -> if x < 5 then x else x + 1) ;;
let nat2 =  Seq.initInfinite (fun x -> if x <> 5 then x else -5 ) ;;
let even10 =  Seq.initInfinite (fun x -> 10 + 2 *x) ;; 
let sqTrue =   Seq.initInfinite (fun x -> true) ;;
let sqTrueFalse =  Seq.initInfinite (fun x ->  x % 2  = 0)  ;;

let lnat = nat |> Seq.take 10 |> Seq.toList ;;
// [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

let lnat1 = nat1 |> Seq.take 10 |> Seq.toList ;;
// [0; 1; 2; 3; 4; 6; 7; 8; 9; 10]

let lnat2 = nat2 |> Seq.take 10 |> Seq.toList ;;
// [0; 1; 2; 3; 4; -5 ; 6; 7; 8; 9]

let leven10 = even10  |> Seq.take 10 |> Seq.toList ;;
// [10; 12; 14; 16; 18; 20; 22; 24; 26; 28]

let  lsqTrue = sqTrue  |> Seq.take 10 |> Seq.toList ;;
// [true; true; true; true; true; true; true; true; true; true]

let  lsqTrueFalse = sqTrueFalse |> Seq.take 10 |> Seq.toList ;;
// [true; false; true; false; true; false; true; false; true; false]


(*

Uso della ricorsione
^^^^^^^^^^^^^^^^^^^^

E' possibile definire una sequenza infinita mediante ricorsione,
sfruttando il fatto che le sequence expression sono valutate in modo lazy.

Esercizio
^^^^^^^^^

i) Definire la funzione ricorsiva

    intFrom : int -> seq<int>

che, dato un intero n,  genera la sequenza infinita degli interi k >= n.    

ii) Usando intFrom, definire la sequenza infinita dei numeri naturali 0, 1, 2, ...

iii) Usando intFrom, definire la sequenza infinita int10  degli elementi k >= -10.

iv) Da int10, usando le funzioni sulle sequenze, estrarre la lista
 
  [-4; -3; -2; -1; 0; 1; 2; 3; 4]

*)

// i)
// genera sequenza n, n+1, n+2, ...
let rec intFrom n = seq { yield n  // primo elemeno della sequenza 
                          yield! intFrom (n + 1)  // elementi successivi n+1, n+2, ... 
                         } ;;
// intFrom : int -> seq<int>

// ii)

let naturali = intFrom 0

// iii)

let int10 = intFrom -10 ;;

// iv)

// [-4; -3; -2; -1; 0; 1; 2; 3; 4] 
// e' la lista dei primi 9 elementi della sottosequenza ottenuta da int10 saltando i primi 6 elementi

int10 |> Seq.skip 6 |> Seq.take 9 |> Seq.toList ;;




(*

Nota
^^^^

Supponiamo di definire la funzione intFrom nel modo seguente (errato!):

  let rec intFromErr n  = cons  n ( intFromErr (n + 1) )

// intFromErr  : n:int -> seq<int>
  
A differenza di prima, la valutazione di

  intFromErr 0

non termina, in quanto  cons  e' valutato un modo strict.

Quindi:

 intFromErr 0   =  cons  0 ( intFromErr (0 + 1) )                     // valuto   0 + 1
                =  cons  0 ( intFromErr 1 )                           // valuto  intFromErr 1 
                =  cons  0 ( cons 1  intFromErr (1 + 1) )             // valuto  1 + 1 
                =  cons  0 ( cons 1  intFromErr 2 )                   // valuto  intFromErr 2 
                =  cons  0 ( cons 1  ( cons 2 intFromErr (2 + 1)) )   // valuto  2 + 1
                =  cons  0 ( cons 1  ( cons 2 intFromErr 3) )         // valuto  intFromErr 3
                .....      

La computazione non termina e produce stack overflow

*)   






(*

Esercizio
^^^^^^^^^^

Ridefinire le sequenze infinite nat1, nat2, even10, sqTrue, sqTrueFalse usando
sequence expression con ricorsione.


Per nat1, nat2, even10 vanno  definite delle opportune funzioni generatrici (analoghe a intFrom).

*)

// genera sequenza infinita n, n+1, n+2, ... senza il numero 5
let rec intFrom1 n =
    seq{ if n <> 5 then yield n 
         yield!  intFrom1 (n + 1) 
         } ;;
// notare l'uso di if-then come filtro (esclude 5 dalla sequenza)


let rnat1 = intFrom1 0 ;;

// genera sequenza infinita n, n+1, n+2, ... in cui 5 e' sostituito da -5
let rec intFrom2 n =
    seq{ if n <> 5 then yield n else yield -5
         yield!  intFrom2 (n + 1) 
         } ;;


let rnat2 = intFrom2 0 ;;


// genera sequenza infinita n, n+2, n+4, ....
let rec intFrom3 n =
    seq{ yield n 
         yield!  intFrom3 (n + 2) 
         } ;;

let  reven10 = intFrom3 10 ;;

// genera sequenza infinita true, true, true, true, ...
let rec rsqTrue =
    seq { yield true
          yield! rsqTrue } ;;

// genera sequenza infinita true, false, true, false, ...
let rec rsqTrueFalse = seq { yield true
                             yield false
                             yield! rsqTrueFalse } ;                    

(*

Esercizio: map
^^^^^^^^^^^^^^

i) Definire la funzione ricorsiva higher-order

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


ii)  Applicare map alla sequenza infinita nat dei naturali 
per generare la sequenza infinita squares  dei quadrati dei naturali.
  
Verificare che la lista dei primi 15 elementi di squares e':

[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196; 225; 256; 289; 324; 361]

iii)  Modificare la definizione di map in modo che funzioni correttamente 
anche su sequenze finite.

Rispetto a prima, occorre anche considerare il caso in cui la sequenza sq e' vuota
(infatti, dopo un certo numero di chiamate ricorsive su una sequenza finita, si genera la sequenza vuota).
Notare che sulle sequenze non e' possibile fare pattern matching.
Puo' essere utile usare la funzione

   Seq.isEmpty : seq<'a> -> bool  // verifica se una  sequenza e' vuota


*)   

// i)


//  map : ('a -> 'b) -> seq<'a> -> seq<'b>
// Si assume che la sequenza sq sia infinita
// let rec map f sq =


 
// ii) 

let squares = map (fun x -> x * x) nat ;;
// seq [0; 1; 4; 9; ...] (sequenza dei quadrati dei  numeri naturali)

// lista dei primi 15 quadrati
let listSq = squares |> Seq.take 15 |> Seq.toList ;;
// [0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196]


// iii) map che funziona anche su sequenze finite


// let rec map1 f sq =


// se la sequenza sq e' vuota,  (map1 f sq) definisce la sequenza vuota 

// sequenza  degli elementi   5^2, 6^2,  ... ,  10^2
map1 (fun x -> x * x) ( seq[ 5 .. 10 ] ) ;;

(*

Esempio: filter   
^^^^^^^^^^^^^^^


i) Definire la funzione ricorsiva higher-order

 filter : ('a -> bool) -> seq<'a> -> seq<'a>

che, dato un predicato pred : 'a -> bool e una sequenza *infinita* sq,
genera la sequenza degli elementi di sq che verificano sq.

ii) Applicare filter alla sequenza infinita nat dei naturali
per generare la sequenza dei multipli di 3 (0, 3, 6, ...)

Verificare che la lista dei primi 20 elementi della sequenza generata e'

 [0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51; 54; 57]


*)   

// i)

// filter : ('a -> bool) -> seq<'a> -> seq<'a>
// Si assume che la sequenza sq sia infinita
// let rec filter pred sq =
 
// ii)

// primi 20 multipli di 3
// filter (...) nat |> Seq.take 20 |> Seq.toList ;;

/////////////////////////////////////////////////////////////////////////


(***  APPROFONDIMENTI SU LAZY EVALUATION ***)

(*

In alcuni casi la lazy e' preferibile alla valutazione eager.
Un esempio gia' visto e' con gli operatori booleani.

Altri esempi:

-  Sappiamo che 0 * n vale 0,  qualunque sia n.
   La valutazione di

      0 *  Expr

   e' piu' efficiente se effettuata in modalita' lazy
   (calcolare Expr potrebbe essere computazionalmente pesante)                         

- Analgo discorso per la funzione

  first : 'a -> 'b -> 'a

  che, applicata a due argomenti,  restituisce il primo:

    let first x y = x ;;  // il valore di y non e' usato in first

  Anche in questo caso sarebbe la valutazione lazy sarebeb piu' efficiente,
  dato che e' inutile valutare il secondo argomento

Vediamo attraverso un esempio come si puo' forzare la lazy evaluation di una espressione


*)

(* Esempio: lazyAnd
   ^^^^^^^^^^^^^^^^

Definire una funzione lazyAnd tale che

   lazyAnd b1  b2

valuta l'espressione 'b1 AND b2' in modalita' lazy (come operatore &&)   

*)

// Soluzione errata

let lazyAndErr b1 b2 =
  match b1 with  // valuto b1
    | false -> false  // se b1 e' false, bi AND b2 e' false ...
    | _  ->  b2 // ... altrimenti b1 AND b2 e' il valore di b2


(*

Notare cosa succede valutando

  lazyAndErr ( 10 > 0 )  ( 10 /0 > 0 )

** Problema **

Gli argomenti di  lazyAndErr sono valutati subito
(e questo indipendentemente da come la funzione  lazyAndErr  e' definita)!



** Soluzione **

Una possibile soluzione e' sfruttare il fatto che la valutazione
di una espressione funzionale

  fun x -> Expr

definisce una closure e  Expr non viene valutata. 

Ad esempio, la valutazione di

  fun x -> 1/0 ;;

produce

 val it : x:'a -> int = <fun:clo@2>
 // definisce una closure e  1/0 non e'valutata


Si puo' sfruttare questo principio per "nascondere" una espressione in una funzione costante

Ad esempio,  l'espressione booleana

    10/0 > 0

puo' essere rappresentata dalla funzione costante  

    fun x ->  (10/0 > 0)
//  val it : x:'a -> bool = <fun:clo@8-6>

Chiamiamo tale funzione la *rappresentazione lazy*  dell'espressione booleana 10/0 > 0

La funzione costante   fun x ->  (10/0 > 0)    ha tipo polimorfo  'a -> bool.
Dato che non interessa avere un dominio generico, e' consuetudine  fissare come dominio
il tipo *unit*, che ha come unico valore ().
Quindi:

- la *rappresentazione lazy* di una espressione booleana B e' una funzione costante LB avente:
  o  tipo unit -> bool 
  o  valore B  

Per  "estrarre" l'espressione B dalla sua rappresentazione lazy LB
e' sufficiente applicare la funzione LB  al valore () . 

La funzione lazyAnd e' specificata come segue:

* Siano LB1 e LB2 due espressioni tali che:

   LB1 :  unit -> bool     e' la rappresentazione lazy dell'espressione booleana B1
   LB2 :  unit -> bool     e' la rappresentazione lazy dell'espressione booleana B2

  e sia

    LB   =   lazyAnd LB1 LB2  

  Allora
   
    LB :  unit -> bool     e' la rappresentazione lazy di ( B1 && B2 )

La funzione lazyAnd ha  tipo:

   (unit -> bool) ->  (unit -> bool) ->  (unit -> bool) 


Rispetto alla soluzione precedente, la valutazione di LB1 e LB2 non produce effetti indesiderati,
in quanto LB1 e LB2 sono funzioni.





*)   

// rappresentazione lazy di true e false

let lazyTrue  =  (fun x ->  true)  : (unit -> bool)   // rappresentazione lazy di true
let lazyFalse =  (fun x ->  false) : (unit -> bool)   // rappresentazione lazy di false


// let lazyAnd lb1  lb2 =
 
// Esempi

let e1 = (fun x ->   5 > 0) :   (unit -> bool) 
let e2 = (fun x ->  10 < 0)  :  (unit -> bool) 
let e3 = (fun x ->  2/0 > 0) :  (unit -> bool) 


let v1 =  lazyAnd e1 e1 ;;  // (5 > 0) && (5 > 0)
v1 () ;; // true

let v2 = lazyAnd e1 e2 ;;  // (5 > 0) && (10 < 0)
v2 () ;;  // false

let v3 =  lazyAnd e2 e3 ;;  // (10 < 0) &&  (2/0 > 0)
v3 () ;; // false

let v4 = lazyAnd  e1 ( lazyAnd e2 e3 );;  // (5 > 0) && ( (10 < 0) &&  (2/0 > 0))
v4 () ;; // false

let v5 =  lazyAnd lazyTrue ( lazyAnd lazyFalse e3 );;  // true  && ( false &&  (2/0 > 0))
v5 () ;; // false

let v6 =  lazyAnd lazyTrue e3 ;;  // true && (2/0 > 0)

(*

Notare che v6 viene definito, ma v6() solleva una eccezione.
Invece

   lazyAnd  e3  lazyTrue     //  (2/0 > 0)  && true 

solleva subito un'eccezione

*)

(*

La funzione predefinita lazy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Per la rappresentazione lazy di una espressione si puo' usare la funzione predefinita lazy,

   https://msdn.microsoft.com/en-us/library/dd233247.aspx

Applicando lazy ha una espressione Expr di tipo T, viene costruito
un valore di tipo Lazy<T>  che definisce la rappresentazione lazy di Expr.

Ad esempio, l'espressione

 lazy ( 10 + 2 ) ;;
 
ha tipo Lazy<int> e  rappresenta l'espressione 10 + 2.

Dato un termine t di tipo  Lazy<T> , per accedere al suo  contenuto si usa Force:

-   t.Force   restituisce  una  funzione di tipo unit -> T
-   applicando il valore (), si ha un il valore di tipo T contenuto in t.

Esempi:
------

let t1 =  lazy ( 10 +  2 ) ;;  // e1 e' la rappresentazione lazy dell'espressione 10 +  2
// t1 : Lazy<int> = Value is not created

t1.Force ;;
// val it :  unit -> int

t1.Force ();;
// val it : int = 12 (valore dell'espressione 10 + 2)
 

Esercizio
^^^^^^^^^

Riscrivere la funzione lazyAnd usando  usando la funzione lazy per la rappresentazione lazy di espressioni booleane.

Quindi, lazyAnd deve avere tipo

   Lazy<bool> ->  Lazy<bool> -> Lazy<bool>

*)
   






(*

Uso della lazy evaluation nella definizione di proprieta' per QuickCheck
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Le property della forma

    Cond1 ==> Cond2     // se vale Cond1 allora valuta Cond2

sono valutate in modalita' strict.

Ntare che se Cond1 e' falsa, allora Cond1 ==> Cond2 e' vera,
quindi in teoria non c'e' bisogno di valutare Cond2



*)
#r "FsCheck";;
open FsCheck;;

(*

Esempio 1
^^^^^^^^^

Vogliamo verificare con  QuickCheck la seguente proprieta' invariante inv
dell'inverso di un intero n:

-  se n e' diverso da 0,  allora   n * (1/n) = 1

*)   

let inv n =
  n <> 0 ==>  ( n * 1/n = 1 ) ;;  // non dimenticare le parentesi
//val inv :int -> Property


(*

L'esecuzione 

  Check.Quick inv ;;

solleva una eccezione

 ...  exception_ System.DivideByZeroException: Division by zero ...

Infatti, poiche'  ==> e' valutato in modalita' strict, se n vale zero
viene subito calcolato il valore di

   0 * 1/0 = 0

prima di applicare ==>.

Per risolvere il problema, occorra forzare la lazy evaluation, in modo che la condizione
( n * 1/n = 1 ) venga valutato solo se necessario (ossia, se n e' diverso da 0)


*)


let invLz n =
  n <> 0 ==>  lazy ( n * 1/n = 1 ) ;;
//val invL :int -> Property

Check.Quick invLz ;;

(*

Ok, passed 100 tests.
val it : unit = ()

*)



(*

Esempio 2
^^^^^^^^^

Verificare con  QuickCheck la seguente proprieta' invariante invMin  sulle liste:

- se la lista ls non e' vuota, allora il minimo elemento di ls coincide con la testa  di List.sort
   

Ricordarsi che la proprieta' va definita su una lista con tipo non-polimorfo (int list, string list, ...)

*)   

let invMin (ls : int list) =
  ls <> []  ==>  lazy (  List.min ls  =     (List.sort ls |>  List.head) )  ;;
// val invMin : int list -> Property

Check.Quick invMin ;;

// anche in questo caso, se non si forza la valutazione lazy si ha errore in esecuzione (lista vuota)




