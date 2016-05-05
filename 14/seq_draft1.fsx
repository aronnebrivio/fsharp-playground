

(****    SEQUENZE  (DRAFT)    ****)

(*

Una  *sequenza* e' una collezione  eventualmente infinita di elementi dello stesso tipo.
Il *tipo*  di una sequenza i cui elementi hanno tipo  T e'  seq<T>
(come al solito, T puo' essere anche un tipo polimorfo).

La notazione
            
    seq [ e0 ; e1 ; e2 ;  ... ]

rappresenta una sequenza i cui elementi sono e0, e1, e2, ...


Gli elementi di una sequenza vengono calcolati solamente quando richiesto (on demand),
e questo permette di lavorare su sequenze infinite.
Questa modalita' di valutazione, in cui la computazione effettiva degli elementi e' ritardata,
e' detta *lazy evaluation*.

Per i dettagli vedere il Cap. 11 del libro, in particolare le sezioni 11.1, 11.2, 11.6. 


Sequence expressions
^^^^^^^^^^^^^^^^^^^^
Un modo per definire una sequenza e' quello di  descrivere gli elementi della sequenza
tramite *sequence expression* (tipo particolare di *computation expression* ).
Una sequence expression genera uno o piu' elementi di una sequenza.

L'espressione

  seq{ 
     seq_expr0   // sequence expression 0
     seq_expr1   // sequence expression 1
     ...
     seq_exprn   // sequence expression n
     } 

definisce la sequenza ottenuta eseguendo in successione
le sequence expression seq_expr0, seq_expr1, ...  ,seq_exprn.

Le sequence expression descrivono il processo (eventualmente infinito) di generazione
degli elementi della sequenza; gli elementi vengono  effettivamente generati quando richiesto.

Esempi di sequence expression  
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(Vedi Tabella 11.2 del libro)

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

// Esempi


let sq1 = seq {
  yield 0  // yield genera un elemento
  yield 1
  yield 2
  yield 3
  } ;;

// sq1 : seq<int> definisce la sequenza seq [ 0; 1; 2; 3 ]

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

(*

  La funzione

     Seq.nth : int -> seq<'a> -> 'a

  estrae da una sequenza l'elemento di indice specificato.
  Data la sequenza

    sq = seq [ e0 ; e1 ; e2 ; .... ]   

   e un intero n >= 0, vale:

        Seq.nth  n sq   = e(n)  (elemento di sq di indice n)
  
  Se sa ha meno di n elementi, viene sollevata una eccezione.
 
*)

Seq.nth 0 sq2 ;;  // 100  (e0, primo elemento di sq2)
Seq.nth 2 sq2 ;;  // 2    (e2)
Seq.nth 5 sq2 ;;  // 200  (e5)
Seq.nth 9 sq2 ;;  // 3    (e9, ultimo elemento di sq2)
// Seq.nth 10 sq2  solleva eccezione
(*

 Esiste una analoga funzione per le liste

  List.nth  'a list -> int -> 'a list

in cui i due argomenti sono invertiti:

  List.nth ls n = elemento in posizione n di ls
  
*)

List.nth [0 .. 9 ] 0 ;; // 0 , primo elemento di [0 .. 9 ]
List.nth [0 .. 9 ] 9 ;; // 9 , ultimo elemento di [0 .. 9 ]
// List.nth [0 .. 9 ] 10 solleva una eccezione

(*

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

     cons : 'a ->  seq<'a> -> seq<'a>
   append : seq<'a> -> seq<'a> -> seq<'a>

analoghe alle omonime funzioni su liste.
Verificare che:

- la lista degli elementi nelle sequenza  'cons 100 sq2' e'
   [100; 100; 0; 1; 2; 3; 200; 0; 1; 2; 3]
- la lista degli elementi nelle sequenza 'append sq1 sq1' e'
   [0; 1; 2; 3; 0; 1; 2; 3]

ii) Usando le funzioni predefinite sulle sequenze, definire le funzioni

     head : seq<'a> -> 'a
     tail : seq<'a> -> seq<'a>

analoghe alle omonime funzioni su liste.     
Vericare che:

-  head sq2 =00
-  la lista degli elementi nella sequenza 'tail sq2' e'
   [0; 1; 2; 3; 200; 0; 1; 2; 3]

*)

// let cons x sq = ...



// let append sq1 sq2 = ...


// let head sq = ...

// let tail sq = ...



(*

Nota
^^^^

A differenza delle liste, sulle sequenze non e' possibile effettuare pattern-matching.
L'unico modo per decomporre una sequenza e' usare le funzioni definite sopra.



*)   




/////////////////////////////////////////////////////////////////////


(***  LAZY EVALUATION  VERSUS STRICT (O EAGER)  EVALUATION  ***)

(*

La valutazione *lazy (pigra)*  ritarda la computazione di una espressione
fino a quando il risultato deve essere utilizzato.

Le sequence expression sono valutate in modo lazy.
Durante l'esecuzione e' costruita solamenta la porzione finita della sequenza effettivamente usata.
Questo permette di lavorare su sequenze infinite.

*)   

// esempio di uso della lazy evaluation nella definizione di una sequenza

let sqLz = seq {
  yield 0 
  yield 1
  yield 2/0  // la valutazione di 2/0 solleva una eccezione 
  yield 3
  } ;;

// La definizione di seqlz non produce errori in quanto la definizione non genera alcun elemento

Seq.nth 0  sqLz ;; // 0
// nella valutazione viene generato solamente il primo elemento di sqLz

Seq.nth 1  sqLz ;; // 1
// nella valutazione vengono  generati solamente i primi due elementi di sqLz

(*

Verificare cosa succede calcolando

Seq.nth 2  sqLz ;; 
Seq.nth 3  sqLz ;;

In entrambi i casi viene valutata l'espressione 2/0, e questo solleva una eccezione.


*)   


(*

Strict (eager) evaluation
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Le espressioni F# viste finora sono valutate in modalita' *strict (eager, golosa)*,
che e' la modalita' usata nei linguaggi imperativi:

*  In una espressione della forma 

       expr1 Op expr2     

   le espressioni expr1 e expr2 sono valutate prima di applicare l'operatore Op   

*  Analogamente, in una applicazione di funzione della forma

      f t1 t2 .... tn

    gli argomenti t1 , t2 , ...  , tn sono valutati prima di applicare f.


Le liste sono costruite in modo strict (e non lazy).

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
  } ;;

e  

let listErr = [ 0; 1; 2/0; 3]

- la definizione di sqLz non produce errori, in quanto l'espressione 2/0 non e' valutata 
- la definizione di listErr solleva una eccezione dovuta alla valutazione di 2/0


Operatori con valutazione lazy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Esempi di operatori  valutati in F#  (e nei linguaggi imperativi) in modo  lazy
sono gli operatori booleani && (and) e || (or).

Per valutare l'espressione booleana
  
   B1 && B2 

- viene prima valutata l'espressione booelana B1;
- se B1 e' false, il risultato della valutazione e' false (e B2 non viene valutata);
- se B2 e' true, il risultato e' dato dalla valutazione di B2.

La valutazione di B1 || B2 e' analoga.

Quindi

 ( 10 < 0 ) &&  ( 2/0 > 0 ) ;;

e' false  (se && venisse valutato in modo strict, la valutazione dell'espressione
2/0 > 0  solleverebbe una eccezione).

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

Notare che l'espressione 
 
   seq [ 0; 1; 2/0; 3] ;;

solleva una eccezione in quanto, come visto sopra, la lista e' valutata in modo strict (e non lazy).

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

costruisce la sequenza infinita i cui elementi sono

   f(0) , f(1) , f(2) , ...

ossia la sequenza infinita 

Nota sulle sequenze infinite
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In una sequenza infinita sqInf, per ogni n >= 0 l'elemento

  Seq.nth n sqInf

e'  definito.
Al contrario, se sqFin e' una sequenza finita,
esiste almeno un intero k tale che la chiamata

    Seq.nth k sqFin

solleva una eccezione (ad esempio, se sqFin ha lunghezza n, basta porre k=n).    


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




// let nat =  Seq.initInfinite ...
// let nat1 =  Seq.initInfinite ...
// let nat2 =  Seq.initInfinite ...
// let even10 =  Seq.initInfinite  ...
// let sqTrue =   Seq.initInfinite  ...
// let sqTrueFalse =  Seq.initInfinite  ...



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

ii) Usando intFrom, definire la sequenza dei infinita dei numeri naturali 0, 1, 2, ...

iii) Usando intFrom, definire la sequenza infinita int10  degli elementi k >= -10.

Da int10, usando le funzioni sulle sequenze, estrarre la lista
 
  [-4; -3; -2; -1; 0; 1; 2; 3; 4]

*)

// i)
// genera sequenza n, n+1, n+2, ...
// let rec intFrom n =  ...


// ii)

// let naturali = ...

// iii)

// let int10 = ...

// [-4; -3; -2; -1; 0; 1; 2; 3; 4] 
// e' la lista dei primi 9 elementi della sottosequenza ottenuta da int10 saltando i primi 6 elementi


(*

Nota
^^^^

Supponiamo di definire la funzionne intFrom nel modo seguente:

  let rec intFromErr n  = cons  n ( intFromErr (n + 1) )
// intFromErr  : n:int -> seq<int>
  
Cosa succede definendo i numeri naturali come

  intFromErr 0  ?


*)   






(*

Esercizio
^^^^^^^^^^

Ridefinire le sequenze infinite nat1, nat2, even10, sqTrue, sqTrueFalse
senza usare Seq.initInfinite e usando la ricorsione.

Per nat1, nat2, even10 vanno  definite delle opportune funzioni generatrici (analoghe a intFrom).

*)

// genera sequenza infinita n, n+1, n+2, ... senza il numero 5
// let rec intFrom1 n = ...
 


// let rnat1 = ...

// genera sequenza infinita n, n+1, n+2, ... in cui 5 e' sostituito da -5
// let rec intFrom2 n = ....
  
// let rnat2 = ...


// genera sequenza infinita n, n+2, n+4, ....
// let rec intFrom3 n = ...
   
// let  reven10 = ...

// genera sequenza infinita true, true, true, true, ...
// let rec rsqTrue = ...
   
// genera sequenza infinita true, false, true, false, ...
// let rec rsqTrueFalse =  ...

