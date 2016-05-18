(**** MODULI (PARTE 2)  QUEUE   ****)

(**

L'obiettivo e' definire e implementare il modulo Queue 
che modella una coda generica (modulo parametrico).

Le operazioni che deve fornire il modulo sono:

- put: inserisce un elemento nella coda

- get: toglie un elemento dalla coda;
       se la coda e' vuota, solleva una eccezione
   
La coda va gestita in modalita' LIFO (Last In First Out):
il prossimo elemento da prelevare e' l'ultimo elemento che e' stato inserito.

Una invocazione di put o get non deve modificare la code su cui opera
(code immutabili); va pero'  restituita la coda ottenuta dopo l'operazione

Quindi, put e get devono avere tipi:

*  put   : 'a -> Queue<'a> -> Queue<'a>

   put el q =  coda ottenuta inserendo el in q 

*  get : Queue<'a> -> 'a * Queue<'a>

   get q = (el,q1) dove el e' l'elemento tolto da q e
                   q1 e' la coda ottenuta       

**)    


// -------------  Queue.fsi   --------------------

// signature

module Queue  // nome del modulo
type Queue<'a>  // coda parametrica

val empty : Queue<'a>  
val put   : 'a -> Queue<'a> -> Queue<'a>
val get   : Queue<'a> -> 'a * Queue<'a>

exception EmptyQueue
// eccezione da sollevare se si esegue get su una coda vuota

//--------   END  Queue.fsi  --------------


(*

Naive implementation (file QueueN.fs)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Implementiamo la coda con una lista.
Il primo elemento della lista e' il primo elemento della coda (prossimo elemento da estrarre dalla coda);
quindi, i nuovi elementi vanno inseriti in coda alla lista.

- La coda vuota e' rappresentata dalla lista vuota

- L'operazione get e' immediata (richiede l'accesso alla testa della lista).
  Se la lista e' vuota va sollevata l'eccezione EmptyQueue.

- L'operazione put e' invece costosa in quanto occorre attraversare tutta la lista

NOTE
====

1) Ricordare che [HR, pag. 151]:


*  the type specified as hidden in the signature
   must be a *tagged type* or a *record type*,
   so we have to add a tag in the type definition

Quindi in  QueueN.fs  *non* si puo' definire

type Queue<'a> = 'a list  //  type abbreviation

che genera un errore in compilazione:

--------------------------------------------------------------------
The type definitions in the signature and implementation are not
compatible because an abbreviation is being hidden by a signature.
The abbreviation must be visible to other CLI languages.
Consider making the abbreviation visible in the signature.
----------------------------------------------------------------------

Occorre usare un costruttore (tag value), ad esempio:       

type Queue<'a> =  Q of  'a list


2) E' possibile anche rappresentare la coda con una lista
in cui il primo elemento della lista e' l'ultimo della coda
(quindi, il prossimo elemento da estrarre e' l'ultimo elemento della coda).

Con questa rappresentazione, l'operazione put e' implementata in tempo costante
(inserimento in testa), mentre get richiede l'attraversamento di tutta la lista.



*)   

// --------- QueueN.fs -----------


// Naive implementation of Queue

module Queue

exception EmptyQueue

type Queue<'a> =  Q of  'a list  // *deve* essere un tagged value

(*
  il primo elemento della lista e' il primo elemento della coda
  (prossimo elemento da togliere)
 *) 
 


let empty = Q [] // coda vuota
// val empty : Queue<'a>

let get  (Q qs) =
// l'elemento da togliere e' il primo della lista qs
  match qs with
    | [] ->   raise EmptyQueue
    | x :: xs ->  (x , Q xs)
// val get : Queue<'a> -> 'a * Queue<'a>


let rec put y (Q qs) =
// y va inserito dopo l'ultimo elemento della lista qs
  match qs with
    | [] -> Q [y]
    | x :: xs ->
      let (Q ys) = put y ( Q xs)
      Q( x :: ys)
// val val put : 'a -> Queue<'a> -> Queue<'a>



// --------- END  QueueN.fs -----------

(***  Uso modulo Queue
      ^^^^^^^^^^^^^^^^

Per verificare la correttezza dell'implementazione,  scrivere  uno script file 
test-queue.fsx in cui si compiono le seguenti operazioni:

a) Definire la coda q1  ottenuta ponendo nella coda vuota gli elementi 3, 5, 10.
b) Estrarre un elemento da q1 e sia q2 la coda ottenuta.
   Notare che l'elemento estratto  e' 3.
c) Estrarre un elemento da q2 e sia q3 la coda ottenuta.
   Notare che l'elemento estratto  e' 5.
d) Aggiungere alla coda q3  gli elementi 15, 20 e sia q4 la coda ottenuta.
   Estrarre tre elementi da q3 e sia q5 la coda ottenuta.
   Notare che gli elementi estratti sono 10, 15 e  20.
   Verificare che q5 e' la coda vuota (provare a estrarre elemento da q5).

NOTE
====

1) Il file test-queue.fsx deve iniziare con

------
#r "QueueN.dll"   // oppure mettere il path completo
open Queue        

------

L'istruzione 'open Queue' si puo' omettere, ma in questo caso
vanno usati i nomi qualificati  (es, 'Queue.put' invece di 'put').

In casi come questi e' piu' comodo aprire il modulo con open.
Se pero' si aprono troppi moduli, puo' crearsi ambiguita' quando
una funzione e'  definita in piu' di un modulo (namespace pollution).
In tal caso vanno usati i nomi qualificati.

2) Eseguendo i test in  test-queue.fsx, l'implementazione della coda e' nascosta.
Se qualcosa non funziona, conviene  fare i test in QueueN.fs,
in modo che sia visibile la struttura della lista che implementa la coda.  

***)     

//----------   test-queue.fsx ------

#r "QueueN.dll";;
open Queue;;

// a)
let q1 = put 3 empty |> put 5 |> put 10 

//b)
let (x1,q2) = get q1
// val x1 : int = 3

//c)
let (x2,q3) = get q2
// val x2 : int = 5

//d)
let q4 = q3 |> put 15 |>  put 20
let (y1, q4_1) = get q4
// val y1 : 10
let (y2, q4_2) = get q4_1
// val y2 : 15
let (y3, q5) = get q4_2
// val y3 : 20

//----------   END test-queue.fsx ------

//////////////////////////////////////////////////////////////////77


(**  A more efficient implementation (file QueueP.fs)
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Nell'implementazione naive, l'inserimento di un elemento e' costosa.
Si puo' migliorare usando la rappresentazione proposta da L.C. Paulson (vedi HR Sez. 7.5).

Una coda qs e' rappresentata da una coppia di liste front e rear:

- front e' la lista dei primi elementi della coda
- rear  e' la lista dei rimanenti elementi della coda, elencati pero' in ordine inverso.

Una coda ammette in generale piu' rappresentazioni.
Ad esempio, la coda

   a ; b ; c           // a e' il primo elemento della coda, c l'ultimo

ammette  quattro rappresentazioni:

1) front = [a ; b ; c]   rear = []

2) front = [a ; b]       rear = [c]

3) front = [a]           rear = [c ; b]

4) front = []            rear = [c ; b ; a]

L'operazione put puo' essere implementata in modo efficiente;
infatti, basta aggiungere un elemento in testa alla lista rear

Ad esempio, se la coda  

   a ; b ; c

e' rappresentata come

front = [a ; b]    rear = [c]

aggiungendo l'elemento d si ottiene

front = [a ; b]    rear = [d; c]
 
che rappresenta la coda

 a ; b ; c ; d


L'operazione get si implementa prendendo il primo elemento di front.
Occorre prestare attenzione al caso in cui la lista front sia vuota.
Ad esempio supponiamo di voler eseguire get sulla coda 

   a ; b ; c 

e che la coda sia rappresentata come
 
 front = []                rear = [c ; b ; a]   // (#)

Conviene travasare tutti gli elementi da rear e front;
ovviamente, poiche' la coda deve essere la stessa,
gli elementi vanno posti in ordine inverso:

 front = [a; b; c]          rear = []          // (##)
 
Notare che (#) e (##) rappresentano la stessa coda.
A questo punto, poiche' la lista front non e' vuota, get si implementa
come nel caso precedente (si elimina la testa della lista front).

i) Implementare  nel file QueueP.fs il modulo Queue rappresentando una
coda con la coppia di liste front e rear (usare un record con due campi).

ii) Eseguire i test in test-queue.fsx con la nuova implementazione
(basta cambiare il nome del file dll riferito da #r).

*)

//----------    file QueueP.fs ---------------

// Paulson's implementation 

module Queue
exception EmptyQueue

type Queue<'a> = {front: 'a list; rear: 'a list} // record con due campi front e rear

(*

front =  primi elementi della coda
rear  =  tutti gli altri elementi, in ordine inverso

*)   


let empty = {front = []; rear = []}
// val empty : Queue<'a>

let put y {front = fs; rear = rs} = {front = fs; rear = y::rs}
// y va inserito in testa a rear
// val put : 'a -> Queue<'a> -> Queue<'a>

let rec get = function
// va tolto il primo elemento di front, trattando a parte il caso in cui front e' vuoto  
              | {front = []; rear = []} -> raise EmptyQueue  // coda vuota
              | {front = x::fs; rear = rs} ->
                    (x,{front = fs; rear = rs})
              | {front = []; rear = rs} ->  // rear non e' vuota
                    get {front = List.rev rs; rear = []}   // front non e' vuota 
// val get : Queue<'a> -> 'a * Queue<'a>




//----------  END  file QueueP,fs ---------------

(*

Esercizio: ulteriori operazioni
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Aggiungere al modulo Queue le seguenti operazioni
e implementarle in QueueN.fs e QueueP.fs:

*  isEmpty  :  Queue<'a> -> bool

   Controlla se una coda e' vuota

*  toList :  Queue<'a> -> 'a list
  
   Restituisce la lista degli elementi in una coda
   (gli elementi vanno elencati in base all'ordine di inserimento).
   Utile per fare test.
   
* ofList :  'a list ->  Queue<'a>

  Inserisce gli elementi di una lista nella coda vuota

* put_list :  'a list ->  Queue<'a>  -> Queue<'a>

 Inserisce gli elementi di una lista in una coda 


Aggiungere in test_queue.fsx opportuni test  (da provare con entrambe le implementazioni).


*)   
