
 (***  FUNZIONI ITERATIVE   ***)


(*

Consideriamo la funzione fattoriale fact

*)  

let rec fact n =
  match n with 
    | 0  -> 1                   // n = 0 
    | _  -> n * fact (n-1) ;;   // n > 0 
// val fact : int -> int

(*

Esempio di computazione della funzione fact
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

fact 4   =  4 * ( fact 3 ) 
         =  4 * ( 3 *  ( fact 2 ) )
         =  4 * ( 3 *  ( 2 * ( fact 1 ) )) 
         =  4 * ( 3 *  ( 2 * ( 1 * (fact 0) ) ))
         =  4 * ( 3 *  ( 2 * ( 1 * 1 ) ))

Notare che:

- vengono effettuate 4 chiamate ricorsive, ossia  fact(3), fact(2), fact(1), fact(0).

- ogni chiamata ricorsiva richiede la definizione di un nuovo ambiente
  in cui viene definito il legame (binding) per n.

Infatti:

* la chiamata iniziale f(4) corrisponde al calcolo di f(n) con binding n --> 4   

* la chiamata f(3) corrisponde al calcolo di f(n) con binding n --> 3 

e cosi' via.

Gli ambienti creati nelle chiamate ricorsive sono mantenuti in uno stack.


Problema
^^^^^^^^

Poiche' la moltiplicazione  puo' essere calcolata dopo che si sono fatte
tutte le chiamate ricorsive, lo stack deve conservare tutti gli ambienti
definiti dalle chiamate ricorsive.
Ad esempio, per il calcolo di f(4), lo stack deve contenere i binding

  n --> 4 ,   n --> 3 ,  n --> 2  , n --> 1  ,  n --> 0

che servono  per costruire il termine

   4 * ( 3 *  ( 2 * ( 1 * 1 ) ))

da valutare per ottenere il risultato.

Si puo' gestire meglio l'uso della memoria?

L'idea e' di calcolare gradualmente il risultato usando un accumulatore,
come avviene nella versione iterativa del fattoriale.


Versione iterativa del fattoriale
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Nella versione iterativa, si usa un accumulatore acc
utilizzato per calcolare gradualmente  il prodotto

   1 * 2 * 3 * .... * n

Piu' precisamente, il calcolo di  n! in modo iterativo
corrisponde alla esecuzione del seguente ciclo:

    ---------------------------------
    |** pseudo codice imperativo **  |    
    |   // calcolo n!                |
    |     acc = 1   // accumulatore  |
    |       k = n                    |
    |   while(k > 0){                |
    |     acc = k * acc              |
    |       k = k - 1                |
    |  }                             |
    |//Quando il ciclo termina vale: |
    |//  acc = n!                    |
    ----------------------------------

Il ciclo viene eseguito n volte.
Al termine del ciclo si ha
 
 acc =  1 * ( 2 *  ...  (n-2) * ( (n-1) * (n*1) ))
              
ossia, acc = n!.


Esempio
^^^^^^^

Supponiamo di voler calcolare 4!. All'inizio si ha

acc |  1
----------
 k |   4

Dopo la prima iterazione

acc |  4 * 1 = 4
---------------------
  k |  3

Dopo la seconda iterazione

acc |  3  * (4 * 1) = 12
----------------------------
 k  |  2

Dopo la terza iterazione

acc |  2 * (3 * ( 4 * 1))  = 24
-------------------------------
 k  |  1

Dopo la quarta iterazione

acc |  1 * (2 * (3 * ( 4 * 1)))  = 24
-----------------------------------------
 k  |  0

e il ciclo termina.

Il valore di acc corrisonde a 4!

*****

Si puo' dimostrare in maniera formale che, quando il ciclo termina,
acc e' uguale a n! osservando che  al termine di ogni iterazione
vale questa proprieta' invariante:

(INV)     acc  =  (k + 1 ) * (k + 2 ) * ... * n 

In altri termini, il valore di  acc e' una  porzione del calcolo  di n!.

Al termine del ciclo, k vale 0.
Dalla proprieta' (INV) segue che

      acc  =  (0 + 1 ) * (0 + 2 ) * ... * n 
           =   1 * 2 * ... * n    
            

ossia acc = n!.


Versione iterativa del fattoriale in F#
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Mostriamo ora come si possa simulare in  F# la computazione iterativa del fattoriale
e i benefici che si ottengono in termini di utilizzo della memoria.

Definiamo la funzione ausiliaria

    factA : int * int -> int

con l'idea che  la chiamata

    factA (k,acc)

corrisponde all'esecuzione del ciclo del programma iterativo
con i valori di k e acc definiti dall'argomento.

 (i)  Se k = 0, il ciclo  termina e acc corrisponde a n!
      Quindi, la funzione restituisce acc.

(ii)  Se k > 0 , occorre fare una nuova iterazione del  ciclo dove
      *   il nuovo valore di  k    e'    k-1 
      *   il nuovo valore di acc   e'    k * acc
     Questo corrisponde a eseguira una chiamata ricorsiva
     con argomento (k-1 , k * acc).

Segue che:

 factA(0,acc)  =  acc  // (i)
 factA(k,acc)  =  factA(k-1 , k * acc)  se k > 0  // (ii)

Per calcolare n!, devo eseguire il ciclo con i valori iniziali 
k=n e acc=1.
Quindi:

  n! = factA(n,1)   

In termini piu' formali,  si puo' dimostrare, per induzione su k, che:
   
   factA(k,acc) =  k!  * acc   

Quindi:

  factA(n,1) =  n! * 1  = n!


Esempio 
^^^^^^^

Per calcolare 4! occorre calcolare

factA(4,1)

Infatti:

factA(4,1)  =  factA(3,4)      //  4 = 4 * 1
            =  factA(2,12)      // 12 = 3 * 4 = 3 * (4 * 1 )
            =  factA(1,24)      // 24 = 2 * 12 = 2 * (3 * (4 * 1))
            =  factA(0,24)      // 24 = 1 * 24 = 1 *  (2 * (3 * (4 * 1)))
            =  24  

Quindi 4! = 24

Possiamo ora definire la funzione  ifact (fattoriale iterativo)


*)

// fattoriale iterativo

let  ifact n = 
 let rec factA = function   // funzione ausiliaria
    | (0,acc) -> acc
    | (k,acc) -> factA(k-1,k*acc)
 factA(n,1) ;;  // n! = factA(n,1)
// val ifact : int -> int

(*

Differenza fra le due versioni della funzione fattoriale
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Versione ricorsiva

Per calcolare fact(n)  occorre calcolare
 
     n * fact (n - 1)  

La moltiplicazione non puo' essere eseguita subito,
ma va ritardata fino a quando il valore di  fact (n - 1) e' stato calcolato.
Quindi, lo stack deve mantenere *tutti* i binding per n definiti nelle chiamate ricorsive.
in modo che alla fine si abbiano a disposizione tutti i termini da moltiplicare.     

-  Versione iterativo

Per calcolare factA(k,acc) occorre calcolare

         factA(k-1,k*acc) 

Dopo la chiamata ricorsiva non c'e' alcuna operazione da compiere,
quindi *non* e' necessario  conservare l'ambiente corrente (binding per k e acc).

Tail recursion
^^^^^^^^^^^^^^

Quando in una funzione ricorsiva la chiamata ricorsiva viene operata
come ultimo passo (come in factA) si parla di *ricorsione in coda* (*tail recursion*).

Una funzione in cui le chiamate ricorsive sono in coda (ad es., la funzione ifact)
e' detta *iterativa*.
Le funzioni iterative consentono un uso ottimizzato delle risorse.

Considerazioni finali
^^^^^^^^^^^^^^^^^^^^^

Come per la ricorsione, non esiste un'unica ricetta per definire le
funzioni iterative, ogni esempio ha le sue particolarita' (occorre
fare esercizi).

L'esempio del fattoriale e' trattato sul libro nelle sezioni 9.3, 9.4.
Per approfondimenti sulla gestione della memoria, vedi sezioni 9.1, 9.2.
Per approfondimenti sulla ricorsione in coda, vedi corso di Algoritmi.


*)   



