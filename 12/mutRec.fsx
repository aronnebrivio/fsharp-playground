
(**   MUTUAL  RECURSION   **)

(*

Si ha *ricorsione mutua*  (*mutual recursion*)  quando
due o piu' definizioni si richiamano a vicenda
(ad esempio, per definire  un elemento  A si usa  B e,
contestualmente, per definira B si usa A).

In F#, le definizioni che si richiamano per ricorsione mutua vanno legate da
*and* (vedi gli esempi)


*)   

(*

Esempio 1: pari e dispari
^^^^^^^^^^^^^^^^^^^^^^^^^^
Consideriamo la seguente definizione:

Dato n >= 0
-  n e' pari (even)    SSE  n=0 oppure  n-1 e' dispari
-  n e' dispari (odd)  SSE  n > 1 e  n-1 e' pari. 

Questo e' un esempio di definizione mutuamente ricorsiva.
Partendo dalla definizione, si possono definire per mutua ricorsione
le funzioni

   even : int -> bool
    odd : int -> bool

usando un costrutto della forma

let rec even n = 
   ... posso usare odd ...
and odd n =        
   ... posso usare even ...

*)
   
// Si *assume*  n >= 0
let rec even n = 
    match n with
    | 0 -> true
    | n -> odd (n-1)   // vale n>0, quindi n-1 >= 0   
and odd n =
// and lega le due definizioni (senza and, la chiamata a odd in even darebbe errore)    
  match n with
    | 0 -> false
    | n -> even (n-1) //  vale n>0, quindi n-1 >= 0  

(*

Se n >= 0, la computazione di 'even n' e 'odd n' termina.
Infatti, nella def. di 'even n' si chiama odd su un argomento piu' piccolo;
analogamente, nella def. di  'odd n', si  chiama even con argomento piu' piccolo.

Notare che le chiamate ricorsive sono corrette,
in quanto l'argomento n-1  e'  un intero maggiore o uguale a zero.

Esempi:

 even 2 =  odd 1
        =  even 0
        =  true

 even 3 =  odd 2
        =  even 1
        =  odd 0
        =  false

Cosa succede se calcolo 'even -1' ?


*)  

// alcune proprieta' per QuickCheck

(*
NOTA

I test vanno eseguiti solo per n >= 0, altrimenti
il calcolo di even e odd non termina.

*)   

let prop_even n =
  (n >= 0 && even n)  ==> lazy (not (odd n))
// se n>=0 e   n e' pari, allora n non e' dispari.

Check.Quick prop_even

let prop_even_ev n =
  let isEven = (n % 2  = 0) // isEven = true sse n e' pari
  [n >= 0 ==> lazy (even n = isEven );
   n >= 0 ==> lazy (odd n = not isEven  )]  
Check.Quick prop_even_ev



(*

Esempio 2: file system
^^^^^^^^^^^^^^^^^^^^^^

Un file system puo' essere definito per ricorsione mutua come segue:

- un file system (FileSys) e' un contenitore di elementi  (Element list)
- un elemento e' un file (File) o una directory (Dir):
  * un file e' caratterizzato da un nome  (stringa s)
  * una directory e' definito da un nome (stringa s) e contenuto (un FileSys)

  FileSys  ::=      []    |  Element :: FileSys
  Element  ::=    File(s) |  Dir(s,FileSys)

Per scrivere la definizione in F#, va usato un costrutto della forma

 type FileSys  = ... posso usare Element ...
  and Element  = ... posso usare FileSys ...


*)   

type FileSys = Element list
and Element  = | File of string
               | Dir of string * FileSys;;


// esempio di Element  
let d1 =
  Dir("d1",[File "a1";
            Dir("d2", [File "a2"; Dir("d3", [File "a3"])]);
            File "a4";
            Dir("d3", [File "a5"])
           ]);;

(* d1  : Element

                         Dir("d1")
                           | 
   -----------------------------------------------         
   |              |               |              | 
 File("a1")     Dir("d2")       File("a4")     Dir("d3")
                  |                              |
             --------------                    File("a5")
             |             |
           File("a2")    Dir("d3")      
                           |
                         File("a3")     
*)   



// esempio di FilySys
let f1 = [d1;File("f")];;

(*  f : FileSys 

        --------------
       |             | 
     Dir("d1")      File("f")
       |   
... come sopra ....

*)


(*

Esercizio
^^^^^^^^^
** provare a svolgerlo prima di leggere la soluzione sotto **

Definire per mutua ricorsione le funzioni

  namesFileSys : Element list -> string list
  namesElement : Element -> string list

che restituiscono la lista dei nomi dei file e directory contenuti
nel termine passato come argomento.

Esempio:

  namesElement d1 = ["d1"; "a1"; "d2"; "a2"; "d3"; "a3"; "a4"; "d3"; "a5"]

  namesFileSys f1 = ["d1"; "a1"; "d2"; "a2"; "d3"; "a3"; "a4"; "d3"; "a5"; "f"]

Usare uno schema di mutua ricorsione della forma

 let rec namesFileSys fileSys =
    ...  posso usare namesElement ...
 and namesElement el =
    ...  posso usare namesFileSys ...


*)   

let rec namesFileSys fileSys =
  match fileSys with
    | []    -> []
    | el::els -> (namesElement el) @ (namesFileSys els)
and namesElement el =
  match el with
    | File s    -> [s]
    | Dir(s,fileSys) -> s :: (namesFileSys fileSys);;


let names1 = namesElement d1;;
// val names : string list = ["d1"; "a1"; "d2"; "a2"; "d3"; "a3"; "a4"; "d3"; "a5"]

let names2 = namesFileSys f1;;
// val names : string list =   ["d1"; "a1"; "d2"; "a2"; "d3"; "a3"; "a4"; "d3"; "a5"; "f"]
