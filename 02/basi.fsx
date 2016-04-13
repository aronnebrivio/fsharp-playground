// *** BASI DEL LINGUAGGIO  ***



//----   ESPRESSIONI ----

(* Ogni espressione di F# ha un *tipo (type)* e un  *valore  (value)*

   Per indicare che una espressione Expr ha tipo Type si scrive

        Expr : Type

   Esempio:

       2  : int       // 2 ha tipo int
     3.5  : float     // 3.5 ha tipo float

   Il concetto di tipo e' una delle nozioni piu' importanti della programmazione funzionale.
   Compito dell'interprete e' determinare  tipo e valore di  espressioni.

*)


// Le espressioni piu' semplici sono le costanti numeriche. Esempi:

2 ;;
// val it : int = 2

(* il tipo  dell'espressione 2 (legata al nome <it> ) e'  int e il valore e' 2 *)


3.14;;
// val it : float = 3.14
(* L'espressione  ha tipo float e valore 3.14   *)


//  Le definizioni scritte nell'interprete vanno terminate con ;;

// Esempi di espressioni aritmetiche 

2 + 5 ;;
// val it : int = 7

3.13 * 2.5 ;;
// val it : float = 7.825


// ----- LET DECLARATIONS  --------------

let x = 2;;
// val x : int = 2

(*
 
Una dichiarazione della forma

  let x =  ....

(*let declaration*) lega l'identificatore x al valore  dell'espressione scritta a destra di =.
Nell'esempio sopra, l'identificatore x viene legato al valore 2.
L'identificatore x puo' ora essere usato in successive definizioni.

- Il legame fra un identificatore e il suo valore e' chiamato *binding*

- L'insieme dei binding costituisce l'ambiente (environment) del programma.

- Un binding va intesa come una definizione di un identificatore;
  è possibile, ma deprecato, ri-definire un identificatore gia' definito.


**NON**  confondere le let declaration con gli assegnamenti dei linguaggi imperativi.

L'assegnamento

   x =  v

non crea alcun legame fra x e v, ma memorizza  nell'area di memoria denotata da x il valore v.
Il contenuto della locazione di memoria x puo'  variare durante l'esecuzione del programma
(mentre una let declaration introduce un legame immutabile fra un identificatore e un valore)

*)

let y = 3 ;;
// val y : int = 3

let z = x + y ;; 
// val z : int = 5

// Esempio di espressioni booleane

let t = (5 = z) ;;
// val t : bool = true
// 5 = z e' una espressione di tipo bool e valore true

y ;;
// val it : int = 3
// L'identificatore y e' una espressione di tipo int e valore 3

y - 1 ;;
// val it : int = 2


//----   DEFINIZIONE DI FUNZIONI ----

// Definiamo la funzione square che calcola il quadrato di un intero x

let square x = x * x ;;
// val square : x:int -> int

(*

square e' la funzione che a x associa  il valore  x * x.

Il tipo di square e'

    int -> int    // funzione che, dato un int, restituisce un int

scritto anche, in forma piu' dettagliata:

    x:int -> int   // funzione che, dato un argomento x di tipo int, restituisce un int


Per applicare la funzione square a un argomento v (di tipo int) si scrive:  
 
    square v   // nome della funzione seguito dall'argomento (separati da uno o piu' spazi)
  
In programamzione funzionale, a differenza dei linguaggi imperativi,
in genere non si racchiudono gli argomenti della funzione fra parentesi tonde
(tranne nei casi in cui le parentesi sono necessarie per interpretare correttamente
la struttura dell'espressione).

*)   


// Esempi di applicazione della funzione square


let a = square 5 ;;
// val a : int = 25

let b = square ( square 2 ) ;;
// val b : int = 16

(*
  Nel secondo esempio le parentesi sono necessarie in quanto l'applicazione
  di funzioni e' associativa a sinistra.

  Senza parentesi l'espressione

      square  square 2

  e' interpretata come

     ( square  square ) 2 

  che non ha senso, in quanto square non puo' essere applicata a square
  (l'argomento di square deve avere tipo int)

*)   


// ---- LAMBDA EXPRESSIONS  -----

// La funzione square puo' essere definita nel seguente modo:

let square1 = fun x -> x * x;;

(* 

Questa sintassi  e' quella primitiva dei linguaggi funzionali
e corrisponde direttamente al formalismo usato nel lambda calcolo.

Al nome  square1 e' legata l'espressione

   fun x -> x * x   // *lambda expression*
  
che rappresenta la funzione che a x (argomento della funzione)
associa il valore dell'espressione  x * x (corpo della  funzione).

La visibilita' (scope) dell'identificatore x e' limitata all'espressione a destra di ->


L'espressione

   fun x -> x * x 

ha tipo

  int -> int

Una lambda expression e' un termine, quindi puo' essere usato
come parte di una espressione piu' complessa


*)

// Esempi di uso di lambda expression all'interno di espressioni

let x1 = ( fun x -> 2 * x) 10 + ( fun x ->  x - 1 ) 20 ;;
// val x1 : int = 39

(*

Il termine

    fun x -> 2 * x

definisce la funzione di tipo int -> int che associa a x  il valore 2 * x.
La funzione e' chiamata  *funzione anonima* in quanto ad essa non e' stato assegnato alcun nome.
La funzione e' applicata al valore 10.

Analogamente, il termine

    fun x -> x - 1 

definisce la funzione di tipo int -> int che associa a x  il valore x - 1.
La funzione e' applicata a 20.

Notare che le variabili x nelle due espressioni funzionali
vanno considerate variabili distinte (vedere le regole di scope)

Il valore di x1 e' quindi

  (2 * 10) + (20 - 1) = 39 

*)

let w = 10 ;;
(fun w -> w*w) w ;;
// val it : int = 100

(*

Notare il diverso significato di w:

- il w dell'espressione 'w*w' e' legato all'identificatore w della lambda expression

- il w piu' a destra e' invece legato al w della let definition
    

L'espressione 

    (fun w -> w*w) w

e' equivalente a

   (fun z -> z*z) w

*)   


   

// ---- TYPE ANNOTATIONS  -----

(*
 
La definizione 

  let square x = x * x ;;

e' interpretata come funzione di tipo int -> int in quanto
all'operazione di moltiplicazione  viene assegnato per default il tipo int -> int.

Per definire una funzione  squareFloat che calcola il quadrato di un float
occorre forzare l'interprete ad assegnare a x il tipo float.

Per far questo, si usa una *annotazione di tipo (type annotation)*


*)   



let squareFloat (x : float) = x * x ;;

(*

La funzione  squareFloat ha tipo  float -> float
e puo' essere applicata solamente a un argomento di tipo float.

Quindi:
  
  squareFloat 2.0    e' corretto      (2.0 e' una costante di tipo float)   
  squareFloat 2      non e' corretto  (2   e' una costante di tipo int)


F# dispone di parecchie funzioni per le conversioni fra tipi numerici.
Ad esempio, la funzione

 float : int -> float

trasforma un valore di tipo int nel corrispondente valore di tipo float. 

L'espressione

   squareFloat ( float 2  )

e' corretta; cosa succede omettendo le parentesi tonde?


OSSERVAZIONE SULLE ANNOTAZIONI DI TIPO (TYPE ANNOTATION)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Le annotazioni di tipo vanno inserite solo se strettamente necessarie.

Ad esempio, si puo' definire  squareFloat annotando anche
l'espressione a destra di =:

 let squareFloat (x : float) = ( x * x ) : float  ;;

Tuttavia in programmazione funzionale si tende a delegare il piu' possibile
all'interprete la ricostruzione dei tipi, intervenendo con annotazioni solo dove necessario.



OSSERVAZIONE SUL CAST
^^^^^^^^^^^^^^^^^^^^^

Nei linguaggi tipo C, Java, ... le espressioni miste della forma

  5 + 2.3    10 - 3 * 2.5

sono accettate (vengono fatti implicitamente dei cast in modo che gli argomenti abbiano lo stesso tipo).

I linguaggi funzionali richiedono un uso rigoroso dei tipi,
quindi vanno  esplicitamente usate le funzioni che effettuano i cast necessari.

Tuttavia le versioni piu' recenti di F# ammettono una sintassi piu' libera
ed effettuano implicitamente alcune conversioni di tipo.

*)  


// Esempio di definizione di funzione contenente una definizione locale

let doubleSqr x =
    let y = square x  // definizione locale  (y non e' visibile all'esterno di doubleSqr)
    2 * y ;; // valore della funzione

//  val doubleSqr : int -> int
//  doubleSqr x calcola 2 * x^2


//  Definire la funzione  circleAre che calcola l'area del cerchio di raggio r

let circleArea r = System.Math.PI * squareFloat r ;;
//  val circleArea : float -> float
//  La costante System.Math.PI : float corrisponde al valore di pi greco



// Definire la funzione isPari che riconosce se un numero e'  pari

let isPari n  =   n % 2 = 0 ;;
// val isPari : int -> bool

//------ TUPLE -------

(*

   Una *tupla* e' ottenuta aggregando elementi; le componenti di una tupla sono separati da virgole. 
   Il tipo di una tupla e' il  *prodotto cartesiano*  (denotato da '*' )  dei  tipi delle componenti.

*)

// esempi di definizioni di tuple

let a1 = 5 ;;
let b1 = 10 ;;

let t1 = ( a1 + 1 , a1 < b1 ) ;;  
// val t1 : int * bool = (6, true)

(*
  t1 e' una tupla con due componenti (coppia):

  - la prima componente   e' 6  (valore di a1 +1)
  - la seconda componente e' true (valore di   a1 < b1)

Poiche' a1 + 1 ha tipo int e  a1 < b1 ha valore bool, il tipo di t1 e'

    int * bool  // prodotto cartesiano fra  int e bool.

Esempi di termini di tipo (int * bool):

 ( 5, true )    ( 10, false)



*)



let t2 = ( (fun x -> x + 1) , t1) ;;
//val t2 : (int -> int) * (int * bool) = (<fun:t2@26>, (6, true))

(*
 La tupla t2 ha tipo  (int -> int) * (int * bool)

- La prima componente di t2 e' una funzione di tipo int -> int
  (la funzione successore di un intero);

- La seconda componente di t2 ha tipo int * bool e valore ( 6, true ).

L'interprete assegna alle funzioni anonime un nome generato automaticamente
(in questo caso <fun:t2@26>).

*)


// Attenzione all'uso delle parentesi !

let pi = 3,14 ;;
// val pi : int * int = (3, 14)
(*

NOTA SULLA SINTASSI DELLE TUPLE
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Una tupla e' costruita usando le virgole, le parentesi tonde possono essere omesse.

Ad esempio, invece di

let t1 = ( a1 + 1 , a1 < b1 ) ;;

  si puo' scrivere 

let t1 =  a1 + 1 , a1 < b1  ;;

Attenzione a non usare la virgola scorrettamente!.

Scrivendo

let pi =  3,14  ;;

il valore di pi e' la coppia (3,14).

Equivale a

let pi  =  (3 , 14 ) ;;  


Se si vuole definire pi come la costante numerica 3,14 va scritto

let pi  = 3.14 ;;    // per separare le cifre intere e decimali va usato il punto

*)   


// altri esempi di definizioni di tuple


let t3 = ( fun x -> x + 1 , t1) ;;
//val t3 : int -> int * (int * bool)

(*
 t3 e' una funzione di tipo

             int -> int * (int * bool)

Definizione equivalente:

let t3 = ( fun x -> (x + 1 , t1)) 



*)

let a3 = t3 7  ;;
// val a3 : int * (int * bool) = (8, (6, true))




// Definiamo la funzione add2 che calcola la somma fra due interi x e y

let add2 (x,y)  = x + y ;;
// val add2 : int * int -> int
// add2(x,y) calcola  x+y


(*

 Notare che add2 ha *un unico* argomento,  ossia la tupla  (x,y) di tipo int * int. 

 Le parentesi non servono per racchiudono l'elenco degli argomenti della funzione add2
 (come invece avviene in C, Java ecc.),  ma indicano che  (x,y) e' una tupla

*)
   

let s1 = add2 (7, 8) ;;
// val s1 : int = 15

(*

add2 e' applicata all'argomento (7,8) (tupla con due elementi)


In questo caso le parentesi sono necessarie. Infatti

   add2 7 , 8

verrebbe interpretato come la tupla il cui primo elemento e'

   add2 7  

e il secondo elemento e' 8.
L'espressione non ha senso in quanto add2 non puo' essere applicata a 7
(7 non ha tipo int * int).


*)   


//----- INTRODUZIONE AL  PATTERN MATCHING -----

(*

Il *pattern matching* e' uno dei costrutti piu' importante della programmazione funzionale.

Permette la definizione 'per casi' in base ai possibili valori di una espressione.

*)   


// Esempio di funzione definita mediante pattern matching su n

let f n =
    match n with
    | 1 -> "uno"                          // valore di f n quando n vale 1
    | 2 -> "due"                          // valore di f n quando n vale 2
    | _ -> "diverso da uno e due" ;;      // valore di f n quando n e' diverso da 1 e 2 

// val f : int -> string

(*

Notare che il pattern matching *NON* e' una struttura di controllo,
ma un costrutto che permette di definire una espressione.

In particolare, a destra di -> vanno messe espressioni con lo stesso tipo.

Definizioni della forma

 match n with
    | 1 -> "uno"  // tipo string
    | 2 -> "due"  // tipo string 
    | _ ->  0     // tipo int


sono sbagliate e danno errore di tipo


*)   



// La funzione daysOfMonth calcola quanti giorni ha il mese specificato 


let daysOfMonth month =
    match month with
        | 2 -> 28      // February
        | 4 -> 30      // April, 
        | 6 -> 30      // June, 
        | 9 -> 30      // September, 
        | 11 -> 30      // November
        | _  -> 31  ;;  // All other months

//  val daysOfMonth : int -> int

// si noti uso della "wildcard" _ per denotare un pattern arbitrario

 // sintassi alternativa piu' compatta   usando "or-patterns"
        
let daysOfMonth1 = function
    | 2        -> 28      // February
    | 4|6|9|11 -> 30      // April, June, September, November
    | _        -> 31 ;;   // All other months
  
// Esempi di funzioni booleane


// Definizione funzione booleana not

let not x  =
    match x with
        | true  -> false
        | false -> true ;;

//  val not : bool -> bool

// oppure 

let not1 = function
    | true  ->  false
    | false ->  true  ;;

// Esempi di espressioni che usano not

not  ( 10 < 20 ) ;;
// val it : bool = false

not1 ( not1  ( 10 < 20 ) ) ;;
// val it : bool = true


// Definizione di And


let And (x,y)  =
    match (x,y) with
    | (true, true) -> true
    | _ -> false  ;;

// val And : bool * bool -> bool


// oppure

let And2 = function    
    | (true, true) -> true
    | _ -> false ;;


And ( 4 = 4   ,  5 < 5 ) ;;
// val it : bool = false

And (  (fun x -> x + 1) 10 =  (fun x -> x - 1) 12 , true) ;; 
// val it : bool = true

(*
   Come esercizio, scrivere la funzione Or : bool * bool -> bool
*)

let Or (x,y) = 
    match (x,y) with
    | (false, false) -> false
    | _ -> true ;;

// oppure
let Or = function
    | (false, false) -> false
    | _ -> true ;;

(*

Definire la funzione isPariString che, dato un intero n,
restituisce una delle stringhe "pari", "dispari"  a seconda che n sia pario dispari.

*)

let isPariString n =
    match n % 2 with
        | 0 -> "pari"
        | _ -> "dispari" ;;
// val isPariString : int -> string


let s = "5 e' un numero " + isPariString 5  ;;
// val s : string = "5 e' un numero dispari"


(*

L'operatore binario + e' overloaded (la sua implementazione è scelta in base ai tipi degli argomenti)

In questo caso, avendo i due argomenti tipo string,
+ rappresenta l'operatore di concatenazione fra stringhe.
Quindi  s ha tipo  string  e valore  "5 e' un numero dispari"


*)


