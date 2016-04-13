
// ----  TYPE INFERENCE  (Esempio) ----


// exp (b,n) calcola b ^ n
let rec exp (b, n ) =
  match n with
    | 0  -> 1.0                          (* 1 *)
    | _  ->  b  *  exp (b, n-1) ;;       (* 2 *)





(*

* Poiche' l'argomento della funzione exp e' una coppia,
  il tipo di exp deve essere della forma

        τ1 * τ2 -> τ3 
      
* Il tipo τ3 e' dato dal tipo delle espressioni a destra di ->
  (ricordare che tutte le espressioni a destra di -> devono avere lo stesso tipo).

  Poiche' nel caso  (* 1 *) a destra di -> c'e' 1.0, che e' una costante di tipo float, si puo' porre
    
       τ3 = float

* Il tipo   τ2 e' il tipo di n.
  In (* 1 *)  viene considerato il caso n = 0; questo significa che n deve avere tipo int, quindi
 
      τ2 = int

* Il tipo   τ1 e' il tipo di b.
  In   (* 2 *) compare l'espressione

       b *  exp (b, n-1)

  Il valore di  exp (b, n-1) ha tipo τ3, ossia float. 
  Poiche' gli operandi di * devono avere lo stesso tipo,
  segue che b deve avere tipo float:

     τ1 = float

Si puo' concludere che 

  exp : float * int -> float

*)





// --------  IF-THEN-ELSE EXPRESSIONS --------

(*


Data una espressione ExpBool di tipo bool


   if ExpBool then e1 else e2
  
e' equivalente a

match ExpBool with 
 | true  ->  e1
 | false ->  e2 

Quindi, e1 e e2 devono avere lo stesso tipo.

L'espressione if-then-else  scritta sopra  *NON* va confusa
con l'analoga struttura di controllo di selezione dei linguaggi imperativi!

In particolare, non sono ammesse  espressioni della forma

   if ExpBool  then e1    

in cui manca il ramo else. Infatti, tale espressione equivale a

match ExpBool with 
  | true -> e1

che non ha senso (pattern-matching incompleto)

Le espressioni booleane  composte sono costruite  usando gli operatori booleani:

  not ,   &&  (and)  , || (or)

&& and || sono valutati in modalita' lazy (come in C, Java ...)


*)

// Esempi di  if-then-else expression.

let pariOdispari n  = if (n % 2 = 0 ) then "pari" else "dispari" ;; 
// val pariOdispari : int -> string

pariOdispari 10 ;;

(*

Evitare di scrivere codice ridondante della forma

let pariOdispari n  = if (n % 2 = 0 ) = true  then "pari" else "dispari" ;; 


Infatti:

  ExpBool = true     ha lo stesso valore di     ExpBool
  ExpBool = false    ha lo stesso valore di     not ExpBool
  



*)   



// fattoriale definito con if-then-else
let rec fact_if n =
    if n=0 then 1 else n * fact_if(n-1);

// fattoriale definito con pattern matching
let rec fact n =
    match n with
        | 0 -> 1
        | _ -> n * fact ( n-1 );;

// In questi casi e' preferibile il pattern matching (codice piu' comprensibile)


(*

Verificare come viene valutata l'espressione

let f x =  if x > 0  then  "x e' positivo"  else false ;;

*)   


// ----  POLYMORPHISM  -----


(*

Un  *tipo polimorfo (polymorphic type)* e' un tipo contenente variabili,
dette *variabili di tipo (type variable)* .

In genere le variabili di tipo sono denotate da lettere greche;
nel codice  si usano gli apici davanti al nome, esempio:

   'a  , 'b,  ....


Una  funzione (piu' in generale, una espressione) e' detta *polimorfa*
se il suo tipo contiene variabili.

*) 

// Esempi di funzioni polimorfe


// Definire la funzione swap che data la coppia (x,y) restituisce (y,x)

let swap (x,y) = (y,x)

// notare la semplicita' del codice rispetto alla programmazione imperativa

(*

La funzione swap e' polimorfa in quanto puo' essere applicata ad arbitrarie coppie (x,y)

Il tipo della funzione e'

     swap : 'a * 'b -> 'b * 'a

che e' il tipo di una funzione che, data una coppia di tipo  'a * 'b,
restituisce una coppia di tipo 'b * 'a.

Quando swap e' applicata, le variabili di tipo 'a e 'b sono istanziate in base agli argomenti

*)   


let s1 = swap ("uno", 2) ;;
// val s1 : int * string = (2, "uno")


let s2 = swap ( "funzione  successore" , fun x -> x + 1 ) ;;
// val s2 : (int -> int) * string = (<fun:s2@181>, "funzione  successore")

// Definire la funzione id che, dato  x,  costruisce la coppia (x,x)

let id x = (x,x) ;;
// val id : 'a -> 'a * 'a

let id1 = id 100 ;;
// val id1 : int * int = (100, 100)

let id2 = id "ciao"  ;;
// val id2 : string * string = ("ciao", "ciao")

let id3 = id id2 ;;
// val id3 : (string * string) * (string * string) = (("ciao", "ciao"), ("ciao", "ciao"))


// Definire la funzione first che restituisce la prima componente di una coppia

let first (x,y) = x ;;
// val first : 'a * 'b -> 'a

let f2 = first id2 ;;
// val f2 : string = "ciao"

let f3 = first id3 ;;
// val f3 : string * string = ("ciao", "ciao")

// altra definizione

let first1 p =
  let (x,y) = p  
  x
// val first1 : 'a * 'b -> 'a

(*

** IMPORTANTE **
^^^^^^^^^^^^^^^^

Notare l'uso del pattern in una let-expression.

Nell'ultimo esempio, scrivendo

    let  (x,y) = p

si assume che p soddisfi il pattern (x,y), e quindi che p sia una tupla con due componenti (coppia).
La prima componente di p e' legata a x, la seconda componente a y.

Se p e' la coppia ("uno", 2), la valutazione della let-expression crea i binding

   x --> "uno"       y -- > 2   

Questo uso del pattern matching e' molto utile quando occorre destrutturare un termine complesso.

Ad esempio, se f e' una funzione che restituisce una tupla con tre componenti, la definizione 

  let (x,y,z) = f k

permette di accedere direttamente alle tre  componenti di f k


*)  


// ---    TYPE CONSTRAINTS ------------

(** 

Le funzioni polimorfe possono richiedere vincoli aggiuntivi sulle variabili di tipo.
Nel seguito incontreremo due tipi di vincoli su una variabile di tipo 'a:

1)   when 'a : equality

Sul tipo che istanzia 'a deve essere  definita l'uguaglianza =


2)  when 'a : comparison 
 
Sul tipo che istanzia 'a deve essere  definito l'ordinamento <

**)   


// 1) Esempio di funzione  polimorfa con vincolo su = 

let  cmp (x,y) =
    if x = y then "uguali" else "diversi" ;;

//   cmp : 'a * 'a -> string when 'a : equality
//                           ^^^^^^^^^^^^^^^^^^

//  Esempi di chiamate corrette (il tipo 'a e' istanziato correttamente) 

cmp ( 1, 10 - 9 ) ;;  //  "uguali"     'a = int

cmp ( "sole", "luna" ) ;;  // "diversi"    'a = string

cmp ( (0,0) , (0,1) );;  // "diversi"     'a = int * int

cmp ( ( 1 - 1 ,"sole", 'x') , (0, "so"+"le", 'x' ) ) ;;  // "uguali"    'a = int * string * char
//  "so" + "le" e' la stringa "sole" (+ e' l'operatore di concatenazione fra stringhe)


// Verificare come viene valutata l'espressione
// cmp (fun x -> x + 1, fun x -> x + 1) ;;     

// 2) Esempio di  funzione polimorfa  con vincolo su < 

let  less (x,y) =
    if x < y then "minore" else "maggiore o uguale" ;;

(*

Il tipo della funzione e'  

   less : 'a * 'a -> string when 'a : comparison
                            ^^^^^^^^^^^^^^^^^^^^

In questo caso 'a puo' essere istanziato con un qualunque tipo
su cui sia definito l'ordinamento < , ad esempio int, char, bool, ..

  
*)


less('z' , 'a')  ;;  // "maggiore o uguale"    'a = char

less("abaco" , "baco" ) ;;  // "minore"  'a = string
// < sul tipo string corrisponde all'ordinamento lessicografico
