////////////////////////////////////////////////

// From Section 6.5 Expression trees
#r "FsCheck"
open FsCheck 



// Definizione di una lista polimorfa, i cui elementi come tagged union
 
type  'a GList =
    | GNil
    | GCons of  'a  *  'a GList;;

let lg1 = GCons( 3 , GCons ( 4, GCons ( (1 + 4) , GNil)));;
let lg2 = GCons( 3 < 5 , GCons ( true, GCons ( 10 > 20 , GNil)));;


// GListToList : 'a GList -> 'a list
// Trasforma una GList in una 'a list

let rec GListToList gList =
    match gList with
        | GNil -> []
        | GCons ( x , tail) -> x :: ( GListToList tail );;


let list2 =  GListToList lg2;;
// val list2 : bool list = [true; true; false]


// viceversa

let rec List2GList = function
   | [] -> GNil
   |  x :: tail  -> GCons ( x , List2GList tail);;

let l3 =  List2GList [1..10];;


// le due funzioni sono inverse
let  prop_list (xs : char list, ys : bool GList ) =
  [xs = (List2GList xs |> GListToList) |@ "L2G";
   ys = ( GListToList ys |> List2GList ) |@ "G2L"
   ]

do Check.Quick prop_list



//  e ::=  n | e1 + e2 |   e1 * e2 | e mod 2  
// make sure you understand BNF

type aexp =
  | C of int
  | Sum of aexp * aexp
  | Prod of aexp * aexp
  | Mod2 of aexp;;

// infix notation
let (++) m n = Sum(m,n);;
let (@@) m n = Prod(m,n);;

// examples
// ((3 * 5) mod 2 ) + 12 
let a1 =   (Mod2(C(3)  @@ C(5))) ++ C(12);;


// explain natural semantics: e >> v  and trace ((3 * 5) mod 2 ) + 12 

// eval  : aexp -> int

let rec eval t  =
    match t with
    | C n         -> n
    | Mod2 t      -> eval t  % 2
    | Sum(t1,t2)  -> eval t1  + eval t2 
    | Prod(t1,t2) -> eval t1  * eval t2 ;;

let r1 = eval a1;;

(* un esempio di compilazione su AST: semplificazione con 0, basato su
equazioni:

n + 0 = 0 + n = n
n * 0 = 0 * n = 0

   *)

// val opt : e:aexp -> aexp
let rec opt e =
  match e with
    | C n -> C n
    | Sum(C 0,a) | Sum(a,C 0) ->  opt a
    | Sum(n1,n2) -> Sum(opt n1,opt n2)
    | Prod(C 0,_) | Prod(_,C 0) ->  C 0
    | Prod(n1,n2) -> Prod(opt n1,opt n2)
    | Mod2 a -> Mod2 (opt a)


// validiamo  la correttezza della trasformazione:
// se ottimizzo e poi valuto ottengo lo stesso che valutare    
let prop_opt a =
  eval a = (opt a |> eval)
do Check.Quick prop_opt


(*
Let's extend expressions with variables 

e ::=  .. | x  

so that we can say something like

 (3 + x) * ( 5 + y )

So, we modify our def of expression to account for variables, here
denoted by strings *)

type aexpv =
  | C of int
  | V of string          // NEW !
  | Sum of aexpv * aexpv
  | Prod of aexpv * aexpv
  | Mod2 of aexpv;;


 (*
We can evaluate such an expression ony of we know how to evaluate a
 variable. Similarly to what F# does, we use an environment, ie a
 finite map of variables to values, to record and looup values of vars

env ::= empty | env + (x -> v)

 Enviroments: to implement finite maps we do not reinvent the wheel
but use the module Map
*)

// we fix the notion of enviroment
type enviroment = Map<string,int>;;

//  the  empty enviroment is 
let emp = Map.empty;;

//  adding to an env
let envy = Map.add "y" 3 emp;;

let envyx = Map.add "y" 4 (Map.add "x" -7 envy);;


// It's easier to turn a list of pairs into  a map
let envyx2 = Map.ofList [("y", 3), ("x", - 7)];;

//	lookup in an enviroment is like array selection
 let three = Map.find "y" envy ;;

// also in array-like notation

let ee = envyx.["y"];;

// Note: enviroments are partial, so lookup can fail

let _ = envyx.["z"];;

// One can use Map.tryFind which returns an option
// val it : ('a -> Map<'a,'b> -> 'b option) when 'a : comparison 


(* The evaluation judgment is now modulo the enviroment 

env |- e >> v has rules  as follows:


env(x) = v
------------	
env |- x >> v
*)


let et =
    Prod(Mod2 (C 5),
         Sum( (C (- 3)),
             Sum(V "x", V "y")));;

let rec aeval t env =
    match t with
    | C n      -> n
    | V s      -> Map.find s env
    | Mod2 t      ->   aeval t env  % 2
    | Sum(t1,t2)   -> aeval t1 env + aeval t2 env
    | Prod(t1,t2)  -> aeval t1 env * aeval t2 env;;

let res = aeval et envyx;;

/////////////// MUTREC


// Example of mutually recursive  function definition:
// note the <and> keyword

let rec even n = 
    match n with
    | 0 -> true
    | n -> odd (n-1)
and odd n =
 match n with
    | 0 -> false
    | n -> even (n-1)

// n >= 0 premise used to rule out negative integers that loop
let prop_even n =
  [(n >= 0 && even n)  ==> lazy (not (odd n))
   (n >= 0 && not (odd n))  ==> lazy (even n)
   ]
do Check.Quick prop_even


// Mutual recursion does not need to be considered primitive, but can
// be unrolled back to basic recursion. Here's a simple example:

type flag = Ev | Od

let rec even_odd (n,f) =
  match (n,f) with
    | (0,Ev) -> true
    | (0,Od) -> false
    | (m,Ev) -> even_odd (m-1,Od)
    | (m,Od) -> even_odd (m-1,Ev)


let prop_even_ev_od n =
  [n >= 0 ==> lazy (even n = even_odd(n,Ev ));
   n >= 0 ==> lazy (odd n = even_odd(n,Od ))]  
do Check.Quick prop_even_ev_od


// Another Example of Mut Rec: File system
// The types are Mut Rec!!
// FileSys ::= [] | Element :: FileSys
// Element ::= File(s) | Dir(s,FileSys)

type FileSys = Element list
and Element  =
  | File of string
  | Dir of string * FileSys;;

let d1 =
  Dir("d1",[File "a1";
            Dir("d2", [File "a2"; Dir("d3", [File "a3"])]);
            File "a4";
            Dir("d3", [File "a5"])
           ]);;


let d2 = [d1;File("f")];;

// Let's collect the names of all the files in a file system
// Since FileSys is mut rec, we need a mut rec function

//  namesFileSys : fs:Element list -> string list
//  namesElement : e:Element -> string list

let rec namesFileSys fs =
  match fs with
    | []    -> []
    | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement e =
  match e with
    | File s    -> [s]
    | Dir(s,fs) -> s :: (namesFileSys fs);;


let l1 = namesElement d1;;
let l2= namesFileSys d2;;



(*

Tuple vs. record
^^^^^^^^^^^^^^^^^

Quando occorre aggregare piu' dati in una struttura, non sempre l'uso
di una tupla e' la soluzione piu' conveniente.

Infatti:

- due tuple con gli stessi tipi non sono distiguibili
  + esempio: coppia di float che denota un numero complesso e coordinate geografiche

- elementi della tupla aventi lo stesso tipo possono essere confusi.
 + supponiamo di voler rappresentare un carrello della spesa contenente elementi della forma

       (  nome_prodotto  , peso , prezzo_unitario_prodotto )

  Poiche' sia il peso che il prezzo unitario hanno tipo float, la tupla ha tipo

        string * float * float

  e la seconda e terza componente sono indistinguibili in base al tipo.  

Una soluzione migliore, che permette un maggior controllo nell'uso dei dati,
e' l'utilizzo di record.

Un record non è che una tupla dove accesso non è per posizione ma attraverso una label


*)

// DEFINIRE RECORDS
// Definizione di un tipo record che permette di rappresentare un elemento del carrello

type Item = { name : string ; weight : float ; uprice : float  } ;;

(*

Viene definito il tipo record Item avente tre label: name, weight, uprice.
I tipi record vanno sempre definiti esplicitamente.

*)

// CREARE RECORDS
// Esempi di valori di tipo Item (record expression)

let it1 = { name = "mele" ; weight = 2.5 ; uprice = 2.5 } ;; 

(*

it1 e' composta dai seguenti campi (field):

- la stringa "mele" con label name
- il float 2.5      con label weight
- il float 2.5      con label uprice

*)   

let it2 = { name = "pere" ; uprice = 2.0 ; weight = 1.5 } ;;
let it2' = { uprice = 2.0 ; name = "pere" ;  weight = 1.5 } ;;

// a differenza delle tuple, l' ordine con cui i dati sono elencati e' irrilevante

let b = it2 = it2'


(*

I record sono immutabili; non e' quindi possibile modificare i campi
di it1, it2. Come nelle liste, si creano nuovi valori. La sintassi
<with> aiuta a creare un nuovo record da vecchio cambiando un solo campo.

Es: le pere aumentano
*)

let it3 = { it2 with uprice = 3.0 } ;;

// In vero, esistono anche record mutabili, dove un campo è dichiarato
// <mutable>. Per ragioni pedagogoche, li evitiamo

(*** USARE RECORDS: PATTERN MATCHING  ***)

// pattern matching in una let-definition -- records sono tuple, sotto sotto

let   { name = n1 ; weight = w1 ; uprice = p1 } = it1 ;;

(*

Il pattern matching determina i seguenti binding:

 n1  -->  "mele"
 w1  -->   2.5
 p1  -->   2.5

*)

w1 * p1 ;; //  6.25


// come al solito, nel pattern si possono usare underscore


let   { weight = w2 ; name = _ ; uprice = _ } = it2 ;;

// w2 : float = 1.5

w2  * 10.0 ;;  // 15.0 

// anzi pure dimenticare i campi non rilevanti:


let   { weight = w3} = it2 ;;

(*
Esempio:

  cost : Item -> float

che dato un item ne calcola il costo complessivo.


*)   

let cost item =
  match item with
    {weight = w ; uprice = p} -> w * p ;;
// val cost : Item -> float

cost it1 ;;  // 6.25 

// OPPURE direttamente

let cost1  {weight = w ; uprice = p} = w * p ;; // 3.0
// val cost1 : Item -> float


(* E' possibile accedere a un campo di un record usando la dot notation *)

let cost3 item = item.weight * item.uprice ;;
// val cost3 : Item -> float

cost3 it3 ;;  
