// ******  TAGGED UNIONS  ******

(* Noti in ADA, Pascal, VBasic come "variant records", sono chiamati
   "datatypes" in FP; è una struttura dati che tiene valori che
   possono prendere tipi diversi ma *fissati*. Solo uno di questi tipi
   può essere usato in ogni momento ed un campo *tag* lo indica
   esplicitamente.

Cominciamo con il caso degenerato:

******  TIPI ENUMERATI  ******)

type colore = Rosso | Blu | Verde ;;

(*

Viene definito il tipo colore i cui elementi sono le costanti Rosso,
Blu e Verde.

Rosso, Blu e Verde sono detti *constructor*.  In questa
definizione abbiamo costruttori senza argomenti, che definiscono le
costanti di tipo colore.

NOTA: I costruttori *devono* essere in maiuscolo

 NOTA: da non confondersi con gli enum type in C# (che ci sono anche in F#).
*)   

(**  PATTERN MATCHING  con enumeration types **)

(* Definire la  funzione

     valore : colore -> int

che  associa a ogni colore un intero nel modo seguente:

    Rosso ---> 1    Blu   ---> 2    Verde ---> 3 

*)

let valore col =
    match col with  // pattern matching su un valore di tipo colore
    | Rosso -> 1
    | Blu -> 2
    | Verde -> 3 ;;
// val  valore : colore -> int 



// Esempio di applicazione della funzione valore

let v1 = valore Rosso ;;
// val v1 : int = 1


// altro esempio di  enumeration type

type month = January | February | March | April | May | June | July
             | August | September| October | November | December ;;

// Definire la funzione daysOfMonth che calcola il numero di giorni in
// un mese (anno non bisestile)

let daysOfMonth  m =
  match m with
    | February                            -> 28
    | April | June | September | November -> 30
    | _                                   -> 31 ;;

// val daysOfMonth : month -> int


let d = daysOfMonth March ;;
// val d : int = 31

// guarda chi si vede ...

type mbool = True | False ;;

(*

Definire le funzioni

*  myNot : mbool -> mbool  // operatore Not su mbool
  
*  mbool2bool : mbool -> bool   // trasforma un bool nel corrispondente valore mbool

*)   


let myNot b = 
    match b with
    | True  -> False
    | False -> True ;;

// val myNot : mbool -> mbool

let mbool2bool  b = 
    match b with
    | True  -> true
    | False -> false ;;

// val mbool2bool : mbool -> bool

let bb =
  myNot True
  |> mbool2bool
  |> not ;;
// val bb : bool = true






(****  TAGGED VALUES  ****

Sono usati per raggruppare in un unico tipo elementi di forma diversa 
(discriminated union).

Esempio
^^^^^^^
Definiamo il tipo figura in cui ogni elemento puo' essere:

- un Rettangolo di cui si specifica la misura della base e dell'altezza

   OPPURE
 
- un Quadrato di cui si specifica la misura del lato;

   OPPURE

- un Triangolo di cui si specifica la misura della base e dell'altezza. 

*)

// Definizione del datatype figura

type figura = 
   | Rettangolo of  float * float      // base * altezza
   | Quadrato   of  float            // lato
   | Triangolo  of  float * float  ;;  // base * altezza

(*

I costruttori usati nella definizione sono funzioni. In particolare:

 Rettangolo :  float * float -> figura 
 Quadrato   :  float -> figura 
 Triangolo  :  float * float -> figura 

Applicando uno dei costruttore sopra a un argomento si ottiene un 'tagged value' di tipo figura-

NOTA

Un enumeration type (es., i tipi colore e month definiti sopra)
e' un  datatype in cui tutti i costruttori hanno arieta' zero (non hanno argomenti).

*)   


// Esempi di tagged value di tipo figura

let rett = Rettangolo (4. , 5.) ;;
//  rett : figura = Rettangolo (4,5) 

let quad1 = Quadrato 10.  ;; 
// val quad1 : figura = Quadrato 10

let quad2 = Quadrato 5.  ;;

let tr = Triangolo (5.,3.) ;;


(**  PATTERN MATCHING  con tagged value  **)


(*

Definiamo la funzione area che calcola l'area di una figura fig.
Si assume, per ora, che fig sia una figura ben definita (le dimensioni non sono negative).


La funzione area deve avere tipo

  area : figura -> float 


*)

let area fig =
   match fig with
   | Rettangolo(b,h) ->  ( b * h )    //  b * h ha tipo int 
   | Quadrato lato   ->  (lato * lato )
   | Triangolo(b,h)  ->  ( b * h )  / 2. ;;

(*
   Ricordarsi che / e' overloaded (divisione intera oppure divisione decimale);
   il significato di / e' dato dal tipo degli operandi. 
*)


// Esempi

let aRett = area rett ;;
// val aRett : float = 20.0

let aQ1 = area quad1  ;;
// val aQ1 : float = 100.0

let aQ2 = area quad2 ;;
// val aQ2 : float = 25.0




// *** Esempio di discriminated union con un solo costruttore 

type figuraColorata =  Col of figura * colore ;;

let f1 = Col (quad1, Rosso) ;;

(*
Definire la funzione  valFigura che calcola il valore di una figura colorata figCol
determinato dalla seguente formula:

   valFigura (figCol)  =  area(figCol) *  valore(colore(figCol))  

dove il valore di un colore e' definito come sopra (funzione valore : colore -> int).
Si assume  che figCol sia ben definita (dimensioni non negative).  *)

let valFigura figCol = 
   match figCol with
   |  Col(fig,col) -> (area fig) * float (valore col) ;;

// valFigura : figuraColorata -> float


let vQ1 = valFigura f1  ;;
// val vQ1 : float = 100.0



(* Sembra ridondante, ma questi tags aiutano con
 l'astrazione. Considera queste *abreviazioni* di tipo:

type [typename] = [existingType]


 
   *)

type EmailAddress = string
type State = string
type Zip = string

(*Sono solo alias, rimossi dal compilatore: non vi è data encapsluation .

Non tutte le stringhe sono uguali: una mail è diversa dal nome di
  uno stato. Meglio se le differenzio con dei singleton tagged union
*)

type EmailAddress = EmailAdd of string
type ZipCode = ZipC of string
type StateCode = StateC of string


// construzione
let a' = "a" |> EmailAdd

// distruzione
let (EmailAdd a'') = a'

// O ancora:

type miles = float
type kilometres = float

(* E' praticamente garantito che essendo tutti float, a un certo punto
 li confonderò, cosa che successe nel disastro del NASA’s Mars Climate
 Orbiter, andato perduto nel 1999 per una confusione tra miglia e
 kilometri http://mars.jpl.nasa.gov/msp98/news/mco991110.html

Non vi gantisco che questo vi manderà su Marte, ma molto meglio
*)

type miles2 = M of float
type kilometres2 = K of float


// In F# esistono anche le unità di misura
// https://blogs.msdn.microsoft.com/andrewkennedy/2008/08/29/units-of-measure-in-f-part-one-introducing-units/

   
(*

OPTION TYPE
^^^^^^^^^^^

In F#  e' definito la tagged union polimorfa

type 'a option =   
    | None
    | Some of 'a

Molto utile per esprimere funzioni PARZIALI 

(Maybe in Haskell)

Sintassi alternativa generale per tipi polimorfi (angle brackets)

 type Option<'a> =       
   | Some of 'a        
   | None   

   *)


// costruzione:
let validInt = Some 1
let invalidInt = None

// distruzione: PM!
let choose n =
  match n with 
  | Some x -> printfn "the valid value is %A" x
  | None -> printfn "the value is None" 

do choose validInt   
do choose invalidInt   

(* Esempio 1
   ^^^^^^^^^^ 

La funzione fattoriale definita le scorse lezioni ha tipo

  int -> int

Il fattoriale di n e' in realta' una funzione parziale su int,
in quanto e' definito solo per n >= 0.
Come gestire il caso in cui la funzione e' applicata a un intero n negativo?

Sono possibili almeno die soluzioni:

(i) Viene sollevata una eccezione (vedremo in futuro poi i dettagli e
in particolare come gestirle) *)


let rec gfact n =
    match n with
    | 0 -> 1
    | n -> if  n > 0 then n * gfact (n-1) else failwithf "Negative argument %d to fact" n

let _ =  gfact (3 - 22);;

(*
(ii) Si definisce la funzione in modo che restituisca un valore di tipo 'int option'.

Piu' precisamente, definiamo la funzione

  factOpt : int -> int option

Il risultato R dell'applicazione

   factOpt n

e' un tagged value di tipo 'int option' della forma 'Some k' oppure 'None'.

- Se R = Some k, allora  k e' il fattoriale di n
  (in questo caso si ha n >= 0). 
  
- Se R = None, il fattoriale non e' definito
  (si e' applicata la funzione a  n < 0).

*)


// (ii): with options

let rec factOpt m =
  match m with
    | 0 -> Some 1
    | n  when n < 0 -> None
    | n ->
      match factOpt (n-1) with
        Some k -> Some ( n * k)
        | _  -> None
(*


Con when e' possibile porre delle condizioni su un pattern 

 | p when b1 -> e1 equivalente a p -> if b then e1 else ...

NOTA
^^^^
Evitare di usare when quando non serve

Esempio:

il pattern    xs when xs = []       si scrive semplicemente     []
il pattern   x :: xs when xs = []       si scrive semplicemente     [x]

o di metterci condizioni troppo complesse, che l'interprete non riesce
a decidere se sono esaustive e disgiunte

Utile nel casi di PM come

match e with
  (x,y) when x = y -> e1
  | _ -> e2
  
*)


(*


In (#), si ha n > 0 e  quindi n-1 >= 0.

Questo significa che factOpt (n-1) e' definito e restituisce un valore
della forma Some k (k e' il fattoriale di n-1).

*)   


// Esempi di applicazione di factOpt

let n1 = factOpt -2 ;;
// val n1 : int option = None

let n2 = factOpt 4 ;;
// val n2 : int option = Some 24


(*

Esempio 2
^^^^^^^^^^ 
Definire la funzione

      printfact : int -> string

che calcola il fattoriale di un intero n e restituisce una stringa che descrive il risultato;
se il fattoriale non e' definito, va restituito un opportuno messaggio.
Per calcolare il fattoriale, usare factOpt.


*)

let printfact n = 
   match (factOpt n) with 
   | None ->  "the input " + (string n) + " is negative"
   | Some f ->  ("the factorial of " + (string n) + "  is: " + (string f)) ;;

// val printfact : int -> string

// Esempi

let fs1 = printfact -3 ;;


let fs2 = printfact 3 ;;
// val fs2 : string = "the factorial of 3  is: 6"


(* Esempio 3:
 Trovare la prima posizione di un elemento in una lista

Si noti uso della funzione ausiliaria locale
 findPosI : p:int -> xs:'a list -> int option when 'a : equality

 e sua chiamata con indice a partire da 0.
*)
let findPos x xs =
  let rec findPosI p  xs =
    match xs with
      | []     -> None
      | y::ys  when x = y -> Some p 
      | _ :: ys -> findPosI (p+1) ys
  findPosI 0  xs

// val findPos : x:'a -> xs:'a list -> int option when 'a : equality

let p1 = findPos 4 [2 .. 6];;

let pn = findPos 7 [2 .. 6];;



// In generale, una somma disgiunta è definibile come il tipo polimorfo 'a + 'b

type either<'a, 'b> =   
    | Left of 'a
    | Right of 'b


// Un'altra versione dei booleani

type bool2 = either<unit,unit>    

let true2 = Left ()
let false2 = Right ()

let ite test e1 e2 =
  match test with
    Left () -> e1
    | Right () -> e2


// nota: constanti **non** possono essere usate come patterns e quindi
// questo non va quello che dovrebbe


let ite_bug (test : bool2) e1 e2 =
  match test with
    true2 -> e1
    | false2 -> e2
      
// warning FS0026: This rule will never be matched

// figure
type figura2 = either< float * float,either<float,float * float> >

// let quad2 = Quadrato 5.  ;;
let quad3 = Left (Right 5.)  ;;
let rr = Left (2., 3.)
