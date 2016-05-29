// TYPING

(**  RECAP: ESPRESSIONI  INTERE  CONTENENTI VARIABILI     **)

(*

Definizione di expv
^^^^^^^^^^^^^^^^^^^^

Una espressione intera di tipo expv e' definita da

   e  ::=  x | n |  e1 + e2 

dove:

-  x e' una variabile;
-  n e' una costante intera;
-  e1, e2, sono espressioni di tipo expv.

*)
type expv =
  | V of string          // variabile
  | C of int
  | Sum of expv * expv

(*
Valutazione di expv
^^^^^^^^^^^^^^^^^^^^

La valutazione di una espressione di tipo expv ha senso solo
se alle variabili che compaiono in essa  e' assegnato un valore.

Chiamiamo ambiente (environment) una funzione finita (mappa) che assegna valori alle variabili.
*)


// Per rappresentare un ambiente usiamo la collezione delle funzioni finite

type envt = Map<string,int> ;;

(* Funzioni che useremo:

*  Map.empty : Map<'a,'b>

   Costante che definisce la mappa vuota. 

*  Map.ofList : ('a *' b) list ->  Map<'a,'b> 
   
   Data la lista di coppie  [(a0,b0) ; ... ; ( an,bn)] : ('a * 'b )

       Map.ofList  [(a0,b0) ; ... ; ( an,bn)] 

    crea la mappa  {(a0,b0), (a1,b1), ... , (an,bn)}.
    Le chiavi a0, a1, a2, ... devono essere distinte o viene tenuta buona l'ultima


*  Map.add: 'a -> 'b -> Map<'a,'b> -> Map<'a,'b>

   Aggiunge a una mappa una coppia (a,b).
   Piu' precisamente, data  una mappa m :  Map<'a,'b>,

         add a b m  

   e' la mappa m' tale che:
    
    - m'(a) = m(b)   // se m e' definita su a, il valore di a e' ridefinito
    - m'(k) = m(k)   per ogni chiave k diversa da a


* Map.find: 'a -> Map<'a,'b> -> b
  
  Dati a : 'a  e  m : Map<'a,'b>

      Map.find a m  = b  se m(a) = b  
      
  Solleva una eccezione KeyNotFoundException se a non appartiene a Dom(m).

* Map.tryFind: 'a -> Map<'a,'b> -> 'b option
  
  Simile a find, ma usa option type

  Map.tryFind a m  = Some b   se  m(a) = b
                      None     se a non appartiene a Dom(m)
*)

let env1 = Map.add "x" 55 Map.empty;;
let env2 = Map.add "x" 1 env1;;
let env3 = Map.add "y" 2 env2;;
let v = Map.find "y" env3 
let no = Map.tryFind "pippo" env3 


(*

Il giudizio

  env |- e >> v 

indica che l'espressione e (espressione di tipo expv) ha valore v (un intero) nell'ambiente env.
   
Definiamo  'env |- e >>  v' per induzione sulla struttura dell'espressione e.

1) Se 'e' e' la variabile x, allora  env |- x >>  env(x)
   
2)  env |- n >> n  ,  per ogni costante intera n

3)  env |- e1 >> n1  e   env |- e2 >> n2   implica   env |-  e1 + e2 >> n1 + n2


Esempio.

Consideriamo l'ambiente 

  envxyz = { (x,1) , (y,2) , (z,3) }

Allora:

  envxyz |-  y + z + 10  >> 1 5


 evalv  : expv -> envt -> int
 ( evalv e env = v )   sse  (  env  |-  e >> v )

Nota che per questo linguaggio ambiente è fisso, ma se avessi
esptessioni come LET, allora verrebbe cambiato dinamicamente
*)
let rec evalv e ( env : envt)  =
    match e with
    | V x ->  Map.find x env  // calcola env(x)  
    | C n -> n
    | Sum(e1,e2)   -> evalv e1 env  + evalv e2 env 

// Esempi

// envxyz = { (x,1) , (y,2) , (z,3) }    
let envxyz = Map.ofList [ ("x",1) ; ("y",2) ; ("z", 3)] ;;         
let vv = evalv  (Sum(Sum ( V "y" , V "z" ),  C 10)) envxyz;; // 15 


// Towards typing: expressions ranging over integers and booleans

type exp =
  | I of int                 (* integers *)
  | V of string              (* vars *)
  | Sum of exp * exp           (* addition *)
  | Diff of exp * exp        (* difference *)
  | Prod of exp * exp        (* products *)
  | B of bool                    (* true/false         *)
  | Eq of exp * exp           (* equality     *)
  | Neg of  exp            (* negation     *)
  | Less of exp * exp;;    (* less *)


// infix notation
let (++) m n = Sum(m,n);;
let (@@) m n = Prod(m,n);;
let (--) m n = Diff(m,n);;
let (==) m n = Eq(m,n);;
        

// for enviroments we use againn the class Map, but map vars to exp,

type enviroment = Map<string,exp>;;

let anenv = Map.add "x" (I 3) (Map.add "a" (I -7) Map.empty);;

// 2 * (3 + x + a)
let  et  = I 2 @@ (I 3 ++ V "x" ++  V "a");;

(*

 evaluation: eval : e:exp -> env:Map<string,exp> -> exp

 Note that differently from before we do not return an integer/bool,
but an exp, because a well typed function must choose what to return. 
Alternatively, we could have used Either<int,bool>, but it's quite similar 

*)

let rec eval e env =
    match e with
    | I n      ->  I n
    | B b     ->  B b
    | V s      -> Map.find s env
    | Sum(e1,e2) -> 
        let (I n1) = eval e1 env
        let (I n2) = eval e2 env
        I (n1 + n2)
    | Diff(e1,e2)  -> 
        let (I n1) = eval e1 env
        let (I n2) = eval e2 env
        I (n1 - n2)                                          
    | Prod(e1,e2)  -> 
        let (I n1) = eval e1 env
        let (I n2) = eval e2 env
        I (n1 * n2)
    | Eq(n,m) -> 
        let v1 = eval n env
        let v2 = eval m env
        B (v1 = v2) 
    | Less(n,m) ->  
        let (I v1) = eval n env
        let (I v2) = eval m env
        B (v1 < v2) 
    | Neg b ->  
        let (B nb) = eval b env
        B (not nb);;   


let p1 = eval et anenv;;

(*

 | Sum(e1,e2) -> let (I n1) = eval e1 env
  ------------------------^^^^^^

 warning FS0025: Incomplete pattern matches on this expression. For
 example, the value 'B (_)' may indicate a case not covered by the
 pattern(s).

What's going on here?

Incomplete pattern match signals our wishful thinking: we "know" that
eval should return an integer, but nothing is stopping us from doing
something silly such as negating an integer or summing two bools *)

let stupid = eval (Neg (I 3)) Map.empty;;


(*
This raises a Microsoft.FSharp.Core.MatchFailureException, (because
the F# compiler implicitely adds raise MatchFailureException to non
exhaustive patterns)

Another small issue: env may **not** contain the value of a certain key:

*****************************************

We rewrite the interpreter to make it "defensive", so that it raises
exceptions if called on the wrong arguments (or the variable is
undefined).

First we introduce some exceptions (we do not handle them yet, though )
*)

exception NotABool;;
exception NotAInt;;
exception NotSameType;;
exception UndefVar of string;;

///  defensive evaluation
let rec evald e env =
    match e with
    | I n     ->  I n
    | B b     ->  B b
    | V s     -> 
        try
            Map.find s env 
        with
            | :? System.Collections.Generic.KeyNotFoundException -> raise (UndefVar s) 
  
    | Sum(e1,e2) -> 
        match(evald e1 env, evald e2 env) with
           | (I n1,I n2) -> I (n1 + n2)
           | (_,_) -> raise NotAInt 
    | Diff(e1,e2)  -> 
        match(evald e1 env, evald e2 env) with
           | (I n1,I n2) -> I (n1 - n2)
           | (_,_) -> raise NotAInt                                         
    | Prod(e1,e2)  -> 
        match(evald e1 env, evald e2 env) with
            | (I n1,I n2) -> I (n1 * n2)
            | (_,_) -> raise NotAInt 
    | Eq(n,m) -> 
        match(evald n env, evald m env) with
            | (B b1,B b2)  -> B (b1 = b2)
            | (I n1,I n2)  -> B (n1 = n2)
            | (_,_) -> raise NotSameType  
    | Less(e1,e2)  -> 
        match(evald e1 env, evald e2 env) with
            | (I n1,I n2) -> B (n1 < n2)
            | (_,_) -> raise NotAInt 
    | Neg b ->  
        match(evald b env) with
            (B nb) -> B (not nb)
            | _ -> raise NotABool;; 
(*
***********************************************
OK, now we get no warnings

    Happy puppy?

Not quite

This is called "dynamic typing" and basically is what happens in
languages such as Pearl, Python, Prolog, Lisp etc.

This is bad because

1. It is inefficient: we keep checking (class) tags at RUN-TIME 
2. It makes the code complicated: here just two tags (two types), but think about real languages
3. It's pointless: these checks can be done at COMPILE-TIME

Hence, we will do this as TYPE-CHECKING

        THE PHASE DISTINCTION !!!!

Aside: it's true that in this toy language we could avoid run time
checks separating boolen expressions from arithmetic ones, or we could
interpret booleans as ints, but this is a general point


Why bother with types? Because they prevent mistakes. They are a
simple, automatic way to find obvious problems in programs before
these programs are ever run.

There are 3 kinds of types.

* The Good: Static types that guarantee absence of certain runtime faults.

* The Bad: Static types that have mostly decorative value but do not guaran-
tee anything at runtime.

* The Ugly: Dynamic types that detect errors only when it can be too late.

Examples of the first kind are Java, F# and Haskell. In Java for instance,
the type system enforces that there will be no memory access errors, which in
other languages manifest as segmentation faults. F# and Haskell have even
more powerful type systems that can be used to enforce basic higher-level
program properties by type alone, for instance strict information hiding in
modules or abstract data types.

Famous examples of the bad kind are C and C++. These languages have
static type systems, but they can be circumvented easily. The language
specification may not even allow these circumventions, but there is no
way for compilers to guarantee their absence.

Examples for dynamic types are scripting languages such as Perl and
Python. These languages are typed, but typing violations are
discovered and reported at runtime only, which leads to runtime
messages such as “TypeError: . . . ”

The ideal for a static type system is to be permissive enough not to
get into the programmer’s way while being strong enough to achieve
Robin Milner’s slogan

          "Well-typed programs cannot go wrong"

 What could go wrong? Some examples of common runtime errors are:
 
- corruption of data,
- null pointer exceptions,
- nontermination,
- running out of memory,
- leaking secrets...

There exist type systems for all of these, and more, but in practise
only the first is covered in widely-used languages such as Java, C#,
Haskell,

*)

type tp = INT | BOOL;;

type tenviroment = Map<string,tp>;;

(* we define type checking as  function that returns the type of an an exp if
 well-typed in a typing context, otherwise it return an error message
 (with failwithf, for a change)

 tpchk : exp -> tenviroment ->  tp

Judgment: tenv |- e : t

** to myself: write the rules!!
*)

let rec tpchk e (tenv : tenviroment) = 
    match e with
     V s -> 
       try
            Map.find s tenv 
        with
            | :? System.Collections.Generic.KeyNotFoundException -> failwithf "Variable %s NOT in %A" s tenv  
                
    | I(_) -> INT
    | B(_) -> BOOL
    | Sum(e1,e2) | Prod(e1,e2) | Diff(e1,e2) -> 
                    let (t1,t2) = (tpchk e1 tenv, tpchk e2 tenv)
                    if t1 = INT && t2 = INT 
                        then INT
                            else failwithf "%A and/or %A not an int" t1 t2
    | Eq(e1,e2)  -> 
       match (tpchk e1 tenv, tpchk e2 tenv) with
       |  t1,t2 when  t1 = t2 -> BOOL
       | t1 , t2 -> failwithf "%A : %A, but %A : %A" e1 t1 e2 t2
    | Less(e1,e2)  -> 
        let (t1,t2) = (tpchk e1 tenv, tpchk e2 tenv)
        if t1 = INT && t2 = INT  then BOOL
        else failwithf "%A : %A, but %A : %A" e1 t1 e2 t2
    | Neg b -> 
        match (tpchk b tenv) with
            | BOOL -> BOOL
            | t -> failwithf "%A not a boolean" t;;  
 
// the same code but with options, so less informative errors

let rec tpchko e tenv  = 
    match e with
     V s -> (Map.tryFind s tenv)
    | I(_) -> Some INT
    | B(_) -> Some BOOL
    | Sum(e1,e2) | Prod(e1,e2) | Diff(e1,e2) -> 
                       match (tpchko e1 tenv, tpchko e2 tenv) with
                        Some INT, Some INT -> Some INT
                        | _ ->  None
                            
    | Eq(e1,e2)  -> 
        match (tpchko e1 tenv, tpchko e2 tenv) with
                    Some t1, Some t2 when t1 = t2 -> Some BOOL
                    | __-> None

    | Less(e1,e2)  ->  
        match (tpchko e1 tenv, tpchko e2 tenv) with
            Some INT, Some INT -> Some BOOL
            | _ ->  None 
    | Neg b -> 
        match tpchko b tenv with
            Some BOOL -> Some BOOL
            | _ -> None
               

let atenv = Map.add "x" INT (Map.add "a" INT Map.empty);;
let p3 = tpchk et atenv;; 
let t4 = tpchko et atenv;; 


let et2 =    (V "x" ++  V "a") == (I 2);;
let p4 = tpchk et2 atenv;; 

let et3 =    (V "x" ++  V "a") == (B true);;


let notty = tpchko et3 atenv

// let nottyfail = (tpchk et3 atenv)

// Let's put the two toghether: evaluating only well typed programs
// tc_ev : e:exp -> tenv:tenviroment -> env:Map<string,exp> -> unit

let main  e tenv env = 
    try
        let t = tpchk e tenv
        let v = eval e env
        sprintf "%A has type: %A and value: %A\n"  e t v
    with 
        | Failure s ->  "ERROR: "+ s

let ok1 = main  et2 atenv anenv;; 
let notok1 = main et3 atenv anenv;; 


// some metatheory

let value = function
    I _ | B _ -> true
    | _ -> false

#r "FsCheck"
open FsCheck

let wellTypedTermsOf =
        let isWellTyped e = tpchko e Map.empty |> Option.isSome
        Arb.mapFilter (fun x -> x) isWellTyped Arb.from<exp>


let prop_pres =
        Prop.forAll wellTypedTermsOf <| fun e ->
            let t =  tpchko e Map.empty
            let v = eval e Map.empty
            t = tpchko v  Map.empty



let config = { Config.Quick with MaxTest = 500; StartSize = 1; EndSize = 10}
do Check.One(config,prop_pres)


// monadic let. 
// letx : e:'a option -> fx:('a -> 'b option) -> 'b option
let letx e fx =
    match e with
        None -> None
        | Some x -> fx x

// "monadic" passing of maybe's
let rec tpchkf e tenv  = 
    match e with
     V s -> (Map.tryFind s tenv)
    | I(_) -> Some INT
    | B(_) -> Some BOOL
    | Sum(e1,e2) | Prod(e1,e2) | Diff(e1,e2) -> 
                       letx (tpchkf e1 tenv) (fun t1 ->
                       letx (tpchkf e2 tenv) (fun t2 -> 
                       if t1 = INT && t2 = INT then Some INT else None))
                              
    | Eq(e1,e2)  -> 
        letx (tpchkf e1 tenv) (fun t1 ->
        letx (tpchkf e2 tenv) (fun t2 -> 
              if t1 = t2 then Some BOOL else None))

    | Less(e1,e2)  ->  
        letx (tpchkf e1 tenv) (fun t1 ->
        letx (tpchkf e2 tenv) (fun t2 -> 
              if t1 = INT && t2 = INT then Some BOOL else None))
     | Neg b -> 
         letx (tpchkf b tenv) (fun t-> if t =  BOOL then Some BOOL else None)
           


let prop_tt tenv e=
    tpchkf e tenv = tpchko e tenv
do Check.Quick (prop_tt Map.empty)

// and here we should introduce the Maybe monad but see chapter 13 of the book