// %% MOTIVATING FsCheck %%
   
// alcune funzioni standard su liste che abbiamo visto lunedì scorso

let rec append (xs, ys) = 
    match xs with 
    | [] -> ys 
    | z::zs -> z :: append (zs, ys) 

let rec rev ls =  
    match ls with 
    | [] -> [] 
    | x :: xs -> append (rev xs, [x]) 

(* Come è che ci convinciamo che sono corrette? Facciamo qualche (unit) test!  *)

let a1 = append ( [ 1 ; 2] , [ 3 ; 4 ; 5 ] ) ;; //  [1; 2; 3; 4; 5]

let r1 = rev [1 .. 10] ;; //  [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]


(* A questo punto, ci diciamo "è tipata, ha superato i test (??), andiamo avanti"

Possiamo fare meglio?

===>  scriviamo delle **proprietà** che il nostro metodo deve soddisfare

- es: rev è involutiva -- una funzione è involutiva se f (f (x)) = x

Nota: Per iniziare, una proprietà è una funzione booleana -- in
seguito introduciamo un semplice DSL per scrivere specifiche.  *)

let prop_revRevIsOrig (xs:int list) =
  rev (rev xs) = xs;;

// val prop_revRevIsOrig : xs:int list -> bool


// A questo punto, faccio degli unit test sulla proprietà

let t1 = prop_revRevIsOrig [1..40]
let t2 = prop_revRevIsOrig []
let t3 = prop_revRevIsOrig [1]

// Che noia! Non possiamo automatizzare? Yes we can -- FsCheck

#r "FsCheck";;

open FsCheck;;

do Check.Quick prop_revRevIsOrig ;;
// verbosamente

do  Check.Verbose prop_revRevIsOrig ;;

// Che cosa possiamo concludere? Che la nostra funzione "soddisfa" la
// nostra spec (algebra è oracolo), o meglio che ha superato 100 test 
// Non è una dimostrazione, ma una **validazione**


// Proviamo a scrivere una proprietà **falsa**


let prop_revIsOrig (xs:int list) =
  rev xs = xs
do Check.Quick prop_revIsOrig ;;

(*
   Falsifiable, after 2 tests (2 shrinks) (StdGen (470885551,296131751)):
Original:
[-2; 0]
Shrunk:
[1; 0]

Ci dice:

- il controsempio originale, che falsifica la proprietà

- il controsempio ** più piccolo ** ( shrinking)  (più dettagli dopo)

- il random seed, nel caso volessi replicare il test

Quindi abbiamo due casi (non mutuamente esclusivi)

1. La proprietà è falsa
2. Il codice è bacato


   *)


// BACK TO SLIDES !!!

// In generale, posso usare back ticks per nomi più significativi in
// una let declaration e in particolare:
let ``reversing a list yields the same list``  (xs:int list) =
  rev xs = xs
do Check.Quick ``reversing a list yields the same list``


(*
INTERMEZZO: tipo unit

- Il tipo di Check.Quick è 'a -> unit, che prende un 'a e non ritorna
nulla di interessante come valore, ma segnala un effetto collaterale,
qui testing

- unit corrisponde a void in Java. Può essere visto come il caso
zero-ario del cartesiano, cioè il tipo che contiene solo la tupla con zero
elementi, scritta '()'.

Esempio: input da console
> System.Console.ReadLine :  unit -> string
*)

   
// DOMANDE: 
// 1. Tutto così facile? Fscheck risolve la questione della qualità del software?

// 1.1 Ribadiamolo: Non è una dimostrazione, ma una validazione 
// 1.2 Fidarsi bene, non fidarsi ....

let prop_what x =
  x < 80
do Check.Quick prop_what

// ma è ovviamente falso ... guardiamoci dentro
do Check.Verbose prop_what

// bella forza, siamo attaccati allo zero ... Il punto è che dobbiamo
// essere consapevoli (da subito) che la **distribuzione** dei dati è
// rilevante e FsCheck offre primitive per monitorarla e modificarla


//DOMANDE:  Da dove vengono queste proprietà? Certo, l'algebra è
// un'ispirazione, ma esistono "categorie" o "patterns" di proprietà?
// Lo vedremo in una lezione successiva


// Altre proprietà? Append è una operazione monoidale, scriviamole insieme ...

// TODO in class

// E' un monoide commutativo?

let prop_comm (xs : int list, ys) =
   append(xs, ys) = append(ys, xs)

do Check.Quick  prop_comm


// "cross-unit testing": testiamo una proprietà che collega rev e append

let prop_rev_app (xs : int list, ys) = 
    rev (append (xs , ys)) = append (rev ys, rev xs)

do Check.Quick prop_rev_app


//  Fino ad ora abbiamo annotato i tipi dei parametri nelle
//  proprietà. Polimorofismo? Viene gestito?


let prop_RevRevp xs =
  rev (rev xs) = xs 
 
// Si, però ...  Potete scrivere properietà polimorfe, ma in realtà
// sono istanziate con 'obj'
do Check.Verbose prop_RevRevp;;


// ... e questo "rompe" il type checking poiche' tutto viene cast in obj
// Quindi tanto vale renderla monoforma, ma attenzione ....

let prop_RevRevf (xs : float list) =
  rev (rev xs) = xs 

do Check.Verbose prop_RevRevf;;


// nan : float is a special value for "not a number", similarly
// infinity : float. This fails because of the way structural equality works in FP 

let bb =  prop_RevRevf [nan];;
let b = (nan = nan);;

// Morale: per testare proprietà polimorfe basta instanziare a bool

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// SHRINKING

// Let's go back to our false property revIsOrig and look at "shrink":

do Check.Quick  prop_revIsOrig


(* To actually understand what went wrong, it is crucial, wrt random
generation of values, to get the smallest possible cex.

FsCheck shrinks the counter example: it tries to find the minimal
counter example that still fails the property. In this case, the
counter example is indeed minimal: the list must have at least two
different elements for the test to fail.

FsCheck displays how many times it found a smaller (in some way)
counter example and so proceeded to shrink further. Note that it also
shrinks integers ...

Not all QC implementations do shrinking. 

More (not so exciting details)  on how shrinking works at
http://fsharpforfunandprofit.com/posts/property-based-testing/

 > Understanding FsCheck: Shrinking
*)





(*
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 Testing against a model

Another technique for gaining confidence in some code is to test it
against a model implementation, where one is the reference
implementation and the other the "new" and possibly improved one. We
can tie our implementation of function on lists to the reference
functions in the standard list theory, and, if they behave the same,
we gain confidence that they do the right thing*)

// linear reverse
let bfrev xs =
    let rec rev_aux (xs, acc) =
        match xs with 
        | [] -> acc
        | y ::  ys -> rev_aux (ys, y :: acc)
    rev_aux (xs, [])

// le due reverse si comportano allo "stesso" modo
let prop_rev_qrev (xs :int list) =
  bfrev xs = rev xs

do Check.Quick prop_rev_qrev

// la mia append "vale" la List.append

let prop_app_mapp (xs :unit list, ys) =
  append (xs, ys) = xs @ ys

do Check.Quick prop_app_mapp

// This is so useful, but it gets a little tricky if you hide your
// code in a library, ie. a dll For example you are providing an new
// implementation of Sets. In this case the data structure is not
// accessible from the outside and hence you cannot check it against
// the usual Set collection. What you can do is


 // ** Export a generator (later)  
 // ** More simply, provide API for converting to and from standard "common" collections such as List*)

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// Conditional  properties: from bool to Property

// Properties may take the form <condition> ==> <property>.  Such a
// property holds if the property after ==> holds whenever the
// condition does.  


// is  a list ordered?
let rec ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered ys

// inserting an element into an ordered list
let rec insert (x,  xs) = 
    match xs with
    | [] -> [x]
    | c::_ when x <= c -> x::xs 
    | c::cs ->  c:: insert( x, cs)
 
// the property we want to check is whether insertion preserves an ordering property
let prop_insert (x:int,  xs) =
  ordered xs ==> ordered (insert (x, xs))

// val prop_insert : x:int * xs:int list -> Property

do Check.Quick prop_insert


// Oops --- FIX THIS !

let rec orderedr xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && orderedr (y ::ys)

let prop_insertOK (x:int,  xs) =
  orderedr xs ==> orderedr (insert (x, xs))


do Check.Quick (prop_insertOK)



//Note: "Arguments exhausted after --- tests."
// First time we meet a **coverage** issue

do Check.Verbose (prop_insertOK)

(* Testing discards test cases which do not satisfy the
condition. Test case generation continues until 100 cases which do
satisfy the condition have been found, or until an overall limit (MaxTest) on
the number of test cases is reached (to avoid looping if the condition
never holds). In this case a message such as "Arguments exhausted
after n tests." indicates that n test cases satisfying the condition
were found, and that the property held in those n cases.

So what the system does is keep trying at each round to find something
which is sorted,  event quite unlikely over non sigleton lists.

There are better ways that we shall see in details in other lectures

 Aside the above example also illustrates the benefit of writing the
property as p ==> q instead of using the boolean operator || to write
not p || q. In the latter case, there is a flat predicate, and QC
doesn't know what the precondition is, so a property may hold
vacuously. For example consider the variant *)

let prop_insert_ordered_vacuous (x : int , xs) = 
  not (orderedr xs) || orderedr (insert (x ,xs))

// QC will happily check it for us

do Check.Quick prop_insert_ordered_vacuous

(* Unfortunately, in the above, the tests passed vacuously only
because their inputs were not ordered, and one should use ==> to avoid
the false sense of security delivered by vacuity.  *)

// Condizioni possono essere composte

let prop_addIsNotMult (x, y) =
    ((x,y) <> (0,0) && (x,y) <> (2,2))    ==> (x + y <> x * y )
     
do Check.Quick prop_addIsNotMult

// %%% conditions and lazyness 
   
// il primo elemento di una lista ordinata è il minimo
let prop_fsm (xs : int list) =
  (List.sort xs |>  List.head) = List.min xs

do Check.Quick prop_fsm

// Ah, la lista vuota ... Secondo tentativo:


let prop_fsm2 (ys : int list) =
  (ys <> []) ==>  ((List.sort ys |>  List.head) = List.min ys)


do Check.Quick prop_fsm2

// A volte, per usare conditional checking devo usare **lazy evaluation**
// Per capire cosa succede, un esempio classico



(* Since F# has eager evaluation by default, a property does
more work than necessary: it evaluates the property at the right of
the condition no matter what the outcome of the condition on the
left. While only a performance consideration in the other  example,
this may limit the expressiveness of properties*) 


// uso la keyword 'lazy', per ritardare la valutazione del RHS

let prop_MS2 (ys : int list) =
  ys <> [] ==>
  lazy ((List.sort ys |>  List.head) = List.min ys)

do Check.Quick prop_MS2



// ESERCIZIO: scrivere una proprietà simile per List.max


// %%% WEAK & STRONG SPECS

// considera la spec "se sorto una lista, è ordinata"
let prop_so ordP (xs : int list)   =
  List.sort xs |>  ordP

do Check.Quick (prop_so orderedr)
do Check.Quick (prop_so ordered)

  (* Attenzione! Ho usato la specifica "sbagliata" di ordered, ma è
  andata bene. E' una specifica lasca, ma non scorreta: ordered
  contiene liste ordinate e non ordinate, quindi se xs è sortata, ci
  sta dentro (soundness)

  Questa proprietà invece di "completezza" invece discrimina tra buona
 e cattiva implementazione: *)

let prop_os ordP (xs : int list)  =
   ordP xs ==> (List.sort xs = xs)

do Check.Quick (prop_os orderedr)
do Check.Quick (prop_os ordered)

// Nessuna specifica è immune da banalizzazioni: per es, se prendo al
// post di sort la funzione constante su lista vuota o singoletto,
// ambo specifiche passano


// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// Combining checks. 

let add(x,y) = x + y 

// use a list of specs adding a flag to have an idea of what went wrong
 
let prop_addspec (x: int, y, z) =
    [add(x,y) = add (y, x)    |@ "comm";
     add (x , add (y, z)) = add (add(x,y), z)    |@ "assoc";
     add (x, 0) = x |@ "leftId";
     add (0, x) = x |@ "RightId"
    ]

do Check.Quick prop_addspec

// Let's go crazy

let add(x,y) = x - y


// see what I mean?


let prop_addspec2 (x: int, y, z) =
    [add(x,y) = add (y, x)    |@ "comm";
     add (x , add (y, z)) = add (add(x,y), z)    |@ "assoc";
     add (x, 0) = x |@ "leftId";
     add (0, x) = x |@ "RightId"
    ]
do Check.Quick prop_addspec2
