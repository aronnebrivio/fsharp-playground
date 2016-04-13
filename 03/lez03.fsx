// LEZIONE 3: LET + RIC


// let globale
let x = 3;;


// globale + locale
let x2 =
    let y = 5 // y e' definita localmente (non visibile all'esterno)
    y * y;;  // valore associato a  x1

//  y;;
// The value or constructor 'y' is not defined


// verbose: 
//  LET id = exp IN exp

let x1 = let y = 5 in  y * y ;;

// in caso di ri-uso dello stesso nome in let locale, fa testo il legame globale

x;;

let x = 22 in x * x;;

x;;

// let multipli = dichiarazioni

let x3 =
    let y = sin 4.
    let z =  cos (float x1)
    y + z;;

// più noioso in verbose    (nota indentazione per il lettore)

let x4 =
    let y = sin 4. in
       let z = cos (float x1) in
         y + z;;

// shadowing:

let w = 1

// nota "parser error"
let w = 10

let r = w * w
//il vecchio valore è dimenticato, "ombreggiato" da quello più recente


////////////////////////////////////////

//// CORREZIONE ESERCIZIO IVA


/// BACK TO SLIDES per PM

// Nota: let e PM

let f e =
  let x,y = e in x + y

let f' e =
  match e with
    x,y -> x + y

// quest'ultima è la forma "primitiva"

// %%%%%%%%%%%%  RICORSIONE

(*

Dato un intero n >= 0,  n! (fattoriale di n) puo' essere definito ricorsivamente
nel modo seguente:

n!   =  0                              se  n = 0

     =  1 * 2 * ... * (n-1) * n         se  n > 0

Questo genera una "formula di ricorsione"

n!   =  1                   se  n = 0

     =  n  * (n-1)!         se  n > 0


- caso base e caso passo
- in questo caso terminante (è ricorsione primitiva), ma in generale terminazione indecidibile

La traduzione in codice è immediata, si noti il let rec

   *)

let rec fact m =
    match m with
        | 0 -> 1
        | n -> n * fact ( n-1 );;



// si noti che l'ordine delle pattern (cronologico) conta: 

let rec fact' m =
    match m with
        | n -> n * fact' ( n-1 )
        | 0 -> 1

// notate il warning: cosa succede se eseguo fact' 2 ?

// funzioni ricorsive "su più argomenti"

(*

   2)  Dato un float x e un intero n >= 0,  la funzione esponenziale puo'
essere definita ricorsivamente come:

x^n  = 1              se n = 0 

    =  x * x^(n-1)    se n > 0

*)

let rec power ( bse, m ) =
    match (bse,m) with
        | _,0  -> 1.
        | x, n  ->  x *  power (x, n-1);;


// più semplicemente, ricorrendo solo su m 
let rec exp ( bse, m ) =
    match m with
        | 0  -> 1.
        | n  ->  bse *  exp (bse, n-1);;


(* Definire una funzione ricorsiva

make_str : int -> string

che, dato un intero n>=0, restituisce la stringa "0 1 2 ... n"


 make_str 20;; 
> val it : string = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"
> 
*)

let rec make_str = function
    | 0 -> "0"
    | n -> make_str ( n - 1) + " " +  (string n);;

 make_str 23;;    



(* alcune funzioni su stringhe dalla classe .NET String


- coercizione: string : (obj -> string)
- string concatenation: + (overloaded)
- lunghezza: String.length  : (string -> int)
- estrazione del i-th (i in 0.. s.length) carattere: s.[i]

https://msdn.microsoft.com/en-us/library/ee353758.aspx
*)


