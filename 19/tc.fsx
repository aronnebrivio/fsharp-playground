type tp = INT | LSTINT ;;

type exp =
    K of int
    | Plus of exp * exp
    | Nil
    | Cons of exp * exp
    | Hd of exp
    | Tl of exp ;;

// 1)
let rec tpck exp =
    match exp with
    | K(x) -> Some INT
    | Plus(e1,e2) -> 
        let (t1,t2) = (tpck e1, tpck e2)
        if t1 = Some INT && t2 = Some INT then Some INT
            else None
    | Nil -> Some LSTINT
    | Cons(e1,e2) ->
        let (t1,t2) = (tpck e1, tpck e2)
        if t1 = Some INT && t2 = Some LSTINT then Some LSTINT
            else None
    | Hd(x) -> 
        let t = tpck x
        if t = Some LSTINT then Some INT
            else None
    | Tl(x) ->
        let t = tpck x
        if t = Some LSTINT then Some LSTINT
            else None ;;

#r "FsCheck"
open FsCheck

// size è la massima dimensione di <e>, len la lunghezza della lista di esempi generata
let test size len =
  let exps = (Gen.sample size len Arb.generate<exp>)   
  List.map2 (fun x y -> printf "%A has type %A\n" x y) exps (List.map tpck exps);;

(*
2.  Riscrivere la funzione di test in modo 
 i. se un termine non ha un tipo, non scrivo "<e> has type null", ma
 "<e> is not typable"
ii. se un termine ha tipo Some <tp>, elimino nella stampa il "Some"

 (hint: mappare una funzione che fa pattern matching su (tpck e) e a
 seconda del risultato fa una specifica printf) 
*)
let test2 size len =
    let exps = (Gen.sample size len Arb.generate<exp>)
    List.map2 (fun x y -> if Option.isSome y then printf "%A has type %A\n" x (Option.get y)
                            else printf "%A is not typable\n" x) exps (List.map tpck exps);;

(*
3. Riscrivere il type checker usando eccezioni. 

 tpckf : exp -> tp

Dove ci sarebbe un errore di tipo si sollevi una opportuna eccezione
che si porti dietro quei valori necessari a dare un messaggio di
errore significativo. Per esempio:

exception TlERR of (exp * tp) 

descrive l'errore ottenuto cercando di tipare una expressione  Tl <exp>
con <tp>, dove <tp> non è LISTINT. Quindi bisogna dichiarare un certo
numero di eccezioni con gli argomenti corretti e sollevarle oppotunamente.
*)
exception NotAnInt;;
exception PlusERR of (exp * exp * tp * tp);;
exception ConsERR of (exp * exp * tp * tp);;
exception HdERR of (exp * tp);;
exception TlERR of (exp * tp);;

let rec tpckf2 exp =
    match exp with
    | K(x) -> INT
    | Plus(e1,e2) -> 
        let (t1,t2) = (tpckf e1, tpckf e2)
        if t1 = INT && t2 = INT then INT
            else failwithf "%A and/or %A not an int" e1 e2
    | Nil -> LSTINT
    | Cons(e1,e2) ->
        let (t1,t2) = (tpckf e1, tpckf e2)
        if t1 = INT && t2 = LSTINT then LSTINT
            else failwithf "%A and/or %A not an int - list of int" e1 e2
    | Hd(x) -> 
        let t = tpckf x
        if t = LSTINT then INT
            else failwithf "%A not a list of int" x
    | Tl(x) ->
        let t = tpckf x
        if t = LSTINT then LSTINT
            else failwithf "%A not a list of int" x;;

let rec tpckf exp =
    match exp with
    | K(x) -> INT
    | Plus(e1,e2) -> 
        match (tpckf e1, tpckf e2) with
        | (INT,INT) -> INT
        | (t1,t2) -> raise (PlusERR (e1,e2,t1,t2))
    | Nil -> LSTINT
    | Cons(e1,e2) ->
        match (tpckf e1, tpckf e2) with
        | (INT,LSTINT) -> LSTINT
        | (t1,t2) -> raise (ConsERR (e1,e2,t1,t2))
    | Hd(x) -> 
        match (tpckf x) with
        | LSTINT -> INT
        | t -> raise (HdERR (x,t))
    | Tl(x) ->
        match (tpckf x) with
        | LSTINT -> LSTINT
        | t -> raise (TlERR (x,t));;

