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

