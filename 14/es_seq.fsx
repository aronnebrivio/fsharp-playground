let sq1 = seq {
  yield 0  // yield genera un elemento
  yield 1
  yield 2
  yield 3
  } ;;

// sq1 : seq<int> definisce la sequenza seq [ 0; 1; 2; 3 ]

let sq2 = seq{
  yield 100
  yield! sq1  // yield! aggiunge tutti gli elementi di sq1
  yield 200
  yield! sq1
  } ;;

(*
i) Usando delle opportune sequence expression, definire la funzioni

     cons : 'a ->  seq<'a> -> seq<'a>
   append : seq<'a> -> seq<'a> -> seq<'a>

	analoghe alle omonime funzioni su liste.
	Verificare che:

	- la lista degli elementi nelle sequenza  'cons 100 sq2' e'
	   [100; 100; 0; 1; 2; 3; 200; 0; 1; 2; 3]
	- la lista degli elementi nelle sequenza 'append sq1 sq1' e'
	   [0; 1; 2; 3; 0; 1; 2; 3]
*)
let cons x sq = seq {
    yield x
    yield! sq
};;

let append sq1 sq2 = seq {
    yield! sq1
    yield! sq2
};;

let head sq = Seq.item 0 sq;;

let tail sq = Seq.skip 1 sq;;

(*
    Usando  Seq.initInfinite definire le seguenti sequenze infinite:

    - nat : sequenza dei numeri naturali 0, 1, 2, ...
    - nat1: sequenza dei numeri naturali senza il numero 5
    - nat2: sequenza dei numeri naturali in cui il numero 5 e' sostituito da -5
    - even10 : sequenza dei numeri pari n >= 10  
    - sqTrue : sequenza costante true, true, true, ....
    - sqTrueFalse: sequenza true, false, true, false, true, false, ...

    Per ciascuna sequenza generare la lista dei primi 10 elementi.
*)
let nat = Seq.initInfinite (fun x -> x);;

let nat1 = seq {
    yield! [0..4]
    yield! Seq.skip 6 nat
};;

let nat2 = seq {
    yield! [0..4]
    yield -5
    yield! Seq.skip 6 nat
};;

let even10 = seq {
    yield! (Seq.skip 5 (Seq.initInfinite (fun x -> x*2)))
} ;;

let sqTrue = Seq.initInfinite (fun x -> true);;

let sqTrueFalse = Seq.initInfinite (fun x -> if x%2=0 then true else false);;