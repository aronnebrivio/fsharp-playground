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

