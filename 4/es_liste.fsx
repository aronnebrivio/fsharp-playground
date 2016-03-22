(*
1.
	Funzione per rimuovere tutti gli elementi della lista pari.
	[] -> []
	[x] -> [x]
	x::y::xs -> remEven (x::xs)
*)
let isEvenList x =
	match x%2 with
	| 0 -> []
	| _ -> [x];;

let rec remEven ls =
	match ls with
	| [] -> []
	| [x] -> isEvenList x
	| x::xs -> isEvenList x @ remEven xs;;

// test
let l1 = remEven [-10..10];;
// => val l1 : int list = [-11; -9; -7; -5; -3; -1; 1; 3; 5; 7; 9]

(*
2.
    rmOddPos :  a' list -> 'a list
	che cancella tutti gli elementi di una lista in posizione dispari;
	il primo elemento della lista ha posizione 0.	
	[] -> []
	[x] -> [x]
	x::y::xs -> remOddPos (x::xs)
*)
let rec remOddPos ls =
	match ls with
	| [] -> []
	| [x] -> [x]
	| x::y::ys -> x::remOddPos (ys) ;;

// test
let l2 = remOddPos [1..10];;
// => val it : int list = [1; 3; 5; 7; 9]

(*
3.
	Definire la funzione 

	   split : 'a list -> 'a list * 'a list

	che, data una lista, costruisce la coppia di liste 
	degli elementi in posizione pari e in posizione dispari.
	[] -> ([],[])
	[x] -> ([x],[])
	x::y::zs -> 

*)
let rec remEvenPos ls =
	match ls with
	| [] -> []
	| [x] -> []
	| x::y::zs -> y::remEvenPos(zs) ;;

// uso remEvenPos e remOddPos
let split ls =
	match ls with
	| [] -> ([],[])
	| [x] -> ([x],[])
	| x::y::zs -> (x::remOddPos(zs) , y::remEvenPos(zs));;

// test
let l3 = split [1..10];;
// => val l3 : int list * int list = ([1; 3; 5; 7; 9], [2; 4; 6; 8; 10])

(*
4.
	Definire la funzione ricorsiva 

	  cmpLength : 'a list * 'b list -> int

	che, data una coppia di liste (ls0,ls1) confronta le lunghezza
	(length) delle liste e restituisce:

	   -1    se  length(ls0) < length(ls1)
	    0    se  length(ls0) = length(ls1)
	    1    se  length(ls0) > length(ls1)
*)
let rec listLen ls =
	match ls with
	| [] -> 0
	| x::xs -> 1 + listLen(xs);;

let rec cmpLength (ls1,ls2) =
	match (ls1,ls2) with
	| ([],[]) -> 0
	| (x::xs, []) -> 1
	| ([],x::xs) -> -1
	| (x::xs, y::ys) -> cmpLength(xs,ys);;


(*
5.
	Definire la funzione ricorsiva

	   remove : ('a * 'a list) -> 'a list when 'a : equality 

	che dato un elemento x e una lista ls, restituisce la 
	lista ottenuta da ls eliminando tutte le occorrenze di x.
*)