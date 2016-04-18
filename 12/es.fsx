(*
	Definire la funzione iterativa

	    isuml :  int list -> int

	che calcola la somma degli elementi di una lista di interi.

	Usare la funzione ausiliaria  

	     sumlA : int list * int   -> int

	in cui nel secondo parametro viene accumulata la somma degli elementi.
*)
let isuml ls =
    let rec sumlA (ls,acc) =
        match ls with
        | [] -> acc
        | x::xs -> sumlA (xs, acc+x)
    sumlA(ls,0);;

// test
isuml [1..10];; // 55

