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


// CONFRONTO CON VERSIONE RICORSIVA
let rec suml = function
    | [] -> 0
    | x :: xs -> x + suml xs;;

(*
    Nell'interprete dare il comando 

     #time

    che, dopo ogni computazione, stampa alcuni dati sulle risorse utilizzate 
    (tempo CPU, uso garbage collector, ecc.)

    Provare ad eseguire delle chiamate della forma

      suml [ 1 ..K ]      
     isuml [ 1 .. K ]

    con K intero grande a piacere.
    Tenere presente che le liste sono costruite nello heap.
*)
#time
suml [1..10000000];;
#time
#time
isuml [1..10000000];;
#time

(*
---> versione iterativa: Real: 00:00:04.341, CPU: 00:00:04.500, GC gen0: 71, gen1: 6
---> versione ricorsiva: Stack Overflow
*)