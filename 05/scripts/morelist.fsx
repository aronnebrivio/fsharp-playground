(*

Esempio 2
^^^^^^^^^

Definire la funzione ricorsiva

  sumprod : int list -> int * int
  
che data una list ls restituisce la coppia (sum,prod),
dove sum e' la somma degli elementi della lista
e prod e' il prodotto degli elementi della lista.

Si noti che siccome la funzione ritorna una coppia, devo analizzare il
risultato della chiamata ricorsiva con un let. 

*)

let rec sumprod ls =  
    match ls with 
        | [] -> (0,1) 
        | x::xs ->  
            let (sum,prod) = sumprod xs 
            (x + sum, x * prod) ;;


let sp = sumprod [1..10] ;; // (55, 3628800)

 
(*

Esempio 3
^^^^^^^^^

Definire la funzione ricorsiva (polimorfa)

     len : 'a list -> int

che calcola la lunghezza di una lista

Vedi anche List.length

*)
 
 
let rec len ls =  
    match ls with 
    | [] -> 0 
    | _::xs -> 1 + len xs 
 
len [3..7] ;;   // 5
len ['a'..'z'] ;;  // 26

(*

Notare che anche la soluzione

let rec len ls =  
    match ls with 
    | [] -> 0
    | [_] -> 1
    | _::xs -> 1 + len xs 

e' corretta.

Tuttavia non c'e' nessun motivo per trattare a parte il caso [x]
(lista con un solo elemento).  Soluzioni di questo tipo, in cui si
aggiungono casi particolari inutili, vanno evitate

*)   



 
(*


Esempio 4
^^^^^^^^^

Definire la funzione ricorsiva (polimorfa)

     append : 'a list * 'a list -> 'a list

che concatena due liste. Quindi

   
       append(l1, l2) = lista contenente gli elementi di l1
                        seguiti dagli elementi di l2
                          

La funzione append e' gia' implementata in F# dall'operatore @,
che e' un operatore infisso con associativita' a destra.

Esempio

 [1 ; 2 ] @ [ 3 ; 4 ] =  [ 1 ; 2 ; 3 ; 4 ]

 [1 ; 2 ] @ [ 3 ; 4 ] @ [5]  =  [ 1 ; 2 ; 3 ; 4 ; 5 ]


*) 
 
let rec append (xs, ys) = 
    match xs with 
    | [] -> ys 
    | z::zs -> z :: append (zs, ys) 
 
 
append ( [ 1 ; 2] , [ 3 ; 4 ; 5 ] ) ;; //  [1; 2; 3; 4; 5]

(*

NOTA SU  ::  e   @
^^^^^^^^^^^^^^^^^^

Attenzione a usare correttamente gli operatori :: (cons) e @ (append)

  ::     ha  operandi di tipo T e 'T list'
   @     ha entrambi gli operandi 'T list'.

Per inserire un elemento 'x : T'  in testa a una lista 'xs : T' list si puo' scrivere

   x  :: xs    OPPURE     [x] @ xs

E' preferibile usare  cons (piu' efficiente).

Per inserire un  elemento x : T  in coda a una lista xs : T list si puo' solo usare  @

   xs @  [x]

mentre   xs :: x   non ha senso (non e' una espressione tipabile).

*)   

(*


Esempio 5
^^^^^^^^^

Definire la funzione ricorsiva (polimorfa)

   rev : 'a list -> 'a list

che inverte gli elementi di una lista (analoga a List.rev):

  rev [ x0 ; x1 ; .... ; x(n-1) ; xn ]  = [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

===

Notare che il pattern matching permette di estrarre il primo elemento
della lista ma non l'ultimo.

La lista 
 
     [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

puo' pero' essere vista come la concatenazione delle due liste

   [ xn ; x(n-1) ; ... ; x1 ]      [x0]

Inoltre [ xn ; x(n-1) ; ... ; x1 ] puo' essere costruita con la
chiamata ricorsiva

  rev [ x1 ; .... ; x(n-1) ; xn ]   // l'argomento di rev e' la coda di ls
  

Questa versione di reverse è quadratica -- vedremo in seguito come
migliorarla rendendola iterativa e lineare.

*)
 
let rec rev ls =  
    match ls with 
    | [] -> [] 
    | x :: xs -> append (rev xs, [x]) 
 

rev [1 .. 10] ;;        // [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]

(*

Esempio 6
^^^^^^^^^

Definire la funzione ricorsiva (polimorfa)

   mem : 'a * 'a list -> bool   when 'a : equality

che verifica l'appartenenza di un elemento in una lista

Anche in questo caso e' gia' definita in F# l'analoga funzione List.mem


===

Si osserva che

    x  appartiene a  [ y0 ; y1 ; .... ;  yn ] 

se e solo se

 ( x = y0 )    OR   ( x appartiene a  [ y1 ; .... ;  yn ] )


*)   
 
let rec mem (x , ls) =  
    match ls with 
    | [] -> false 
    | y::ys -> x=y || mem (x, ys) 
     
mem ('b', ['a' .. 'h'] ) ;; // true
mem ('x', ['a' .. 'h'] ) ;; // false
 
 
