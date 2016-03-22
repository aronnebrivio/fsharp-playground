// Definisco funzione iva
let iva x = 
	match x with
	| 'a' | 'A' -> 4
	| 'b' | 'B' -> 10
	| _ -> 22 ;;

let perc x = (1.0 + float(x) / 100.0) ;;

let piuIva (cat,b) = float(b) * perc (iva cat) ;;

let menoIva (cat,costo) = float(costo) / perc (iva cat) ;;