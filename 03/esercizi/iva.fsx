
// iva : char * float -> float

let iva cat =
     match cat with
     | 'a' | 'A' -> 4
     | 'b' | 'B'  -> 10
     |  _  -> 22


(*

costoConIva   = costoBase  * ( 1 +  IVA /100.0 )

 costoBase   = costoConIva / ( 1 +  IVA /100.0 )

*)   

// funzione ausiliaria

let percent fl = (1.0 + fl /100.0)

// piuIva : char * float -> float)
let piuIva (cat,costoBase) =
      costoBase * percent (float (iva cat) )


// menoIva : char * float -> float)
let menoIva (cat,costoConIva) =
      costoConIva  / percent (float (iva cat) )

// Esempi 

let c1 = piuIva('a', 1000.0)
// c1 :  float = 1040.0

let c2 =  piuIva('b', 1000.0)
//  c2 : float = 1100.0

let c3 =  piuIva('c', 1000.0)
// c3 : float  =  1220.0


let c4 = menoIva ('a',  piuIva('a', 1000.0))   // 1000.0
let c5 = menoIva ('b',  piuIva('b', 1000.0))   // 1000.0
let c6 = menoIva ('c',  piuIva('c', 1000.0))   // 1000.0


// con meno parentesi usando pipe

let piuIva2 (cat,costoBase) =
      costoBase * percent (iva cat |> float )

let piuIva3 (cat,costoBase) =
      costoBase * 
      (cat |> iva |> float  |> percent )