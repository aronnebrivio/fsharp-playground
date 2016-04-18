(*
Blocchi.
Possono essere jolly (J) o colorati
*)
// i)
type colour = Blue | Green | Red;;

// il blocco è un oggetto che può avere 2 tipi diversi (Jolly o Colore)
type block =
    | B of colour
    | J;;

let bl1 = [B Red; B Red; J; B Green; B Green; J; J];;
let bl3 = [J; J; B Blue; J; B Red; B Red];;
let bl4 = [J; J; B Green; J; B Red; B Green; J; J; B Red];;

// ii)
// conta il numero di blocchi colorati e di jolly
let rec count bs =
    match bs with
    | [] -> (0,0)
    | x::xs -> 
        let (c,j) = count xs
        match x with
        | J -> (c,j+1)
        | B _ -> (c+1,j) ;;

// iii)
// getColours : block list -> colour list
// restituisce la lista dei colori presenti nella lista di blocchi
// -> funzione ausiliara che controlli se un elemento è nella lista
let isIn x xs = List.exists (fun z -> z=x) xs;;

let rec getColours bs =
    match bs with
    | [] -> []
    | J :: xs -> getColours xs
    | B col :: xs -> 
        let cols = getColours xs
        if (isIn col cols) then cols else col::cols;;


// iv)
// repaintB f b : permette di ricolorare un blocco colorato (non modifica i jolly)
// f : colour -> colour option
// dato un colore col1
// - se f col1 = Some col2, allora col1 va cambiato in col2
// - altrimenti col1 non va cambiato
let repaintB f b =
    match b with
    | J -> J
    | B col1 -> 
        match f col1 with
        | Some col2 -> B col2
        | None -> b;;

// v)
// repaintBs -> applicare repaint ad ogni elemento della lista
let repaintBs f bs = List.map (repaintB f) bs;;