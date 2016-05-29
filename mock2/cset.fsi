// interface for finite  set of any type 'a, whose equality is reasonable

module FSet


type FSet<'a when 'a : equality> 

val empty : FSet<'a> 

val contains  : 'a ->  FSet<'a> -> bool 

val singleton : 'a -> FSet<'a> 

val add : 'a -> FSet<'a> -> FSet<'a> 

val union : FSet<'a> -> FSet<'a> -> FSet<'a> 

val ofList :  'a list -> FSet<'a> 



