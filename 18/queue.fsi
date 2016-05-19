module Queue
type Queue<'a>  // coda parametrica

val empty : Queue<'a>  
val put   : 'a -> Queue<'a> -> Queue<'a>
val get   : Queue<'a> -> 'a * Queue<'a>
val isEmpty  :  Queue<'a> -> bool
val toList :  Queue<'a> -> 'a list
val ofList :  'a list ->  Queue<'a>
val put_list :  'a list ->  Queue<'a>  -> Queue<'a>

exception EmptyQueue