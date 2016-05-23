module Stack
type Stack<'a>

val empty : 'a Stack
val push : 'a -> 'a Stack -> 'a Stack
val pop : 'a Stack -> 'a * 'a Stack
val top : 'a Stack -> 'a
val size : 'a Stack -> int

exception EmptyStack