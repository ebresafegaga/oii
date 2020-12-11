

type ('a, 'b) eq = Eq : ('a, 'a) eq

let cast : type a b. (a, b) eq -> a -> b = fun Eq x -> x

let a = Eq 