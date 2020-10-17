

(* let rec to_int : type n. n nat -> int = function 
    | Zero -> 0
    | Succ k -> 1 + to_int k   *)

type zero = unit 

type 'a succ = unit -> 'a

type 'a peano = 
    | Zero : zero peano 
    | Succ : 'a peano -> ('a succ) peano


let a = Succ (Succ Zero)