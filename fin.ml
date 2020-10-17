
open Vec

type nat_exist = NatExist : 'n nat -> nat_exist

let rec fFin : type n. n fin -> nat_exist = function 
    | FS i -> 
        let NatExist n = fFin i in 
        NatExist (Succ n)
    | FZ -> NatExist Zero