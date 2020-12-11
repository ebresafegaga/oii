


type ('ty, 'v) dlist = 
    | [] : ('v, 'v) dlist 
    | (::) : 'a * ('ty, 'v) dlist -> ('a -> 'ty, 'v) dlist

let a = [ 1; 2; ""; Some [1;6] ]

let plus1 l = [();l]

let one x = [x]

let rec append : type a b c. (a, b) dlist -> (b, c) dlist -> (a, c) dlist =  fun l1 l2 -> match l1 with 
    | [] -> l2
    | x :: xs -> 
        let t = append xs l2 in 
        x :: t 

let de = []

let d = [1;"";3]

let e = [2;3;5]

let f () = append d e 