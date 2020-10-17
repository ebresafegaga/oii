
type nil = N 

type ('a, 'xs) cons = C


type 'a list = 
    | [] : nil list
    | (::) : 'x * 'xs list -> ('x, 'xs) cons list

let l = 
    [ 1; 4; []; "something"; Some 22, None ]

let b = [1;1;3; Some ""]

let name : type n. (int, n) cons list -> int  = function 
    | x :: xs -> compare x 0