

type zero = Z
type 'a succ = S

type 'a nat = 
    | Zero : zero nat
    | Succ : 'a nat -> 'a succ nat

(* type ('a, 'b) vec = 
    | [] : (zero nat, 'b) vec
    | (::) : 'b * ('n nat, 'b) vec -> ('n succ nat, 'b) vec
    constraint 'a = [ `Zero |  `Succ ] *)

type ('a, 'b) vec = 
    | [] : (zero, 'b) vec
    | (::) : 'b * ('n, 'b) vec -> ('n succ, 'b) vec

type 'a fin = 
    | FZ : 'a succ fin
    | FS : 'a fin -> 'a succ fin 

type 'n permutation = 
    | PNil : zero permutation
    | PCons : 'n succ fin * 'n permutation -> 'n succ permutation

let rec id : type n. n nat -> n permutation = function
    | Zero -> PNil
    | Succ k -> PCons (FZ, id k)

(* let sigma 
    : type n a. 
    n nat -> 
    n permutation -> 
    (n, a) vec -> 
    (n, a) vec = 
        fun n p xs -> match n, p, xs with 
            | _, _, [] -> [] *)
    

let rec fmax : type n. n nat -> n fin = fun n -> 
    match n with 
    | Zero -> failwith "impossible"
    | Succ Zero -> FZ
    | Succ k -> FS (fmax k)

let rec fweak : type n. n fin -> n succ fin = function
    | FZ -> FZ
    | FS i -> FS (fweak i)

let rec vtab : 
    type idx. 
    idx nat -> 
    (idx fin -> 'x) -> 
    (idx, 'x) vec = 
        fun n f -> match n with 
            | Zero -> []
            | Succ k -> f FZ :: vtab k (fun i -> f (FS i))

(* let rec vtab2 : type n. n nat -> (n fin -> 'x) -> (n, 'x) vec = fun n f -> match n with 
    | Zero -> []
    | Succ k -> 
        f (fmax n) :: vtab2 k (fun i -> f (fweak i)) *)

let rec length : type n. (n, 'a) vec -> n nat = function 
    | [] -> Zero
    | _ :: xs -> Succ (length xs)

let rec vproj : type n. (n, 'a) vec -> n fin -> 'a = fun v f -> match v, f with 
    | x :: _, FZ -> x
    | _ :: xs, FS i -> vproj xs i
    | [], _ -> .

let rec vec  : type n. n nat -> 'a -> (n, 'a) vec = fun n thing -> match n with
    | Zero -> []
    | Succ prev -> thing :: vec prev thing

let rec va_apply : type n. (n, ('a -> 'b)) vec -> (n, 'a) vec -> (n, 'b) vec = fun fs xs -> 
    match fs, xs with
    | [], [] -> []
    | f :: fs, x :: xs -> f x :: va_apply fs xs

let (<*>) = va_apply

let map : type n. ('a -> 'b) -> (n, 'a) vec -> (n, 'b) vec = fun f xs -> 
    let len = length xs in 
    let fs = vec len f in 
    fs <*> xs

let head = function x :: _ -> x

let tail = function _ :: xs -> xs

let rec foldr: 
    type n. 
    ('a -> 'b -> 'b) -> 
    'b  -> 
    (n, 'a) vec -> 
    'b  = 
        fun f init -> function 
            | [] -> init
            | x :: xs -> f x (foldr f init xs)

let rec to_list : type n. (n, 'a) vec -> 'a list = function 
    | [] -> []
    | x :: xs ->  x :: to_list xs

type 'a vec_exist = VecExist : ('n, 'a) vec -> 'a vec_exist 

let rec of_list : 'a list -> 'a vec_exist = function
    | [] -> VecExist []
    | x :: xs -> 
        let VecExist xs = of_list xs in 
        VecExist (x :: xs)

let rec filter : type n. ('a -> bool) -> (n, 'a) vec -> 'a vec_exist = fun f -> function 
    | [] -> VecExist []
    | x :: xs -> 
        let VecExist xs = filter f xs in 
        if f x 
            then VecExist xs 
            else VecExist (x :: xs)

let rec zip : type n. (n, 'a) vec -> (n, 'b) vec -> (n, 'a * 'b) vec = 
    fun xs ys -> 
        match xs, ys with 
        | [], [] -> []
        | x :: xs, y :: ys -> (x, y) :: zip xs ys

let rec v_zip : type n. (n, 'a) vec -> (n, 'b) vec -> (n, 'a * 'b) vec = 
    fun xs ys ->
        let n = length xs in
        let f = vec n (fun a b -> a, b) in
        f <*> xs <*> ys

type 'a more_than_five = 'a succ succ succ succ succ 

let take5 : type n. (n more_than_five, 'a) vec -> (zero more_than_five, 'a) vec = function 
  | a :: b :: c :: d :: e :: _ -> [a;b;c;d;e]
  | _ -> .
                                
    
let magic : zero fin -> 'a = function 
    | _ -> .


let a =
  List.fold_left
