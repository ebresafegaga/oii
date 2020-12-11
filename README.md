# Oii

This is just a mini library to have a taste of dependent types with GADTs in OCaml (and some experiments with effects in multicore OCaml)

---

Here are some examples 

```ocaml 
(* Natural numbers *)
type zero = Z
type 'a succ = S

(* A, so called, singleton type for natural numbers *)
type 'a nat = 
    | Zero : zero nat
    | Succ : 'a nat -> 'a succ nat
    
(* Length indexed lists (aka Vectors!) OCaml also has a nice overloaded list syntax *)
type ('a, 'b) vec = 
    | [] : (zero, 'b) vec
    | (::) : 'b * ('n, 'b) vec -> ('n succ, 'b) vec

let xs = [1;2;3;4;5] (* OCaml would infer => xs : (zero succ succ succ succ succ, int) vec *) 

(* My favourite inductive family. Finite Sets 🤩*)
type 'a fin = 
    | FZ : 'a succ fin
    | FS : 'a fin -> 'a succ fin 

(* classic finite set functions *)
let rec fmax : type n. n nat -> n fin = fun n -> 
    match n with 
    | Zero -> failwith "impossible" (* Ocaml would need actual "pi" types to be able to verify this *)
    | Succ Zero -> FZ
    | Succ k -> FS (fmax k)

let rec fweak : type n. n fin -> n succ fin = function
    | FZ -> FZ
    | FS i -> FS (fweak i)

(* No need for optionals or exceptions. `n fin` means that a number must be in the range 1-n *)
let rec index : type n. (n, 'a) vec -> n fin -> 'a = fun v f -> match v, f with 
    | x :: xs, FZ -> x
    | x :: xs, FS i -> index xs i
    | _ -> . (* refutation case makes the OCaml type checker work a little harder to verify totality *)

(* Yes, the OCaml typechecker can see this is not possible (zero fin has no inhabitants), 
   so it gives you a way to say: "this is BS!" 
   aka "ex falso quodlibet" if you look from the Curry-Howard lens *)
let magic : zero fin -> 'a = function 
    | _ -> . 

(*
  No need for optionals!
  Note that there's no need to handle the [] case and the OCaml compiler can see that 
*)
let head = function x :: _ -> x
let tail = function _ :: xs -> xs

(* foldr on vectors *)
let rec foldr : type n. ('a -> 'b -> 'b) -> 'b  -> (n, 'a) vec -> 'b  = fun f init -> function 
    | [] -> init
    | x :: xs -> f x (foldr f init xs)


(* Vectors are applicatives *)

(* this is the `pure` Applicative operation for vectors *)
let rec vec  : type n. n nat -> 'a -> (n, 'a) vec = fun n thing -> match n with
    | Zero -> []
    | Succ prev -> thing :: vec prev thing

(* apply, and it's battleship operator.
  Note how the compiler can verfy that the vectors are of equal length and there's no need to handle ugly cases if they weren't equal *)
let rec va_apply : type n. (n, ('a -> 'b)) vec -> (n, 'a) vec -> (n, 'b) vec = fun fs xs -> 
    match fs, xs with
    | [], [] -> []
    | f :: fs, x :: xs -> f x :: va_apply fs xs

let (<*>) = va_apply

(* all Applicatives are Functors *)
let fmap : type n. ('a -> 'b) -> (n, 'a) vec -> (n, 'b) vec = fun f xs -> 
    let len = length xs in 
    vec len f <*> xs

(* <*> is a like recursive operator on vectors, so a lot of other recursive functions can be defined in terms of it  *)
let zip : type n. (n, 'a) vec -> (n, 'b) vec -> (n, 'a * 'b) vec = fun xs ys ->
    let n = length xs in
    vec n (fun a b -> a, b) <*> xs <*> ys

(* A fake "sigma" type *)
type 'a vec_exist = VecExist : ('n, 'a) vec -> 'a vec_exist 

(* We don't have a type level "length" function to know the index of the vector before hand,
  so exitential types are a pretty good alternative. 
  The only probelem is that we just can't unwrap it anywhere we like because...
  Also, notice how OCaml can pattern match on `[]` as a list and also return it as a vector *)
let rec of_list : 'a list -> 'a vec_exist = function
    | [] -> VecExist []
    | x :: xs -> 
        let VecExist xs = of_list xs in (* you're entitled to a recursive call, never forget that *)
        VecExist (x :: xs)
        
(* same idea *)
let rec filter : type n. ('a -> bool) -> (n, 'a) vec -> 'a vec_exist = fun f -> function 
    | [] -> VecExist []
    | x :: xs -> 
        let VecExist xs = filter f xs in (* invoke the inductive hypothesis *)
        if f x 
        then VecExist xs 
        else VecExist (x :: xs)
        
(* Order preserving embeddings also known as "thinnings" *)

(* My other favourite inductive family. Can you guess what it can be used for just from it's defination? *)
(* The inhabitants of this type actually form a pascal's triangle, which is pretty cool! *)
type ('m, 'n) opf = 
    | OpfZero : (zero, zero) opf 
    | OpfSame : ('m, 'n) opf -> ('m succ, 'n succ) opf 
    | OpfRight : ('m, 'n) opf -> ('m, 'n succ) opf


(*  order preserving embeddings between finite sets *)
let rec opf : type m n. (m, n) opf -> m fin -> n fin = fun f i -> match f, i with 
    | OpfSame _, FZ -> FZ
    | OpfSame prev, FS i -> FS (opf prev i)
    | OpfRight _, FZ -> FZ 
    | OpfRight prev, i -> fweak (opf prev i)
    (* | OpfZero, FZ -> FZ  -- zero fin has no inhabitants  *)
    | _ -> .

(*  id operation for opf *)
let rec i_opf : type n. n nat -> (n, n) opf = function 
    | Zero -> OpfZero
    | Succ k -> OpfSame (i_opf k)
    
(* composition, so maybe this opf thingy forms a Category? *)
let rec c_opf : type m n l. (l, m) opf -> (m, n) opf -> (l, n) opf = fun f g -> match f, g with 
    | OpfZero, OpfZero -> OpfZero
    | OpfSame f, OpfSame g -> OpfSame (c_opf f g)
    | f, OpfRight j -> OpfRight (c_opf f j)
    | OpfRight f, OpfSame g -> OpfRight (c_opf f g)

(* this functions describe EXACTLY how to pick n things from m things (not just only the first n), which is similar to `take` *)
let rec hand_pick : type m n. (m, n) opf -> m nat -> (n, 'a) vec -> (m, 'a) vec = fun f n xs -> 
    match f, n, xs with
    | OpfZero, Zero, [] -> []
    | OpfSame f, Succ k, x :: xs -> x :: hand_pick f k xs
    | OpfRight f, k, _ :: xs -> hand_pick f k xs

(* This inductive family gives a proof that n < m at the type level *)
type ('n, 'm) lte = 
    | LteZero : (zero, 'm) lte
    | LteSucc : ('n, 'm) lte -> ('n succ, 'm succ) lte 

(* With that in mind, we can define a `take` function which never fails *)
let rec vtake : type n m. (n, m) lte -> n nat -> (m, 'a) vec -> (n, 'a) vec = fun proof n xs -> 
    match proof, n, xs with 
    | LteZero, Zero, _ -> []
    | LteSucc proof, Succ k, x :: xs -> x :: vtake proof k xs 

(* Describes EXACTLY how to split a number into 2 parts *)
type ('m, 'k, 'n) split = 
    | SplitZero : (zero, zero, zero) split
    | SplitLeft : ('m, 'k, 'n) split -> ('m succ, 'k succ, 'n) split
    | SplitRight : ('m, 'k, 'n) split -> ('m, 'k succ, 'n succ) split

(* with that we can split a vector into 2 parts ... *)
(* notice how the literal meaning, type and defination of split and join "say" the same thing *)
let rec split : type n k m. (n, k, m) split -> (k, 'a) vec -> (n, 'a) vec * (m, 'a) vec = fun proof xs ->
    match proof, xs with 
    | SplitZero, [] -> [], []
    | SplitLeft proof, n :: xs -> 
        let ns, ms = split proof xs in 
        n :: ns, ms
    | SplitRight proof, m ::xs -> 
        let ns, ms = split proof xs in 
        ns, m :: ms
        
(* ... and join them back together, maintaining the order of the original vector*)
let rec join : type n k m. (n, k, m) split -> (n, 'a) vec -> (m, 'a) vec -> (k, 'a) vec = fun proof ns ms -> 
    match proof, ns, ms with 
    | SplitZero, [], [] -> []
    | SplitLeft proof, n :: ns, ms -> n :: join proof ns ms
    | SplitRight proof, ns, m :: ms -> m :: join proof ns ms 

(* The classic hetoregenous list *)

type nil = N 

type ('a, 'xs) cons = C


type 'a list = 
    | [] : nil list
    | (::) : 'x * 'xs list -> ('x, 'xs) cons list

let l = 
    [ 1; 4; []; "something"; Some 22, None ]

let b = [1;1;3; Some ""]

(* heterogenous trees *)

type empty = E
type ('left, 'item, 'right) branch = B

type 'a t = 
    | Empty : empty t 
    | Branch : 'left t * 'item * 'right t -> ('left, 'item, 'right) branch t
    
 
 
(* type aligned list *)
(* just a pipe of functions left to right *)

type ('a, 'b) ta_list = 
    | TalStart : ('a -> 'b) -> ('a , 'b) ta_list 
    | TalCons : ('a, 'b) ta_list * ('b -> 'c) -> ('a, 'c) ta_list
 
(* compose 2 list of functions *)
let rec compose : type a b c. (a, b) ta_list -> (b, c) ta_list -> (a, c) ta_list = fun f -> function  
    | TalStart k -> TalCons (f, k)
    | TalCons (k, j) -> 
        let r = compose f k in 
        TalCons (r, j)

let start f = TalStart f 

let (<$>) fs f  = TalCons (fs, f)

 (* e.g *)
let pipes f = 
    start f <$> (fun n -> Some n) <$> (function None -> 0 | Some x -> compare x "") 

(* After contruction, just convert to an OCaml function you can play with *)
let rec un_wrap : type a b. (a, b) ta_list -> (a -> b) = function 
    | TalStart f -> f
    | TalCons (fs, f) -> 
        let g = un_wrap fs in 
        fun x -> f (g x)
```

Please send in PRs and hack on it!
