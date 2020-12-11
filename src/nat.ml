open Vector

type natural = 
    | Z 
    | S of natural 

let rec toNat : type n. n nat -> natural = function 
    | Zero -> Z
    | Succ k -> S (toNat k)

type 'r f = { f : 'n. 'n nat -> 'r }
            [@@unboxed]

let rec fromNat {f} = function 
    | Z -> f Zero 
    | S n -> fromNat {f = fun x -> f (Succ x) } n

type ('a, 'r) f = { f : 'm. ('m, 'a) vec -> 'r }
                  [@@unboxed]

let rec filterV 
        : type n. 
        ('a -> bool) ->
        (n, 'a) vec ->
        ('a, 'r) f ->
        'r =
    fun p xs {f} -> match xs with 
        | [] -> f []
        | x :: xs -> 
            filterV p xs @@
                if p x 
                    then {f = fun result -> f (x :: result) }
                    else {f}

let a = [1;3;4;5;6]

let b = filterV ((>) 2) a 
    {f = fun (type a) (vec : (a, _) vec) -> 
        match vec with 
        | [] -> 0
        | xs -> foldr (+) 0 xs }


type 'v term = 
    | Var of 'v 
    | App of 'v term * 'v term 
    | Lam of 'v incr term 

and 'v incr = 
    | Zero 
    | Succ of 'v   

let mapP f (x, y) = (f x, f y)


let mapI f = function 
    | Zero -> Zero 
    | Succ x -> Succ (f x)

let rec mapT : 'a 'b. ('a -> 'b) -> 'a term -> 'b term = fun f -> function 
    | Var x -> Var (f x) 
    | App (a, b) ->  
        let a, b = (mapP (mapT f) (a, b)) in 
        App (a, b)
    | Lam t -> Lam (mapT (mapI f) t)

let rec foldT v a l = function 
    | Var x -> v x
    | App (x, y) -> a (mapP (foldT v a l) (x, y)) 
    | Lam t -> l (foldT v a l) t

type 'a nest = NilN | ConsN of 'a * ('a *'a) nest 

type _ t = T1 : int t | T2 : bool t 

let f (type a) (x : a t) (y : a t) = 
    match x, y with 
    | T1, T1 -> ()
    | T2, T2 -> ()
    | _ -> .

type 'a binTree = Leaf of 'a | Fork of 'a binTree pair
and 'a pair = 'a * 'a


let rec mapB f = function 
    | Leaf x -> Leaf (f x) 
    | Fork p -> Fork (mapP (mapB f) p)

let rec foldB l f = function 
    | Leaf x -> l x
    | Fork p -> f (mapP (foldB l f) p) 

let const = fun _ a -> a
let uncurry f = fun (a, b) -> f a b 


let size = foldB (const 1) (uncurry (+))

let height = 
    let maxp (x, y) = 1 + max x y in 
    foldB (const 0) maxp

let flatten t = 
    let wrap x = List.cons x [] in 
    foldB wrap (uncurry (@)) t


(* type zero = unit 

type 'a succ = unit -> 'a

type 'a peano = 
    | Zero : zero peano 
    | Succ : 'a peano -> ('a succ) peano

let a = Succ (Succ Zero) *)


exception EmptyList of int * string

let test xs = match List.hd xs with  
    | None -> 0
    | Some "no" -> 10
    | Some "yes" -> 20
    | Some _ -> 0
    | exception EmptyList (a, b) -> 0

module type Show = sig 
    type t 
    val show : t -> string 
end

module type T = functor (S : Show) -> Show

module Thingy (T : T) = struct 

    module A  = T (struct type t = string let show x = x end)

    let c = A.show

end


let show (type a) (module S : Show with type t = a) (x: a) = S.show x

module type Type = sig module type T end

module Type = struct module type T = Type end

module A : Type.T =  struct 
    module type T = sig 

    end
end 
