

open Vector

type ('n, 'm) lte = 
    | LteZero : (zero, 'm) lte
    | LteSucc : ('n, 'm) lte -> ('n succ, 'm succ) lte 

type ('m, 'n) opf = 
    | OpfZero : (zero, zero) opf 
    | OpfSame : ('m, 'n) opf -> ('m succ, 'n succ) opf 
    | OpfRight : ('m, 'n) opf -> ('m, 'n succ) opf

type ('m, 'k, 'n) split = 
    | SplitZero : (zero, zero, zero) split
    | SplitLeft : ('m, 'k, 'n) split -> ('m succ, 'k succ, 'n) split
    | SplitRight : ('m, 'k, 'n) split -> ('m, 'k succ, 'n succ) split

(*  order preserving embeddings between finite sets *)

let rec opf : type m n. (m, n) opf -> m fin -> n fin = fun f i -> match f, i with 
    | OpfSame _, FZ -> FZ
    | OpfSame prev, FS i -> FS (opf prev i)
    | OpfRight _, FZ -> FZ 
    | OpfRight prev, i -> fweak (opf prev i)
    (* | OpfZero, FZ -> FZ  -- zero fin has no inhabitants  *)
    | OpfZero, _ -> .

(*  id operation for opf *)
let rec i_opf : type n. n nat -> (n, n) opf = function 
    | Zero -> OpfZero
    | Succ k -> OpfSame (i_opf k)

let rec c_opf : type m n l. (l, m) opf -> (m, n) opf -> (l, n) opf = fun f g -> match f, g with 
    | OpfZero, OpfZero -> OpfZero
    | OpfSame f, OpfSame g -> OpfSame (c_opf f g)
    | f, OpfRight j -> OpfRight (c_opf f j)
    | OpfRight f, OpfSame g -> OpfRight (c_opf f g)

let rec hand_pick : type m n. (m, n) opf -> m nat -> (n, 'a) vec -> (m, 'a) vec = fun f n xs -> 
    match f, n, xs with
    | OpfZero, Zero, [] -> []
    | OpfSame f, Succ k, x :: xs -> x :: hand_pick f k xs
    | OpfRight f, k, _ :: xs -> hand_pick f k xs

let rec vtake : type n m. (n, m) lte -> n nat -> (m, 'a) vec -> (n, 'a) vec = fun proof n xs -> 
    match proof, n, xs with 
    | LteZero, Zero, _ -> []
    | LteSucc proof, Succ k, x :: xs -> x :: vtake proof k xs 

let rec split : type n k m. (n, k, m) split -> (k, 'a) vec -> (n, 'a) vec * (m, 'a) vec = fun proof xs ->
    match proof, xs with 
    | SplitZero, [] -> [], []
    | SplitLeft proof, n :: xs -> 
        let ns, ms = split proof xs in 
        n :: ns, ms
    | SplitRight proof, m ::xs -> 
        let ns, ms = split proof xs in 
        ns, m :: ms

let rec join : type n k m. (n, k, m) split -> (n, 'a) vec -> (m, 'a) vec -> (k, 'a) vec = 
    fun proof ns ms -> 
        match proof, ns, ms with 
        | SplitZero, [], [] -> []
        | SplitLeft proof, n :: ns, ms -> n :: join proof ns ms
        | SplitRight proof, ns, m :: ms -> m :: join proof ns ms 

let rec lte_trans : type l m n. (l, m) lte -> (m, n) lte -> (l, n) lte =
    fun p q ->
        match p, q with 
        | LteZero, _ -> LteZero
        | LteSucc p, LteSucc q -> LteSucc (lte_trans p q)


type ('m, 'n) matrix = ('m, ('n, int) vec) vec 

let (<*>) = va_apply

let rec transpose : type m n. (m, n) matrix -> (n, m) matrix = fun xss ->  
    let n = 
        match map length xss with 
        | [] -> failwith ""
        | x :: xs -> x
    in 
    match xss with 
    | [] -> vec n []
    | x :: xs -> vec n (fun x y -> x :: y) <*> x <*> transpose xs

(* Strictly less than : n < m *)

type ('n, 'm) lt = 
    | LtZero : (zero, 'm succ) lt
    | LtSucc : ('n, 'm) lt -> ('n succ, 'm succ) lt

type 'n term = 
    | App : 'n term * 'n term -> 'n term
    | Lam : 'n succ term -> 'n term
    | Var : 'ix nat * ('ix, 'm) lt -> 'm term
    
let const : zero term = 
    Lam (Lam (Var (Succ Zero, LtSucc LtZero)))

let t = 
    Lam (
        Lam (
            (Lam (
                App (Var (Succ (Succ Zero), LtSucc (LtSucc LtZero)),
                     (Var (Zero, LtZero)))))))

let rec eval : type ty. ty term -> _ = function     
    | Var (nat, proof) -> "" 
    | App (_, _) 
    | Lam _ -> ""

let magic : type n. (n, zero) lt -> 'a = function 
  | _ -> .

let name = [ `AL "ogaga"; `OB "tinuoluwa"; `OS "dera" ]

let handle =  function
  | `AL x -> "ogaga"
  (* | _ -> "i don't know you"
  *)
