module t where 

data List (A : Set) : Set where 
    Nil : List A 
    Cons : A -> List A -> List A

append : ∀ {A} ->  List A -> List A -> List A
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

data Nat : Set where 
    zero : Nat
    succ : Nat -> Nat 

{-# BUILTIN NATURAL Nat #-}

_+_ : Nat -> Nat -> Nat 
zero + m   = m
succ n + m = succ (n + m)

_*_ : Nat -> Nat -> Nat 
zero * m   = zero
succ n * m = n + (n * m) 

data Parity : Nat -> Set where 
    even : (k : Nat) -> Parity (k * 2)
    odd : (k : Nat) -> Parity (1 + (k * 2))

parity : (n : Nat) -> Parity n
parity zero     = even zero
parity (succ n) with parity n 
parity (succ .(k * 2))        | even k = odd k
parity (succ .(1 + (k * 2)))  | odd k = {!  !}