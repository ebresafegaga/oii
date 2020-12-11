

type _ typ =
   | Int : int typ
   | String : string typ
   | Arrow : 'a typ * 'b typ -> ('a -> 'b) typ
   | Pair : 'a typ * 'b typ -> ('a * 'b) typ

 let rec to_string: type t. t typ -> t -> string = fun t x ->
   match t with
   | Int -> Int32.to_string (Int32.of_int x)
   | String -> Printf.sprintf "%S" x
   | Arrow (_t1, _t2) ->
        (* let g = Int32.to_string x in   *)
        (* Printf.sprintf "(%s -> %s)" (to_string t1 x1) (to_string t2 x2) *)
        "<fun>"
   | Pair(t1,t2) ->
       let (x1, x2) = x in
       Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2)

let a = to_string (Arrow (Int, Int))

let rec to_string_do_be : type t. t typ -> t -> string = fun t ->
   match t with
   | Int -> fun x -> Int32.to_string (Int32.of_int x)
   | String -> fun x -> Printf.sprintf "%S" x
   | Arrow (t1, t2) ->
        (* let g = Int32.to_string x in   *)
        (* Printf.sprintf "(%s -> %s)" (to_string t1 x1) (to_string t2 x2) *)
        fun f -> ""
   | Pair(t1, t2) ->
       (* let (x1, x2) = x in
       let s = Printf.sprintf "(%s,%s)" (to_string_do_be t1 x1) (to_string_do_be t2 x2) in *)
       failwith ""


type zero = Z
type 'a succ = S

type 'a nat = 
    | Zero : zero nat
    | Succ : 'a nat -> 'a succ nat 

type 'a plus = 
    | ZZZ : zero nat * 'a nat -> 'a plus
    | RRR : 'a succ nat * 'b nat -> ('a * 'b) succ plus
    | DDD : ('a succ * 'b) succ plus -> ('a * 'b) succ succ plus 

let on = Zero
let tw = Succ (Succ (Succ Zero))
let test = (DDD (RRR (tw, tw)))


type 'a witness = 
    | Int : int witness 
    | Float : float witness 
    | List : 'a witness -> ('a list) witness

type peano = 
    | Zero 
    | Succ of peano 

type nil = unit 

type ('x, 'xs) cons = 'x -> 'xs


 type _ t =
          | Int : int t
          | Bool : bool t

let deep : (char t * int) option -> char = function
    | None -> 'c'




type (_,_) eq = Eq : ('a,'a) eq

let cast : type a b. (a,b) eq -> a -> b = fun Eq x -> x


let rec eq_type : type a b. a typ -> b typ -> (a,b) eq option = fun a b -> match a, b with
    | Int, Int -> Some Eq
    | String, String -> Some Eq
    | Pair (a1, a2), Pair (b1, b2) ->
        begin match eq_type a1 b1, eq_type a2 b2 with
        | Some Eq, Some Eq -> Some Eq
        | _ -> None
        end
    | _ -> None

type dyn = Dyn : 'a typ * 'a -> dyn

let get_dyn : type a. a typ -> dyn -> a option = fun a (Dyn (b, x)) -> match eq_type a b with
    | None -> None
    | Some Eq -> Some x


type 'a listS = 
    ListS of { f : 'r. ('a -> 'a listS -> 'r) -> 'r -> 'r }

let nilS = ListS {f = fun co ni -> ni}

let consS x xs = ListS {f = fun co ni -> co x xs} 

let unCons co ni (ListS {f}) = f co ni

let rec fmapS f = unCons (fun x xs -> consS (f x) (fmapS f xs)) nilS

type 'a listC = 
    ListC of {g : 'r. ('a -> 'r -> 'r) -> 'r -> 'r}

let foldC co ni (ListC {g}) = g co ni

let nilC = ListC {g = fun f start -> start}

let consC x (ListC {g}) = ListC {g = fun f start -> f x (g f start)}

let unpack xs = foldC (fun x  xs -> x :: xs) [] xs

let pack xs = List.fold_right consC xs nilC

(* uncons :: (a -> List a -> r) -> r -> List a -> r *)

let a = failwith ""

let uncons : ('a -> 'a listC -> 'r) -> 
             'r -> 
             'a listC -> 
             'r  =
    fun co ni xs ->
        match unpack xs with 
        | x :: xs -> co x (pack xs)
        | [] -> ni  

type id = {f : 'a. 'a -> 'a}

type some = {k : 'a. 'a list -> int}

let tr (xs : int list) (bs : bool list) {k} = (k xs, k bs)

let r4 = { f = fun (type a) (x: a) -> x}

let maybe : id -> 'c * 'b -> ('c * 'b) = 
    fun {f} (a, b) -> (f a, f b)

let zero = fun s z -> z

let succ = fun x1 -> fun s z -> s (x1 s z)

type 'a term = 
    | Int : int -> int term 
    | Add : (int -> int -> int) term
    | App : ('b -> 'a) term * 'b term -> 'a term

type 'r visitor = {fn : 'a. 'r -> 'a term  -> 'r}

let rec fold : type a. 'r -> 'r visitor  -> a term -> 'r = fun i {fn} -> function 
    | Int _ as t -> fn i t
    | Add -> fn i Add
    | _ -> failwith ""
    (* | App (x, y) as t -> f (fold (fold i f x) f y) t *)

let func = { fn = fun (type a) r (t: a term) -> match t with 
                  | Int x -> r+10 
                  | App _ -> 56
                  | Add -> 0 }

let rec map (f : item:'a -> 'b) (xs : 'a list) =
    match xs with 
    | [] -> [] 
    | x :: xs -> f x :: map f xs
(* 
data Seq a = SNil
           | Zero (Seq (a,a))
           | One a (Seq (a,a)) *)

type 'a seq =
    | Nil 
    | Zero of ('a * 'a) seq 
    | One of 'a * ('a * 'a) seq 

(* cons :: a -> Seq a -> Seq a
cons x SNil = One x SNil
cons x (Zero ps) = One x ps
cons x (One y ps) = Zero (cons (x, y) ps) *)

let rec cons : 'a. 'a -> 'a seq -> 'a seq = fun x -> function 
    | Nil -> One (x, Nil)
    | Zero ps -> One (x, ps)
    | One (y, ps) -> Zero (cons (x, y) ps)

let prefix x (xss: 'a list list) = 
    let xcons (ys : 'b list) = x :: ys in 
    List.map xcons xss

type 'a st = 
    ST of { st : 's. 's -> 'a * 's }
            [@@unboxed]

let test (ST {st}) = st 

let y =  ST {st = fun s -> 9, s}

let writeST : 'a st -> 
              'a -> 
              unit st = 
    fun (ST {st}) a -> 
        ST {st = fun s -> (), s}

let runST = fun (ST {st}) -> fst (st 10)

type name = ..

type name += 
    | Noooo of int 
    | Mokk 

let b = function
    | Noooo b -> "tc guk"
    | Mokk -> "no oi"
    | _ -> ""


type 'a mystery_box =
    | EmptyBox : unit mystery_box 
    | IntBox : int * unit mystery_box -> int mystery_box 
    | StringBox : string * int mystery_box -> string mystery_box 
    | BoolBox : bool * string mystery_box -> bool mystery_box 

type 'r f = 
    {f : 'a. 'a mystery_box -> 'r }
        [@@unboxed]

let unwrap (type a) (box : a mystery_box) {f} = match box with
    | EmptyBox -> None 
    | IntBox (_, xs) -> Some (f xs)
    | StringBox (_, xs) -> Some (f xs)
    | BoolBox (_, xs) -> Some (f xs)

let summ = { f = fun (type a) (b : a mystery_box) -> 
                 match b with 
                 | EmptyBox -> 34
                 | _ -> 0 }
