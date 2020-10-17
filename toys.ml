

type _ typ =
   | Int : int typ
   | String : string typ
   | Arrow : 'a typ * 'b typ -> ('a -> 'b) typ
   | Pair : 'a typ * 'b typ -> ('a * 'b) typ

 let rec to_string: type t. t typ -> t -> string = fun t x ->
   match t with
   | Int -> Int32.to_string (Int32.of_int x)
   | String -> Printf.sprintf "%S" x
   | Arrow (t1, t2) ->
        (* let g = Int32.to_string x in   *)
        (* Printf.sprintf "(%s -> %s)" (to_string t1 x1) (to_string t2 x2) *)
        failwith ""
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