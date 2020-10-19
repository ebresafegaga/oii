
let digitNames = [
    0,"Zero"; 1, "One"; 2,"Two";   3, "Three"; 4, "Four";
    5,"Five"; 6, "Six"; 7, "Seven"; 8, "Eight"; 9, "Nine"
]
let numbers = [16; 58; 510]

let lookup = List.assoc 

let str number =
    let rec str acc number = 
        if number <= 0 then acc
        else 
            str (lookup (number mod 10) digitNames ^ acc) (number / 10) 
    in 
    str "" number

let strings numbers = List.map str numbers  


type shape = Rect | Triangle | Square 



let () = 
    Printf.printf "hello, from caml!"