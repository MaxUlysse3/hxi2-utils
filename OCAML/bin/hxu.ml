(* The ordering type *)
type ordering = Greater | Equal | Lesser ;;

(* Compare function using usual ordering *)
let usual_cmp x y =
    if x = y then Equal else if x > y then Greater else Lesser
;;

(* Simple merge_sort. Unstable. O(n log(n))
 Uses a compare funcion with the signature `'a -> 'a -> ordering` *)
let merge_sort cmp l =
    let rec merge u v =
        match u, v with
        | [], [] -> []
        | w, [] | [], w -> w
        | x :: xs, y :: ys -> begin
            match cmp x y with
            | Greater | Equal -> y :: (merge u ys)
            | Lesser -> x :: (merge xs v)
        end
    in
    let rec split l =
        match l with
        | [] -> ([], [])
        | [x] -> ([x], [])
        | x :: y :: xs -> let u, v = split xs in (x :: u, y :: v)
    in
    let rec aux l =
        match l with
        | [] -> []
        | [x] -> [x]
        | w -> 
            let u, v = split w in
            let u = aux u in
            let v = aux v in
            merge u v
    in aux l
;;

(* merge_sort using usual cmp *)
let msort l = merge_sort usual_cmp l ;;

(* STACKS AND QUEUE *)

(* Stack *)
type 'a stack = 'a list ;;

let empty_stack : 'a stack = [] ;;

let is_empty_s (s: 'a stack) =
    s = []
;;

let push_s x (s: 'a stack) : 'a stack =
    x :: s
;;

let pop_s (s: 'a stack) : 'a option * 'a stack =
    match s with
    | [] -> (None, [])
    | x :: xs -> (Some x, xs)
;;

let peek_s (s: 'a stack) : 'a option =
    match s with
    | [] -> None
    | x :: xs -> Some x
;;

(* Queue *)
type 'a queue = 'a list * 'a list ;;

let empty_queue : 'a queue = ([], []) ;;

let is_empty_q (q:  'a queue) =
    q = ([], [])
;;

let push_q x (q: 'a queue) : 'a queue =
    let i, o = q in
    (x :: i, o)
;;

let pop_q (q: 'a queue) : 'a option * 'a queue =
    match q with
    | i, x :: xs -> (Some x, (i, xs))
    | i, [] -> let no = List.rev i in begin
        match no with
        | [] -> (None, q)
        | x :: xs -> (Some x, ([], xs))
    end
;;

(* TREES *)

(* Strict binary tree *)
type ('n, 'l) stree = Leaf of 'l | Branch of 'n * ('n, 'l) stree * ('n, 'l) stree ;;
type ('n, 'l) stree_token = L of 'l | B of 'n ;;

let st_prefix t =
    let rec aux t acc =
        match t with
        | Leaf (x) -> (L x) :: acc
        | Branch (x, l, r) -> aux r (aux l ((B x) :: acc))
    in List.rev (aux t [])
;;

let st_postfix t =
    let rec aux t acc =
        match t with
        | Leaf (x) -> (L x) :: acc
        | Branch (x, l, r) -> (B x) :: (aux r (aux l acc))
    in List.rev (aux t [])
;;

let st_infix t =
    let rec aux t acc =
        match t with
        | Leaf (x) -> (L x) :: acc
        | Branch (x, l, r) -> aux r ((B x) :: (aux l acc))
    in List.rev (aux t [])
;;

let st_breadth t =
    let rec aux forest acc =
        match pop_q forest with
        | None, _ -> acc
        | Some t, forest -> begin
            match t with
            | Leaf (x) -> aux forest ((L x) :: acc)
            | Branch (x, l, r) -> aux (push_q r (push_q l forest)) ((B x) :: acc)
        end
    in List.rev (aux (push_q t empty_queue) [])
;;

(* TODO Faire les of_quelquechose / Faire les nstree / dicts -> hashmap & ABS *)

(* LIST AND ARRAYS *)

(* Generates a list or an array with size `n` and filled with int in [O ; n[ *)
let gen_list n = List.init n (fun x -> Random.int n) ;;
let gen_array n = Array.init n (fun x -> Random.int n) ;;

(* Print a list or an array of ints *)
let print_list l =
    Printf.printf "[ " ;
    match l with
    | [] -> ()
    | x :: xs -> Printf.printf "%d " x ; List.iter (Printf.printf "; %d ") xs
    ;
    Printf.printf "]\n"
;;
let print_array t =
    Printf.printf "[| " ;
    let len = Array.length t in
    if len <> 0 then Printf.printf "%d " t.(0) ;
    for i = 1 to len - 1 do
        Printf.printf "; %d " t.(i)
    done ;
    Printf.printf "|]\n"
;;
