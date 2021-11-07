(* problem 1 *)
let rec last (xs: 'a list): 'a option =
  match xs with
  | []  -> None
  | [x] -> Some x
  | _ :: rest -> last rest

(* problem 2 *)
let rec last_two (xs: 'a list): ('a * 'a) option =
  match xs with
  | [x1 ; x2] -> Some (x1, x2)
  | _ :: rest -> last_two rest
  | _         -> None

(* problem 3 *)
let rec at (i: int) (xs: 'a list): 'a option =
  match i, xs with
  | _, []          -> None
  | 0, (x :: _)    -> Some x
  | n, (_ :: rest) -> at (n - 1) rest

(* problem 4 *)
let length (xs: 'a list): int =
  let rec length_tail (xs: 'a list) (acc: int): int =
    match xs with
    | []        -> acc
    | _ :: rest -> length_tail rest (acc + 1)
  in length_tail xs 0

(* problem 5 *)
let rev (xs: 'a list): 'a list =
  let rec rev_aux (xs: 'a list) (acc: 'a list): 'a list =
    match xs with
    | [] -> acc
    | (x :: rest) -> rev_aux rest (x :: acc)
  in rev_aux xs []

(* problem 6 *)
let is_palindrome (xs: 'a list): bool =
  (xs = rev xs)

(* problem 7 *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten (xs: 'a node list): ('a list) =
  match xs with
  | [] -> []
  | One x :: rest -> x :: flatten rest
  | Many ms :: rest -> (flatten ms) @ (flatten rest)

(* problem 8 *)
let rec compress (xs: 'a list): 'a list =
  match xs with
  | x1 :: x2 :: rest -> let xrest = x2 :: rest in
    if x1 = x2 then compress xrest else x1 :: compress xrest
  | _ -> xs

(* problem 9 *)
let rec eat_next (acc: 'a list) (xs: 'a list): ('a list * 'a list) =
  match acc, xs with
  | [], (x :: rest) -> eat_next [x] rest
  | (a :: arest), (x :: rest) ->
    if a = x then eat_next (x :: acc) rest
    else acc, xs
  | _, _ -> acc, xs
  
let rec pack (xs: 'a list): 'a list list =
  match xs with
  | [] -> []
  | xs -> let acc, rest = eat_next [] xs in acc :: pack rest

(* problem 10 *)
let rec encode (xs: 'a list): (int * 'a) list =
  match (eat_next [] xs) with
  | (a :: acc), rest -> (length acc + 1, a) :: encode rest
  | _, _ -> []

(* problem 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let rec modified_encode (xs: 'a list): ('a rle) list =
  match (eat_next [] xs) with
  | (a :: []), rest -> One a :: modified_encode rest
  | (a :: acc), rest -> Many (length acc + 1, a) :: modified_encode rest
  | _, _ -> []

(* problem 12 *)
let rec dup_n (n: int) (a: 'a): 'a list =
  match n with
  | 0 -> []
  | _ -> a :: dup_n (n - 1) a

let rec decode (xs: 'a rle list): 'a list =
  match xs with
  | [] -> []
  | One x :: rest -> x :: decode rest
  | Many (n, a) :: rest -> dup_n n a @ decode rest

(* problem 13 *)
(* already solved directly with problem 10 *)

(* problem 14 *)
let rec duplicate (xs: 'a list): 'a list =
  match xs with
  | [] -> []
  | x :: rest -> [x; x] @ duplicate rest

(* problem 15 *)
let rec replicate (xs: 'a list) (n: int): 'a list =
  match xs with
  | [] -> []
  | x :: rest -> dup_n n x @ replicate rest n

(* problem 16 *)
let drop_nths (xs: 'a list) (n: int): 'a list =
  let rec aux (xs: 'a list) (acc: int): 'a list =
    match xs with
    | [] -> []
    | x :: rest when acc <> 1 -> x :: aux rest (acc - 1)
    | x :: rest when acc =  1 -> aux rest n
    | _ :: _ -> []
  in aux xs n

(* problem 17 *)
let split (xs: 'a list) (n: int): ('a list * 'a list) =
  let rec aux (xs: 'a list) (ys: 'a list) (n: int): ('a list * 'a list) =
    match xs, n with
    | [], _ -> xs, ys
    | _, 0 -> xs, ys
    | x :: rest, n -> aux rest (x :: ys) (n - 1)
  in let xs, ys = aux xs [] n
  in rev ys, xs

(* problem 18 *)
let rec drop_n (xs: 'a list) (n: int): 'a list =
  match xs, n with
  | [], _ -> []
  | _ , 0 -> xs
  | x :: rest, n -> drop_n rest (n - 1)

let rec take_n (xs: 'a list) (n: int): 'a list =
  match xs, n with
  | [], _ -> []
  | _ , 0 -> []
  | x :: rest, n -> x :: take_n rest (n - 1)

let slice (xs: 'a list) (i: int) (j: int): 'a list =
  take_n (drop_n xs i) (j - i + 1)

(* problem 19 *)
let rec modulo (k: int) (n: int): int =
  if k < 0       then modulo (k + n) n
  else if k >= n then modulo (k - n) n
  else k

let rec rotate (xs: 'a list) (n: int): 'a list =
  let rot1 (xs: 'a list): 'a list =
    match rev xs with
    | [] -> []
    | x :: rest -> x :: rev (rest)
  in match xs, modulo n (length xs) with
  | [], _ -> []
  | xs, 0 -> xs
  | xs, n -> rotate (rot1 xs) (n - 1)

(* problem 20 *)
let rec remove_at (n: int) (xs: 'a list): 'a list =
  match xs, n with
  | [], _ -> []
  | x :: rest, 0 -> rest
  | x :: rest, n -> x :: remove_at (n - 1) rest

(* problem 21 *)
let rec insert_at (elem: 'a) (n: int) (xs: 'a list): 'a list =
  match xs, n with
  | [], _ -> [elem]
  | xs, 0 -> elem :: xs
  | x :: rest, n -> x :: insert_at elem (n - 1) rest

(* problem 22 *)
let identity (a: 'a): 'a = a

let range (i: int) (j: int): int list =
  let rec aux (i: int) (j: int) (acc: int list): int list =
    if i < j then aux (i + 1) j (i :: acc)
    else i :: acc
  in if i < j then rev (aux i j []) else aux j i []

(* problem 23 *)
let rec extract_nth (xs: 'a list) (i: int): ('a * 'a list) =
  match xs, i with
  | [], _ -> raise Not_found
  | x :: rest, 0 -> x, rest
  | x :: rest, i -> let y, ys = extract_nth rest (i - 1) in y, (x :: ys)

let extract_rand (xs: 'a list): ('a * 'a list) =
  let i = Random.int (length xs) in
  extract_nth xs i

let rec extract_rands (xs: 'a list) (n: int): 'a list =
  match xs, n with
  | [], _ -> []
  | _, 0  -> []
  | xs, n -> let y, ys = extract_rand xs
    in y :: (extract_rands ys (n - 1))

(* problem 24 *)
let lotto_select (n: int) (i: int): int list =
  extract_rands (range 1 i) n

(* problem 25 *)
let permutation (xs: 'a list): 'a list =
  extract_rands xs (length xs)

(* problem 26 *)
let rec add_to_each (y: 'a) (xs: 'a list list): 'a list list =
  match xs with
  | [] -> []
  | x :: rest -> (y :: x) :: (add_to_each y rest)

let rec extract (n: int) (xs: 'a list): 'a list list =
  if n == 0 then [[]] else (match xs with
  | [] -> []
  | x :: rest -> (extract n rest) @ add_to_each x (extract (n - 1) rest))

(* problem 27 *)

let () =
  print_endline "Hello, World!"