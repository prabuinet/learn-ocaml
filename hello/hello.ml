open Base
open Stdio

let inc x = x + 1 

let () = 
  printf "%d\n" (inc 10)

let rec fact x = 
  if x <= 0 then 1 
  else x * fact(x - 1)

let () = 
  printf "%d\n" (fact 2)

(* function without argument *)
let print_it () = printf "hello world!\n"
let () = print_it ()

let () = 
  let x = 10 in
  let y = 20 in
  printf "%d\n" (x + y)

let rec pow (x: int) (y: int): int = 
  if y = 0 then x
  else x * pow x (y - 1)

let () = 
  printf "%d\n" (pow 2 5)

let sum_if_true test first second =
  (if test first then first else 0) +
    (if test second then second else 0);;

sum_if_true (fun x -> x > 1) 10 3;;


let even x = x % 2 = 0;;
sum_if_true even 2 4;;

let distance (x1,y1) (x2,y2) =
  let open Float.O in 
  Float.sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0);;

distance (3.,0.) (0.,4.);;

let square x = x * x
let result = square 10

let rec fact x =
  if x <= 0 then 1
  else x * fact(x - 1)

let first_lang languages = 
  match languages with
  | first :: rest -> first
  | [] -> "Ocaml"
  

let rec sum lst = 
  match lst with 
  | [] -> 0
  | first :: rest -> first + sum rest


let rec remove_seq_dup lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | first :: second :: rest -> 
    if first = second then
      remove_seq_dup (second :: rest)
    else
      first :: remove_seq_dup(second :: rest);;

let divide x y =
  if y = 0 then None else Some (x / y);;

let downcase_extension filename =
  match String.rsplit2 filename ~on:'.' with
  | None -> filename
  | Some (base, ext) -> 
    base ^ "." ^ String.lowercase ext;;

type point2d = { x: float; y: float };;
type joint2d = { x: float; y: float };;

let magniture { x = x_pos; y = y_pos; } = 
  Float.sqrt(x_pos **. 2. + y_pos **. 2.);;

type circle = { center: point2d; radius: float };;
type rect = {lower_left: point2d; width: float; height: float };;
type segment_desc = {endpoint1: point2d; endpoint2: point2d };;

type scene_element = 
  | Circle of circle
  | Rect   of rect
  | Segment of segment_desc;;


let arr = [| 1; 2; 3; 4; |];;

type running_sum = 
  {
    mutable sum: float;
    mutable sum_sq: float;
    mutable samples: int;
  };;

let mean rsum = rsum.sum /. Float.of_int rsum.samples;;

let stdev rsum = 
  Float.sqrt (rsum.sum_sq /. Float.of_int rsum.samples -. mean rsum **. 2.);;

let create () = {sum = 0.; sum_sq = 0.; samples = 0};;
let update rsum x = 
  rsum.samples <- rsum.samples + 1;
  rsum.sum <- rsum.sum +. x;
  rsum.sum_sq <- rsum.sum_sq +. x *. x;;

let rsum = create ();;
List.iter [1.;3.;2.;-7.;4.;5.] ~f:(fun x -> update rsum x);;



