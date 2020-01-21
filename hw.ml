open Base
open Stdio

let ratio x y =
  Float.of_int x /. Float.of_int y

let even x =
  x % 2 = 0

let sum_if_true (test : int -> bool) (x : int) (y : int) : int =
  (if test x then x else 0) +
    (if test y then y else 0)


let first_if_true test x y =
  if test x
  then x
  else y


let long_string s =
  String.length s > 6

let is_a_multiple x y =
  x % y = 0

let languages = ["OCaml"; "C"; "Python"]

let my_favourite_language = function
  | first :: _ -> first
  | []            -> "OCaml"

let rec sum = function
  | [] -> 0
  | hd :: tl -> hd + sum tl


let rec remove_sequential_duplicates = function
  | []                   -> []
  | (_ :: []) as l       -> l
  | x :: ((y :: _) as l) ->
     if x = y then remove_sequential_duplicates l
     else x :: remove_sequential_duplicates l


let divide x y =
  if y = 0 then None
  else Some (x / y)


let downcase_extension file =
  match String.rsplit2 ~on:'.' file with
  | None -> file
  | Some (base, ext) -> base ^ "." ^ String.lowercase ext


type point2d = {
    x : float;
    y : float;
  }

type circle_desc = {
    centre : point2d;
    radius : float;
  }

type rect_desc = {
    lower_left : point2d;
    width      : float;
    height     : float;
  }

type segment_desc = {
    endpoint_1 : point2d;
    endpoint_2 : point2d;
}

type scene_element =
  | Circle  of circle_desc
  | Rect    of rect_desc
  | Segment of segment_desc


let magnitude {x = x_pos; y = y_pos } =
  Float.sqrt (x_pos **. 2. +. y_pos **. 2.)

let distance v1 v2 =
  let x = v1.x -. v2.x in
  let y = v1.y -. v2.y in
  magnitude {x; y}


let is_inside_scene_element point scene_element =
  let open Float.O in
  match scene_element with
  | Circle { centre; radius } ->
     (distance centre point < radius)
  | Rect   { lower_left; width; height } ->
     true
     && (lower_left.x <= point.x)
     && (point.x <= lower_left.x + width)
     && (lower_left.y <= point.y)
     && (point.y <= lower_left.y + height)
  | Segment {endpoint_1; endpoint_2} ->
     let total = distance endpoint_1 endpoint_2 in
     let first_half = distance endpoint_1 point in
     let second_half = distance endpoint_2 point in
     total = (first_half + second_half)


let is_inside_scene point scene =
  List.exists ~f:(is_inside_scene_element point) scene


type running_sum =
  {
    mutable sum     : float;
    mutable sum_sq  : float;
    mutable samples : int;
  }


let mean rsum = rsum.sum /. Float.of_int rsum.samples

let stdev rsum =
  let open Float.O in
  Float.sqrt ((rsum.sum_sq / of_int rsum.samples)
              - (rsum.sum / of_int rsum.samples) ** 2.)

let create () =
  {
    sum     = 0.;
    sum_sq  = 0.;
    samples = 0;
  }

let update rsum x =
  begin
    rsum.sum     <- rsum.sum +. x;
    rsum.sum_sq  <- rsum.sum_sq +. x **. 2.;
    rsum.samples <- rsum.samples + 1;
  end

let shuffle array =
  let len = Array.length array in
  for i = 1 to (len - 1) do
    let j = Random.int (i + 1) in
    Array.swap array i j
  done


let partition array x =
  let rec partition_aux lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if x < array.(mid) then partition_aux lo (mid - 1)
      else if x > array.(mid) then partition_aux (mid + 1) hi
      else
        (* We found at least one x in array ... may be there are more *)
        (* and array is sorted in ascending order *)
        partition_aux lo mid
  in
  let lo = 0 in
  let hi = (Array.length array - 1) in
  if x > array.(hi) then (hi + 1) (* client has to handle this *)
  else if x < array.(lo) then lo
  else partition_aux lo hi


let () =
  let numbers = Array.init 20 ~f:(fun i -> i) in
  begin
    Random.self_init ();
    shuffle numbers;
    Array.iter numbers ~f:(printf "%d ");
    printf "\n";
    for _ = 10 to 9 do
      printf "this shouldn't be printed\n"
    done
  end
