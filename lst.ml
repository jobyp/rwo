open Base
open Stdio


let column_widths header rows =
  let columns l = List.map ~f:String.length l in
  let widths l1 l2 = List.map2_exn ~f:Int.max l1 l2 in
  List.fold_left
    ~init:(columns header)
    ~f:(fun acc row -> widths acc (columns row))
    rows


let add_padding row widths =
  let padd word len =
    " " ^ word ^ (String.make (len + 1 - String.length word) ' ')
  in
  List.map2_exn ~f:padd row widths


let render row widths =
  "|" ^ (String.concat ~sep:"|" (add_padding row widths)) ^ "|"


let seperator widths =
  let dashes = List.map ~f:(fun len -> String.make (len + 2) '-') widths in
  "|" ^ (String.concat ~sep:"+" dashes) ^ "|"

let render_table header rows =
  let widths = column_widths header rows in
  begin
    printf "%s\n" (render header widths);
    printf "%s\n" (seperator widths);
    List.iter ~f:(fun row -> printf "%s\n" (render row widths)) rows
  end

let header = ["language";"architect";"first release"]

let rows = [["Lisp" ;"John McCarthy" ;"1958"];
            ["C"    ;"Dennis Ritchie";"1969"];
            ["ML"   ;"Robin Milner"  ;"1973"];
            ["OCaml";"Xavier Leroy"  ;"1996"];]

let even n =
  List.filter ~f:(fun e -> e % 2 = 0) (List.range 0 (2 * n - 1))


let file_extentions filenames =
  List.filter_map filenames
    ~f:(fun file ->
      match String.rsplit2 ~on:'.' file with
      | None | Some ("", _) -> None
      | Some (_, ext) -> Some ext)
  |> List.dedup_and_sort ~compare:String.compare

let is_ocaml_source s =
  match String.rsplit2 ~on:'.' s with
  | Some ("", _)                      -> false
  | Some (_, ".ml") | Some (_, "mli") -> true
  | _                                 -> false


let (ml_files, other_files) =
  List.partition_tf ~f:is_ocaml_source
    ["foo.c"; "foo.ml"; "bar.ml"; "bar.mli"]

let () =
  render_table header rows
