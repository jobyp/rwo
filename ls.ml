open Base
open Stdio

module Sys = Core.Sys
module Filename = Core.Filename


let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.filter ~f:(fun name -> String.compare ".git" name <> 0)
    |> List.map ~f:(fun sub -> ls_rec (Filename.concat s sub))
    |> List.concat



let () =
  ls_rec "/home/pcj/try/real_world_ocaml"
  |> List.iter ~f:(printf "%s\n")


