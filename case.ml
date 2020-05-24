open Base
open Re2

let camel_case s =
  replace_exn ~f:(fun _ -> " ") (create_exn "[ \n\r\x0c\t]+") s
  |> String.split ~on:' '
  |> List.mapi ~f:(fun i -> fun x -> if i > 0 then String.capitalize x else String.uncapitalize x)
  |> String.concat ~sep:""

(* let kebab_case s = s *)