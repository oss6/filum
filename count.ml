open Base

let count_substrings s ~substring =
  let rec _inner s acc =
    if String.is_empty s
    then acc
    else _inner (Chop.first s) (acc + (if String.is_prefix s ~prefix:substring then 1 else 0))
  in
  _inner s 0
