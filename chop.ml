open Base

let first ?(len=1) s =
  String.to_list s
  |> List.filter_mapi ~f:(fun i -> fun x -> if i < len then Some (String.of_char x) else None)
  |> String.concat

let last ?len s =
  String.rev s
  |> first ?len
  |> String.rev

let prune ?(end_="...") len s =
  let retain_len = len - (String.length end_) in
  let sl = String.split ~on:' ' s in
  let rec build c = function
    | [] -> []
    | x :: xs -> let l = c + (String.length x) in
      if l <= retain_len then x :: (build (l + 1) xs) else []
  in
  let remaining = String.concat ~sep:" " (build 0 sl) in
  if String.length remaining = 0 then "" else remaining ^ end_

let slice ?end_ ~start s =
  let end_ = match end_ with
  | None -> String.length s
  | Some x -> x
  in
  String.to_list s
  |> List.filteri ~f:(fun i -> fun _ -> i >= start && i < end_)
  |> String.of_char_list
