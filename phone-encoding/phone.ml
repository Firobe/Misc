open Base

(** Retrieve the encoding of a digit *)
let digit_of_letter c =
  match Char.lowercase c with
  | 'e'             -> '0'
  | 'j' | 'n' | 'q' -> '1'
  | 'r' | 'w' | 'x' -> '2'
  | 'd' | 's' | 'y' -> '3'
  | 'f' | 't'       -> '4'
  | 'a' | 'm'       -> '5'
  | 'c' | 'i' | 'v' -> '6'
  | 'b' | 'k' | 'u' -> '7'
  | 'l' | 'o' | 'p' -> '8'
  | 'g' | 'h' | 'z' -> '9'
  | _ -> Printf.failwithf "This is not a letter: '%c'\n" c ()

(** Populate a hash table with words indexed by their encodings *)
let hash_dict path =
  let hash = Hashtbl.create (module String) in
  List.iter ~f:(fun data ->
      let word = String.filter ~f:Char.is_alpha data in
      let key = String.map ~f:digit_of_letter word in
      Hashtbl.add_multi hash ~key ~data
    ) (Stdio.In_channel.read_lines path) ;
  hash

(** Print all the possible encodings of the given number *)
let solve dict phone_raw =
  (* Keep only digits *)
  let phone = String.filter ~f:Char.is_digit phone_raw in
  let len = String.length phone in

  (* Print all encodings starting at [start] *)
  let rec aux ?(last_failed=false) ?(prefix="") start =
    let append x = prefix ^ " " ^ x in
    if start >= len then Stdio.printf "%s:%s\n%!" phone_raw prefix
    else
      let found_one = ref false in
      (* Check all substrings from [start] to the end *)
      for delta = 1 to len - start do
        let sub = String.sub phone ~pos:start ~len:delta in
        (* If some dictionary words match, add them and start from there *)
        match Hashtbl.find dict sub with
        | None -> ()
        | Some candidates ->
          found_one := true;
          (* TODO: multiple visits unecessary,
           * just visit once and pass a list of prefixes
           * Benchmark if this improves anything *)
          List.iter ~f:(fun word ->
              aux ~prefix:(append word) (start + delta)
            ) candidates
      done;
      (* If no possible word to continue, print digit once *)
      if not !found_one && not last_failed then
        aux ~last_failed:true
          ~prefix:(append (String.of_char phone.[start])) (start + 1)
  in aux 0

let main =
  let dict = hash_dict "dictionary.txt" in
  Stdio.In_channel.read_lines "input.txt" |> List.iter ~f:(solve dict)
