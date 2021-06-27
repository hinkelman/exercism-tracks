open Base;;

let count_consecutive_char (xs : 'char list) =
    let rec aux count acc = function
        | [] -> []
        | [x] -> (count + 1, x) :: acc 
        | a :: (b :: _ as tl) ->
            if equal_char a b then aux (count + 1) acc tl
            else aux 0 ((count + 1, a) :: acc) tl in
    List.rev (aux 0 [] xs)

let encode long_form =
    long_form |>
    String.to_list |> 
    count_consecutive_char |>
    List.map ~f:(fun tup -> 
        let count = fst tup in
        let int_str = if count = 1 then "" else Int.to_string count in
        int_str ^ Char.to_string (snd tup)) |>
    String.concat

let rep_char n (x : char) =
    let rec aux n acc = if n = 0 then acc else aux (n - 1) (x :: acc) in 
    aux n [] |>
    List.map ~f:Char.to_string |>
    String.concat

let is_digit digit =
    match digit with
    | '0' .. '9' -> true
    | _ -> false

let char_list_to_int xs =
    xs |>
    List.map ~f:Char.to_string |>
    String.concat |>
    Int.of_string

let parse_str_char xs =
    let rec aux int_char_list acc = function
    | [] -> acc
    | hd::tl -> 
        if is_digit hd then aux (hd :: int_char_list) acc tl else 
            match int_char_list with
            | [] -> aux int_char_list ((1, hd) :: acc) tl
            | _ -> aux [] ((char_list_to_int (List.rev int_char_list), hd) :: acc) tl in
    List.rev (aux [] [] xs)

let decode short_form =
    short_form |>
    String.to_list |>
    parse_str_char |>
    List.map ~f:(fun tup -> rep_char (fst tup) (snd tup)) |>
    String.concat
