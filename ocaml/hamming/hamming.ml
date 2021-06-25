open Base

type nucleotide = A | C | G | T

let equal_nucleotide (a: nucleotide) (b: nucleotide) =
  match (a, b) with
  | (A, A) | (C, C) | (G, G) | (T, T) -> true
  | (_, _) -> false

let rec ham_help a_lst b_lst dist =
  match a_lst, b_lst with
  | [], [] | _::_, [] | [], _::_ -> dist
  | a_hd::a_tl, b_hd::b_tl -> 
    let inc = if equal_nucleotide a_hd b_hd then 0 else 1 in 
    ham_help a_tl b_tl (dist + inc)

let hamming_distance (strand_a : 'nucleotide list) (strand_b : 'nucleotide list) =
  match strand_a, strand_b with
  | [], [] -> Ok (ham_help strand_a strand_b 0)
  | [], _ -> Error "left strand must not be empty"
  | _, [] -> Error "right strand must not be empty"
  | a, b -> if List.length a = List.length b then Ok (ham_help strand_a strand_b 0) 
  else Error "left and right strands must be of equal length"
