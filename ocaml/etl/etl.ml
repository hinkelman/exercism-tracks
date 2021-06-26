let transform old_data =
    List.concat_map 
    (fun tup -> 
        let pts = fst tup in
        List.map (fun ltr -> (Char.lowercase_ascii ltr, pts)) (snd tup))
    old_data |>
    List.sort_uniq compare
